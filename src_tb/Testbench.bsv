// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package Testbench;

// ================================================================
// BSV library imports

import Connectable :: *;
import FIFOF :: *;
import GetPut :: *;

// ----------------
// Other lib imports


// ================================================================
// Project imports

import APB_Types        :: *;
import APB_Defs         :: *;

import APB_Adapter :: *;
import APB_Mem_Model   :: *;
import MMU_Cache_Common :: *;
import Cur_Cycle :: *;

// ================================================================

Bit #(0) default_user = ?;
typedef enum { WRITE_32, READ_32, WRITE_16, READ_16, WRITE_8, READ_8, FINISH } TB_State deriving (Bits, Eq, FShow);

// ================================================================
// This TB is for verifying the TCM APBL adapter directly with a APB-L memory model
(* synthesize *)
module mkTestbench (Empty);
   APB_Initiator_IFC source <- mkSource;
   APB_Target_IFC  mem_model <- mkAPB_Mem_Model;

   mkConnection (source, mem_model);

   rule rl_dummy_APB_decoder_and_mux;
      mem_model.psel (True);
   endrule
endmodule

(* synthesize *)
module mkSource (APB_Initiator_IFC);

   // Requests and data to/from memory (AHB-L fabric)
   FIFOF #(Single_Req)        f_mem_req         <- mkGFIFOF1 (False, True);
   FIFOF #(Bit #(32))         f_mem_wdata       <- mkGFIFOF1 (False, True);
   FIFOF #(Read_Data)         f_mem_rdata       <- mkFIFOF1;

   Bit #(2) verbosity = 2;
   APB_Adapter_IFC apb_adapter<- mkAPB_Adapter (
      verbosity, f_mem_req, f_mem_wdata, f_mem_rdata);

   // ----------------

   Reg #(Bit #(32)) rg_addr <- mkReg (0);
   Reg #(TB_State) rg_state <- mkReg (WRITE_32);

   rule rl_wr_32 ((rg_state == WRITE_32));
      let wra = Single_Req {addr: rg_addr,
			    is_read: False,
                            size_code: 2'b10};

      Bit #(32) wrd = rg_addr;

      f_mem_req.enq (wra);
      f_mem_wdata.enq (wrd);

      if (rg_addr == 32'hfc) begin
         rg_addr <= 0;
         rg_state <= READ_32;
      end
      else rg_addr <= rg_addr + 4;

      $display ("%0d: %m.rl_wr_32: ", cur_cycle, fshow (wra), " 0x%08h", wrd);
   endrule

   rule rl_rd_32 ((rg_state == READ_32));
      let rda = Single_Req {addr: rg_addr,
			    is_read: True,
			    size_code: 2'b10};
      f_mem_req.enq (rda);
      if (rg_addr == 32'hfc) begin
         rg_addr <= 0;
         rg_state <= WRITE_16;
      end

      else rg_addr <= rg_addr + 4;
      $display ("%0d: %m.rl_rd_32: ", cur_cycle, fshow (rda));
   endrule

   rule rl_rd_rsp_32 ((rg_state == READ_32) || (rg_state == WRITE_16));
      let rdd = f_mem_rdata.first; f_mem_rdata.deq;
      Bit #(32) expected = 0;

      if (rg_state == READ_32) expected = (rg_addr - 4);
      // Reads finished. The last adress written is 0x3fc
      else                     expected = 32'hfc;

      if (rdd.data != expected) begin
         $display ("%0d: %m.rl_rd_rsp_32: ERROR rdata mismath", cur_cycle);
         $display ("   (expected %08h) (actual %08h)", expected, rdd.data);
      end
      else $display ("%0d: %m.rl_rd_rsp_32: ", cur_cycle, fshow (rdd));
   endrule

   rule rl_wr_16 ((rg_state == WRITE_16));
      let wra = Single_Req {addr: rg_addr,
			    is_read: False,
                            size_code: 2'b01};

      Bit #(16) wrd = truncate (rg_addr);
      Bit #(32) wdata = (rg_addr[1] == 1'b1) ? {wrd, 16'h0} : {16'h0, wrd};

      f_mem_req.enq (wra);
      f_mem_wdata.enq (wdata);

      if (rg_addr == 32'hfe) begin
         rg_addr <= 0;
         rg_state <= READ_16;
      end
      else rg_addr <= rg_addr + 2;

      $display ("%0d: %m.rl_wr_16: ", cur_cycle, fshow (wra), " 0x%08h", wrd);
   endrule

   rule rl_rd_16 ((rg_state == READ_16));
      let rda = Single_Req {addr: rg_addr,
			    is_read: True,
			    size_code: 2'b01};
      f_mem_req.enq (rda);
      if (rg_addr == 32'hfe) begin
         rg_addr <= 0;
         rg_state <= WRITE_8;
      end

      else rg_addr <= rg_addr + 2;
      $display ("%0d: %m.rl_rd_16: ", cur_cycle, fshow (rda));
   endrule

   rule rl_rd_rsp_16 ((rg_state == READ_16) || (rg_state == WRITE_8));
      let rdd = f_mem_rdata.first; f_mem_rdata.deq;
      Bit #(16) expected = 0;
      Bit #(32) last_addr = 0;
      Bit #(16) actual = truncate (rdd.data);

      if (rg_state == READ_16) begin
         expected = truncate (rg_addr - 2);
         last_addr= (rg_addr - 2);
      end
      // Reads finished. The last adress written is 0x3fc
      else begin
         expected = 16'hfe;
         last_addr = 32'hfe;
      end

      if (last_addr[1] == 1'b1) actual = rdd.data[31:16];

      if (actual != expected) begin
         $display ("%0d: %m.rl_rd_rsp_16: ERROR rdata mismath", cur_cycle);
         $display ("   (expected %08h) (actual %08h)", expected, actual);
      end
      else $display ("%0d: %m.rl_rd_rsp_16: ", cur_cycle, fshow (rdd));
   endrule

   rule rl_wr_8 ((rg_state == WRITE_8));
      let wra = Single_Req {addr: rg_addr,
			    is_read: False,
                            size_code: 2'b00};

      Bit #(8) wrd = truncate (rg_addr);
      Bit #(32) wdata = case (rg_addr[1:0])
                           2'b00 : {24'h0, wrd};
                           2'b01 : {16'h0, wrd, 8'h0};
                           2'b10 : {8'h0, wrd, 16'h0};
                           2'b11 : {wrd, 24'h0};
                        endcase;

      f_mem_req.enq (wra);
      f_mem_wdata.enq (wdata);

      if (rg_addr == 32'hff) begin
         rg_addr <= 0;
         rg_state <= READ_8;
      end
      else rg_addr <= rg_addr + 1;

      $display ("%0d: %m.rl_wr_8: ", cur_cycle, fshow (wra), " 0x%08h", wrd);
   endrule

   rule rl_rd_8 ((rg_state == READ_8));
      let rda = Single_Req {addr: rg_addr,
			    is_read: True,
			    size_code: 2'b00};
      f_mem_req.enq (rda);
      if (rg_addr == 32'hff) begin
         rg_addr <= 0;
         rg_state <= FINISH;
      end

      else rg_addr <= rg_addr + 1;
      $display ("%0d: %m.rl_rd_8: ", cur_cycle, fshow (rda));
   endrule

   rule rl_rd_rsp_8 ((rg_state == READ_8) || (rg_state == FINISH));
      let rdd = f_mem_rdata.first; f_mem_rdata.deq;
      Bit #(8) expected = 0;
      Bit #(32) last_addr = 0;
      Bit #(8) actual = truncate (rdd.data);

      if (rg_state == READ_8) begin
         expected = truncate (rg_addr - 1);
         last_addr= (rg_addr - 1);
      end
      // Reads finished. The last adress written is 0xff
      else begin
         expected = 8'hff;
         last_addr = 32'hff;
      end

      actual = case (last_addr [1:0])
                  2'b00 : rdd.data[7:0];
                  2'b01 : rdd.data[15:8];
                  2'b10 : rdd.data[23:16];
                  2'b11 : rdd.data[31:24];
               endcase;

      if (actual != expected) begin
         $display ("%0d: %m.rl_rd_rsp_8: ERROR rdata mismath", cur_cycle);
         $display ("   (expected %08h) (actual %08h)", expected, actual);
      end
      else $display ("%0d: %m.rl_rd_rsp_8: ", cur_cycle, fshow (rdd));

      if (rg_state == FINISH) $finish;
   endrule
   return (apb_adapter.mem_master);
endmodule

// ================================================================

endpackage
