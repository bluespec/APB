// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package APB_Mem_Model;

// ================================================================
// A memory model that implements a model of memory:
// - Byte addressed
// - Address-range parameters 'addr_lo' to 'addr_hi'
// - AHB-Lite Slave interface with 32-bit data bus
// - Implements 'memory' with a RegFile of 32-bit words.
//    initialized by a mem hex file.
// - Supports 8b, 16b and 32b writes, based on AHBL HSIZE
//    Sub-word data is taken from proper byte lanes.
//    AHBL HADDR [1:0] and HSIZE are used to select byte lanes.
// - Supports 32b reads only (ignores HADDR [1:0]

// Ref:
//   "AMBA 3 AHB-Lite Protocol v1.0; Specification",
//   Document IHI 0033A, ARM Ltd., 2006

// ================================================================
// TODO:
// - make hex-file init optional

// ================================================================
// BSV lib imports

import DefaultValue        :: *;
import RegFile             :: *;
import Assert              :: *;

// ================================================================
// Project imports

import APB_Defs            :: *;
import APB_Types           :: *;
import Testbench_Commons   :: *;

typedef enum { IDLE, RD_RSP, WR_RSP } Tgt_State deriving (Bits, Eq, FShow);

// ================================================================
// The module
// The memory model is a total of 256B arranged in 32-bit words, 64 
// entries (6-bit index)
//
// This model asserts the SLVERR signal for all read responses. This 
// is not indicative of a real error, but allows us to test the 
// forwarding of error response in the APB Initiator.

(* synthesize *)
module mkAPB_Mem_Model (APB_Target_IFC);

   Bit #(2) verbosity = 0;

   // ----------------
   // AHB-Lite signals and registers

   // Inputs
   Wire #(Bool)        w_psel      <- mkBypassWire;    Reg #(Bool)        rg_sel       <- mkReg (False);
   Wire #(Bit #(32))   w_paddr     <- mkBypassWire;    Reg #(Bit #(8))    rg_paddr     <- mkReg (0);
   Wire #(Bool)        w_penable   <- mkBypassWire;    Reg #(Bool)        rg_pready    <- mkReg (False);
   Wire #(Bit #(32))   w_pwdata    <- mkBypassWire;    Reg #(Bit #(32))   rg_pwdata    <- mkReg (0);
   Wire #(Bool)        w_pwrite    <- mkBypassWire;
   Wire #(Bit #(4))    w_pstrb     <- mkBypassWire;    Reg #(Bit #(4))    rg_pstrb     <- mkReg (0);
   Wire #(Bit #(3))    w_pprot     <- mkBypassWire; 

   Reg  #(Tgt_State)   rg_state    <- mkReg (IDLE);

   // ----------------
   // The Memory model
   // Addressed with 8-bit word-address; each location holds 4-byte word
   // TODO: for some reason the compile doesn't like word_addr_lo to be a module parameter.

   RegFile #(Bit #(6), Bit #(32)) rf <- mkRegFileFull;

   let word_addr    = rg_paddr [7:2];
   let mem_out      = rf.sub (word_addr);
   match { .err, .new_word } = fn_replace_bytes (rg_pstrb, mem_out, rg_pwdata);

   // Assertions
   // continuousAssert ((rg_paddr[1:0] != 0), "paddr not word-aligned!");

   // ================================================================
   // BEHAVIOR

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_new_read_tfr (w_psel && w_penable && !w_pwrite && (rg_state == IDLE));
      rg_paddr <= truncate (w_paddr);
      rg_pstrb <= w_pstrb;
      rg_pready <= True;
      rg_state <= RD_RSP;
      // dynamicAssert ((w_pstrb != 0), "Non-zero pstrb for read requests!");
      $display ("%0d: %m.rl_new_read_tfr: ", $time, fshow (rg_state));
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_read_response (rg_state == RD_RSP);
      rg_pready <= False;
      rg_state <= IDLE;
      $display ("%0d: %m.rl_read_response: (addr %02h) (data %08h) "
         , $time, rg_paddr, mem_out, fshow (rg_state));
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_new_write_tfr (w_psel && w_penable && w_pwrite && (rg_state == IDLE));
      rg_paddr <= truncate (w_paddr);
      rg_pwdata <= w_pwdata;
      rg_pstrb <= w_pstrb;
      rg_pready <= True;
      rg_state <= WR_RSP;
      // dynamicAssert ((w_pstrb == 0), "Zero pstrb for write requests!");
      $display ("%0d: %m.rl_new_write_tfr: ", $time, fshow (rg_state));
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_write_response (rg_state == WR_RSP);
      rg_pready <= False;
      rg_state <= IDLE;
      rf.upd (word_addr, new_word);
      $display ("%0d: %m.rl_write_response: (addr %02h) (data %08h) "
         , $time, rg_paddr, new_word, fshow (rg_state));
   endrule

   // ================================================================
   // INTERFACE

   // ----------------
   // Inputs

   method Action psel (Bool sel);
      w_psel <= sel;
   endmethod

   method Action penable (Bool en);
      w_penable <= en;
   endmethod

   method Action paddr (Bit #(32) addr);
      w_paddr <= addr;
   endmethod

   method Action pwdata(Bit #(32) data);
      w_pwdata <= data;
   endmethod

   method Action pprot (Bit #(3) prot);
      w_pprot <= prot;
   endmethod

   method Action pstrb (Bit #(4) strb);
      w_pstrb <= strb;
   endmethod

   method Action pwrite (Bool write);
      w_pwrite <= write;
   endmethod

   // ----------------
   // Outputs

   method Bool       pready    = rg_pready;
   method Bool       pslverr   = err;
   method Bit #(32)  prdata    = mem_out;
endmodule

// ================================================================
// APB data is aligned to byte lanes based on pstrb
// This function replaces the appropriate bytes of 'old_word'
// with the appropriate bytes of PWDATA depending on the pstrb
// Also returns err=True for unsupported 'size' and misaligned addrs.

function Tuple2 #(Bool,
		  Bit #(32)) fn_replace_bytes (Bit #(4)   strb,
					       Bit #(32)  orig,
					       Bit #(32)  data);
   let err = False;
   let new_word = orig;
   case (strb)
      4'b0001 : new_word = {orig [31:24], orig [23:16], orig [15:8], data [7:0]};
      4'b0010 : new_word = {orig [31:24], orig [23:16], data [15:8], orig [7:0]};
      4'b0100 : new_word = {orig [31:24], data [23:16], orig [15:8], orig [7:0]};
      4'b1000 : new_word = {data [31:24], orig [23:16], orig [15:8], orig [7:0]};
      4'b0011 : new_word = {orig [31:16], data [15:0]};
      4'b1100 : new_word = {data [31:16], orig [15:0]};
      4'b1111 : new_word = data;
      default : err = True;
   endcase
   return tuple2 (err, new_word);
endfunction


endpackage
