// Copyright (c) 2021 Bluespec, Inc.  All Rights Reserved

package AXI4_APB_Adapter;

// ================================================================
// Adapter converting AXI4 32b-wide read/write requests from a
// client into signaling as an APB bus master.
//
// This adapter implements v2.0 of the AMBA APB specification as
// per the document: ARM IHI 0034C ID042910
//
// 'Client' upstream:
// - AXI4 master device: requests/responses are for 8/16/32-bit
//   reads and writes and where lane-alignment is already done.
//
// Expectations between client and adapter:
// - Write data from Client aligned to appropriate bytelane in 32-
//   bit word. APB adapter will derive appropriate write strobes
//   from size code.
//
// - Read response from APB Adapter will have the desired word or
//   sub-word aligned to the appropriate bytelane. All reads issued
//   on the APB will be for 32-bits and with addresses that are
//   32-bit aligned.
//
// Expectations from an APB target:
// - The target must respect the byte strobe information for writes.
// - If the target has locations that are byte addressable, since
//   the requests will always be 32-bit (bus-width) aligned, the
//   target may have to read multiple locations to service a
//   read request and place the bytes in the appropriate byte lanes.
// 
// NOTES:
// =====
// This implementation only supports accesses upto 32-bits for
// reads or writes. Only supports FABRIC32 on AXI4.
//
// The PPROT signal is not implemented and is always tied to 010.
//
// The STANDALONE macro should be enabled when the Adapter is to be
// instantiated in its own hierarchy or for standalone verification
//
// After the response phase (ACCESS state), the FSM always cycles
// back to IDLE before handling the next request.
//
// ================================================================
// TODO:
//
// ================================================================
// Macros:
// 
// STANDALONE: Allows separate compilation with CPU-side interfaces
//
// ================================================================
//
// BSV lib imports

import DefaultValue        :: *;
import FIFOF               :: *;
import GetPut              :: *;

// ----------------
// Other lib imports

// ================================================================
// Project imports

import APB_Types           :: *;
import APB_Defs            :: *;
import AXI4_Types          :: *;
import Fabric_Defs         :: *;
import Cur_Cycle           :: *;
import Semi_FIFOF          :: *;

// ================================================================
// Local state enumeration

typedef enum { IDLE, SETUP, ACCESS } APB_State
deriving (Bits, Eq, FShow);

// Merged request structure
typedef enum { REQ_OP_RD, REQ_OP_WR } Req_Op
deriving (Bits, Eq, FShow);

typedef struct {
   Req_Op                     req_op;

   // AW and AR channel info
   Fabric_Id                  id;
   Fabric_Addr                addr;
   AXI4_Len                   len;
   AXI4_Size                  size;
   AXI4_Burst                 burst;
   AXI4_Lock                  lock;
   AXI4_Cache                 cache;
   AXI4_Prot                  prot;
   AXI4_QoS                   qos;
   AXI4_Region                region;
   Bit #(Wd_User)             user;
   
   // Write data info
   Bit #(TDiv #(Wd_Data, 8))  wstrb;
   Fabric_Data                data;
} Req deriving (Bits, FShow);

// ================================================================
// MODULE INTERFACE

interface AXI4_APB_Adapter_IFC;
   // Reset
   method Action  reset;

   // ----------------
   // Master interface driving the adapter
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi4_slave;

   // ----------------
   // Fabric interface
   interface APB_Initiator_IFC mem_master;

endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Address converters

// Convert a XLEN Address to an APB Fabric Address 
function APB_Fabric_Addr fv_AXI4_Addr_to_APB_Addr (Fabric_Addr addr);
   // Get rid of the byte address 
   Fabric_Addr word_addr = ((addr >> 2) << 2);
   return (word_addr);
endfunction

// Adjust incoming write data based on FABRIC32. As both AXI4 data and fabric data
// is of the same size in this adapter, no adjustments needed.
function APB_Fabric_Data fv_AXI4_Data_to_APB_Data (Fabric_Data data);
   return (data);
endfunction

function Fabric_Data fv_APB_Data_to_AXI4_Data (APB_Fabric_Data data);
   return (data);
endfunction

// ================================================================
// MODULE IMPLEMENTATION
`ifdef STANDALONE
(* synthesize *)
`endif
module mk_AXI4_APB_Adapter #(
     parameter Bit #(2) verbosity   // 0=quiet, 1=rule firings
) (AXI4_APB_Adapter_IFC);

   // Connector to AXI4 fabric
   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   // Requests merged from the (WrA, WrD) and RdA channels
   FIFOF #(Req) f_merged_reqs <- mkGFIFOF (False   /* guarded enq */
                                         , True);  /* unguarded deq */

   // Input signals from APB bus
   Wire #(APB_Fabric_Data)    wi_prdata         <- mkBypassWire;
   Wire #(Bool)               wi_pready         <- mkBypassWire;
   Wire #(Bool)               wi_pslverr        <- mkBypassWire;

   // Master state
   Reg #(APB_State)           rg_state          <- mkReg (IDLE);

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Merge AXI4 requests into a single queue. This merger is necessary
   // since writes are posted by the core and while a write is being
   // processed a write and a read may queue up for processing requiring
   // serialization.
   // Prioritizing writes over reads because a read request will block
   // the CPU pipeline and not allow a following write to read the Xactor.
   // However a write will not block the CPU and may result in a subsequent
   // read to be also waiting in the Xactor. In this case the write should
   // be processed before the read.

   rule rl_merge_rd_req;
      let rda <- pop_o (slave_xactor.o_rd_addr);
      let req = Req {req_op:     REQ_OP_RD,
		     id:         rda.arid,
		     addr:       rda.araddr,
		     len:        rda.arlen,
		     size:       rda.arsize,
		     burst:      rda.arburst,
		     lock:       rda.arlock,
		     cache:      rda.arcache,
		     prot:       rda.arprot,
		     qos:        rda.arqos,
		     region:     rda.arregion,
		     user:       rda.aruser,
		     wstrb:      0,
		     data:       ?};
      f_merged_reqs.enq (req);

      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_merge_rd_req", cur_cycle);
	 $display ("        ", fshow (rda));
      end
   endrule

   (* descending_urgency = "rl_merge_wr_req, rl_merge_rd_req" *)
   rule rl_merge_wr_req;
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);
      let req = Req {req_op:     REQ_OP_WR,
		     id:         wra.awid,
		     addr:       wra.awaddr,
		     len:        wra.awlen,
		     size:       wra.awsize,
		     burst:      wra.awburst,
		     lock:       wra.awlock,
		     cache:      wra.awcache,
		     prot:       wra.awprot,
		     qos:        wra.awqos,
		     region:     wra.awregion,
		     user:       wra.awuser,
		     wstrb:      wrd.wstrb,
		     data:       wrd.wdata};
      f_merged_reqs.enq (req);

      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_merge_wr_req", cur_cycle);
	 $display ("        ", fshow (wra));
	 $display ("        ", fshow (wrd));
      end
   endrule

   // --------
   // Incoming transfer request
   let req = f_merged_reqs.first;

   // A read request is waiting
   Bool read_request = (req.req_op == REQ_OP_RD) && f_merged_reqs.notEmpty;

   // A write request is waiting. For writes, we need wr_addr and wr_data
   // available as we compute HSIZE from wr_data.wstrb
   Bool write_request = (req.req_op == REQ_OP_WR) && f_merged_reqs.notEmpty;

   Bool new_request = f_merged_reqs.notEmpty;

   // Continuous signals driven straight from input FIFOs
   let paddr   = fv_AXI4_Addr_to_APB_Addr (req.addr);
   let pwdata  = fv_AXI4_Data_to_APB_Data (req.data);
   let pstrb   = req.wstrb;    // copy the strobe from AXI4 (zeroed for reads)
   let pwrite  = write_request;

   // Continuous output based on FSM state
   let penable = (rg_state == ACCESS);
   let psel    = (rg_state != IDLE);

   // --------
   // IDLE state: Entry point for a new request. Setup addr phase signals.
   rule rl_new_tfr (new_request && (rg_state == IDLE));
      rg_state <= SETUP;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_new_tfr (paddr 0x%08h) ", cur_cycle, paddr
                 , "(pwrite: ", fshow (pwrite), ") "
                 , "(pstrb: %04b) ", pstrb
                 , "(pwdata 0x%08h) ", pwdata);
      end
   endrule


   // --------
   // SETUP state: A single cycle to allow address decode to settle on the APB bus
   rule rl_setup (rg_state == SETUP);
      rg_state <= ACCESS;

      if (verbosity > 0) begin
         if (write_request)
            $display ("%0d: %m.rl_setup: W: (addr 0x%08h) (wdata 0x%08h)"
               , cur_cycle, paddr, pwdata);
         else
            $display ("%0d: %m.rl_setup: R: (addr 0x%08h) "
               , cur_cycle, paddr);
      end
   endrule


   // --------
   // ACCESS state: Accept read responses. Advance request queue.
   // Forward response to CPU.
   rule rl_read_response (wi_pready && read_request && (rg_state == ACCESS));
      rg_state  <= IDLE;

      // Response handling and packing
      Fabric_Data data = fv_APB_Data_to_AXI4_Data (wi_prdata);
      AXI4_Resp  rresp = wi_pslverr ? axi4_resp_slverr : axi4_resp_okay;

      let rdr = AXI4_Rd_Data {rid:   req.id,
			      rdata: wi_prdata,
			      rresp: rresp,
			      rlast: True,
			      ruser: req.user};
      slave_xactor.i_rd_data.enq (rdr);

      // Advance request queue
      f_merged_reqs.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_read_response: ", cur_cycle, fshow (rdr));
      end
   endrule


   // --------
   // ACCESS state: Accept write response. Advance request queue.
   rule rl_write_response (wi_pready && write_request && (rg_state == ACCESS));
      rg_state  <= IDLE;

      // Response handling
      AXI4_Resp  bresp = wi_pslverr ? axi4_resp_slverr : axi4_resp_okay;
      let wrr = AXI4_Wr_Resp {bid:   req.id,
			      bresp: bresp,
			      buser: req.user};
      slave_xactor.i_wr_resp.enq (wrr);

      // Advance request queue
      f_merged_reqs.deq;

      if (verbosity > 0) begin
         $display ("%0d: %m.rl_write_response: ", cur_cycle, fshow (wrr));
      end
   endrule


   // ================================================================
   // INTERFACE
   method Action reset;
      f_merged_reqs.clear;
      slave_xactor.reset;
      rg_state <= IDLE;
      if (verbosity > 1) $display ("%0d: %m.reset", cur_cycle);
   endmethod

   interface  axi4_slave = slave_xactor.axi_side;

   // ----------------
   // APB side
   interface APB_Initiator_IFC mem_master;
      method paddr     = paddr;
      method pwdata    = pwdata;
      method pwrite    = pwrite;
      method pstrb     = pstrb;
      method pprot     = 3'b010;
      method penable   = penable;
      method psel      = psel;

      method Action prdata (APB_Fabric_Data data);
         wi_prdata <= data;
      endmethod

      method Action pready (Bool ready);
         wi_pready <= ready;
      endmethod

      method Action pslverr (Bool resp);
         wi_pslverr <= resp;
      endmethod
   endinterface
endmodule


// ================================================================

endpackage
