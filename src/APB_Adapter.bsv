// Copyright (c) 2021 Bluespec, Inc.  All Rights Reserved

package APB_Adapter;

// ================================================================
// Adapter converting generic 32b-wide read/write requests from a
// MMIO client into signaling as an APB bus master.
//
// This adapter implements v2.0 of the AMBA APB specification as
// per the document: ARM IHI 0034C ID042910
//
// 'Client' upstream:
// - MMIO device: requests/responses are for 8/16/32-bit reads and
//                writes and where lane-alignment is already done.
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
// reads or writes. 
//
// This module collects and discards write-responses. There is no
// write response to the client. Errors on writes are not checked
// or reported.
// 
// Read responses are passed up to the client.
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

// import Semi_FIFOF :: *;
// import EdgeFIFOFs :: *;

// ================================================================
// Project imports

import APB_Types           :: *;
import APB_Defs            :: *;

`ifdef STANDALONE
import Testbench_Commons   :: *;
`else
import ISA_Decls           :: *;
import MMU_Cache_Common    :: *; // definitions for req/rsp types
import Cur_Cycle           :: *;
`endif

// ================================================================
// Local state enumeration

typedef enum { IDLE, SETUP, ACCESS } APB_State
deriving (Bits, Eq, FShow);

// ================================================================
// MODULE INTERFACE

interface APB_Adapter_IFC;
   // Reset
   method Action  reset;

`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(32))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;
`endif

   // ----------------
   // Fabric interface
   interface APB_Initiator_IFC mem_master;

endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Address converters

`ifdef ISA_PRIV_S
// Convert a 64-bit PA to an APB Fabric Address
// Discard the upper 32 bits

function APB_Fabric_Addr fv_Addr_to_Fabric_Addr (Bit #(64) addr);
   APB_Fabric_Addr word_addr = truncate (addr);
   word_addr = ((word_addr >> 2) << 2);
   return (word_addr);
endfunction

`else

// Convert a XLEN Address to an APB Fabric Address 
function APB_Fabric_Addr fv_Addr_to_Fabric_Addr (Addr addr);
   // Get rid of the byte address 
   Addr word_addr = ((addr >> 2) << 2);
`ifdef RV32
   return (word_addr);
`endif
`ifdef RV64
   return truncate (word_addr);
`endif
endfunction

`endif   // `else ISA_PRIV_S

// Adjust incoming write data based on FABRIC32. As both NM data and fabric data
// is of the same size in this adapter, no adjustments needed.
function APB_Fabric_Data fv_get_apb_wdata (Bit #(32) wr_data);
   let wdata = wr_data;
   return (wdata);
endfunction

// Derive the strb from the size of the transfer requested by the
// client and the byte address
function APB_Fabric_Strb fv_size_to_strb (Bit #(2) byte_addr, Bit #(2) size_code);
   // Words
   APB_Fabric_Strb strb = 4'b1111;

   // bytes
   if (size_code == 2'b00) begin
      strb = case (byte_addr)
                2'b00 : 4'b0001;
                2'b01 : 4'b0010;
                2'b10 : 4'b0100;
                2'b11 : 4'b1000;
             endcase;
   end

   // half-word
   else if (size_code == 2'b01) begin
      strb = (byte_addr[1] == 1'b1) ? 4'b1100 : 4'b0011;
   end

   return (strb);
endfunction

// ================================================================
// MODULE IMPLEMENTATION
`ifdef STANDALONE
(* synthesize *)
`endif
module mkAPB_Adapter #(
     parameter Bit #(2) verbosity   // 0=quiet, 1=rule firings
`ifdef STANDALONE
) (APB_Adapter_IFC);
`else
   // The request and write data FIFOs need explicit EMPTY checking on the DEQ
   // side. This allows us to directly drive the APB signals from these FIFOs
   // removing the need for extra registers in the adapter
   , FIFOF #(Single_Req) f_single_reqs
   , FIFOF #(Bit #(32))  f_single_write_data
   , FIFOF #(Read_Data)  f_single_read_data) (APB_Adapter_IFC);
`endif

`ifdef STANDALONE
   // The request and write data FIFOs need explicit EMPTY checking on the DEQ
   // side. This allows us to directly drive the APB signals from these FIFOs
   // removing the need for extra registers in the adapter
   FIFOF #(Single_Req) f_single_reqs <- mkGFIFOF1 (False /* guarded enq */
                                                 , True  /* unguarded deq */);
   FIFOF #(Bit #(32))  f_single_write_data <- mkGFIFOF1 (False /* guarded enq */
                                                       , True  /* unguarded deq */);
   FIFOF #(Read_Data)  f_single_read_data <- mkFIFOF1;
`endif

   // Input signals from APB bus
   Wire #(APB_Fabric_Data)    wi_prdata         <- mkBypassWire;
   Wire #(Bool)               wi_pready         <- mkBypassWire;
   Wire #(Bool)               wi_pslverr        <- mkBypassWire;

   // Master state
   Reg #(APB_State)           rg_state          <- mkReg (IDLE);

   // ================================================================
   // BEHAVIOR

   // --------
   // Incoming transfer request
   let req = f_single_reqs.first;

   // A read request is waiting
   Bool read_request = req.is_read;

   // A write request is waiting. For writes, we need wr_addr and wr_data
   // available as we compute HSIZE from wr_data.wstrb
   Bool write_request = !req.is_read && f_single_write_data.notEmpty;

   Bool new_request = (read_request || write_request) && f_single_reqs.notEmpty;

   // Continuous signals driven straight from input FIFOs
   let paddr  = fv_Addr_to_Fabric_Addr (req.addr);
   let pwdata = fv_get_apb_wdata (f_single_write_data.first);
   let pstrb  = req.is_read ? 0 : fv_size_to_strb (req.addr[1:0], req.size_code);
   let pwrite = !req.is_read;

   // Continuous output based on FSM state
   let penable= (rg_state == ACCESS);
   let psel = (rg_state != IDLE);

   // --------
   // IDLE state: Entry point for new request. Setup addr phase signals.
   rule rl_new_tfr (new_request && (rg_state == IDLE));
      rg_state  <= SETUP;
      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_new_tfr (paddr 0x%08h) ", cur_cycle, paddr
                 , "           (pwrite: ", fshow (pwrite), ") "
                 , "           (pstrb: %04b) ", pstrb
                 , "           (pwdata 0x%08h) ", pwdata);
      end
   endrule


   // --------
   // SETUP state: A single cycle to allow address decode to settle on the APB bus
   rule rl_setup (rg_state == SETUP);
      rg_state <= ACCESS;

      if (verbosity > 0) begin
         if (pwrite)
            $display ("%06d:[D]:%m.rl_setup: (addr 0x%08h) (wdata 0x%08h)"
               , cur_cycle, paddr, pwdata);
         else
            $display ("%06d:[D]:%m.rl_setup: (addr 0x%08h) "
               , cur_cycle, paddr);
      end
   endrule


   // --------
   // ACCESS state: Accept read responses. Advance request queue.
   // Forward response to CPU.
   rule rl_read_response (wi_pready && !pwrite && (rg_state == ACCESS));
      rg_state  <= IDLE;

      // Response handling and packing
      Bit #(32) data = wi_prdata;
      Bool ok = !wi_pslverr;

      let rsp = Read_Data { ok: ok, data: data };
      f_single_read_data.enq (rsp);

      // Advance request queue
      f_single_reqs.deq;
      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_read_response: ", cur_cycle, fshow (rsp));
      end
   endrule


   // --------
   // ACCESS state: Accept write response. Advance request queue.
   rule rl_write_response (wi_pready && pwrite && (rg_state == ACCESS));
      rg_state  <= IDLE;

      // Advance request queue
      f_single_reqs.deq;
      f_single_write_data.deq;

      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_write_response", cur_cycle);
      end
   endrule


   // ================================================================
   // INTERFACE
   method Action reset;
`ifdef STANDALONE
      f_single_reqs.clear;
      f_single_write_data.clear;
      f_single_read_data.clear;
`endif
      rg_state <= IDLE;
      if (verbosity > 1) $display ("%06d:[D]:%m.reset", cur_cycle);
   endmethod


`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);
`endif


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
