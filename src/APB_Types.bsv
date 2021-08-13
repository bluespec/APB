// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package APB_Types;

// ================================================================
// Standard types for APB: Initiator and Target interfaces, and
// their bus data types.

// Ref:
//   "AMBA 3 APB Protocol v1.0; Specification",

// ================================================================
// BSV library imports

import DefaultValue  :: *;
import Connectable   :: *;

// Project imports
import APB_Defs      :: *;

// ================================================================
// AHB Initiator signal interface

(* always_ready, always_enabled *)
interface APB_Initiator_IFC;
   // Outputs
   (* result = "PADDR"  *)    method APB_Fabric_Addr paddr;
   (* result = "PENABLE"*)    method Bool            penable;
   (* result = "PWDATA" *)    method APB_Fabric_Data pwdata;
   (* result = "PWRITE" *)    method Bool            pwrite;
   (* result = "PSTRB" *)     method APB_Fabric_Strb pstrb;
   (* result = "PPROT" *)     method Bit #(3)        pprot;

   // Inputs
   (* prefix = "", result = "unused0" *)
   method Action prdata    ((* port = "PRDATA"  *) APB_Fabric_Data data);

   (* prefix = "", result = "unused1" *)
   method Action pready    ((* port = "PREADY"  *) Bool ready);

   (* prefix = "", result = "unused2" *)
   method Action pslverr   ((* port = "PSLVERR" *) Bool slverr);
endinterface

// ================================================================
// AHB Target signal interface

(* always_ready, always_enabled *)
interface APB_Target_IFC;
    // Inputs
   (* prefix = "", result = "unused0" *)
   method Action psel      ((* port = "PSEL" *)      Bool            sel);
   (* prefix = "", result = "unused1" *)
   method Action penable   ((* port = "PENABLE" *)   Bool            en);
   (* prefix = "", result = "unused2" *)
   method Action paddr     ((* port = "PADDR" *)     APB_Fabric_Addr addr);
   (* prefix = "", result = "unused3" *)
   method Action pwdata    ((* port = "PWDATA" *)    APB_Fabric_Data data);
   (* prefix = "", result = "unused4" *)
   method Action pwrite    ((* port = "PWRITE" *)    Bool            write);
   (* prefix = "", result = "unused5" *)
   method Action pstrb     ((* port = "PSTRB" *)     APB_Fabric_Strb strb);
   (* prefix = "", result = "unused6" *)
   method Action pprot     ((* port = "PPROT" *)     Bit #(3)        prot);

   // Outputs
   (* result = "PRDATA" *) method APB_Fabric_Data prdata;
   (* result = "PREADY" *) method Bool            pready;
   (* result = "PSLVERR"*) method Bool            pslverr;
endinterface

// ================================================================
// For debugging

function Action fa_display_bus_signals (APB_Fabric_Addr paddr,
					APB_Fabric_Data pwdata,
					Bool            penable,
					Bool            pwrite,
					APB_Fabric_Data prdata,
					APB_Fabric_Strb pstrb,
					Bit #(3)        pprot,
					Bool            pready,
					Bool            pslverr);
   action
      $display ("    paddr    : %08h", paddr);
      $display ("    pwrite   : ",     fshow (pwrite));
      $display ("    pwdata   : %08h", pwdata);
      $display ("    pstrb    : %04b", pstrb);
      $display ("    pstrb    : %03b", pprot);
      $display ("    penable  : ",     fshow (penable));
      $display ("    prdata   : %08h", prdata);
      $display ("    pready   : ",     fshow (pready));
      $display ("    pslverr  : ",     fshow (pslverr));
   endaction
endfunction

// ================================================================
// Connecting APB_Initiator_IFC directly APB_Target_IFC directly (no
// mulitplexed interconnect)

instance Connectable #(APB_Initiator_IFC, APB_Target_IFC);

   module mkConnection #(APB_Initiator_IFC initiator,
			 APB_Target_IFC    target)
                       (Empty);

      // ----------------------------------------------------------------
      // Initiator to target signals

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_paddr;
         target.paddr (initiator.paddr);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_penable;
         target.penable (initiator.penable);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_pprot;
         target.pprot (initiator.pprot);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_pstrb;
         target.pstrb (initiator.pstrb);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_pwdata;
         target.pwdata (initiator.pwdata);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_pwrite;
         target.pwrite (initiator.pwrite);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hsel;
	 target.psel (True);
      endrule

      // ----------------------------------------------------------------
      // Target to initiator signals

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_prdata;
	 initiator.prdata (target.prdata);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_pready;
	 initiator.pready (target.pready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_presp;
	 initiator.pslverr  (target.pslverr);
      endrule

      // Note:
      //     psel and pready need to be driven separately
      //     e.g., by arbitration module?
      // Do we need preadyin to the target?
   endmodule
endinstance

// ================================================================
// Dummy APB Initiator
// Always drives htrans = IDLE
// and other default value.

module mkDummy_APB_Initiator (APB_Initiator_IFC);
   return
   interface APB_Initiator_IFC;
      // Outputs
      method APB_Fabric_Addr paddr     = 'hBAAD_AADD;
      method APB_Fabric_Data pwdata    = 'hBAAD_BEEF;
      method APB_Fabric_Strb pstrb     = 'h0;
      method Bit #(3)        pprot     = 'h010;
      method Bool            pwrite    = False;
      method Bool            penable   = False;

      // Inputs
      method Action prdata  (APB_Fabric_Data data)  = noAction;
      method Action pready  (Bool            ready) = noAction;
      method Action pslverr (Bool            resp)  = noAction;
   endinterface;
endmodule

// ================================================================
// Dummy Target.  Always drives
// pready = True, pslverr = False and pdata = 'hFADAFADA;

module mkDummy_APB_Target (APB_Target_IFC);
   return
   interface APB_Target_IFC;
      // Inputs
      method Action psel      (Bool            sel)      = noAction;
      method Action penable   (Bool            en)       = noAction;
      method Action paddr     (APB_Fabric_Addr addr)     = noAction;
      method Action pprot     (Bit #(3)        prot)     = noAction;
      method Action pstrb     (APB_Fabric_Strb strb)     = noAction;
      method Action pwrite    (Bool            write)    = noAction;
      method Action pwdata    (APB_Fabric_Data data)     = noAction;

      // Outputs
      method Bool            pready  = True;
      method Bool            pslverr = False;
      method Bit #(wd_data)  prdata  = 'hABCD_EF89;
   endinterface;
endmodule

// ================================================================

endpackage
