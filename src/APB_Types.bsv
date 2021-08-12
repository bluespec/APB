// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package APB_Types;

// ================================================================
// Standard types for APB: Initiator and Target interfaces, and
// their bus data types.

// Ref:
//   "AMBA 3 APB Protocol v1.0; Specification",

// ================================================================
// BSV library imports

import DefaultValue :: *;
import Connectable  :: *;

// ================================================================
// AHB Initiator signal interface

(* always_ready, always_enabled *)
interface APB_Initiator_IFC #(numeric type wd_data);
   // Outputs
   (* result = "PADDR"  *)    method Bit #(32)       paddr;
   (* result = "PENABLE"*)    method Bool            penable;
   (* result = "PWDATA" *)    method Bit #(wd_data)  pwdata;
   (* result = "PWRITE" *)    method Bool            pwrite;

   // Inputs
   (* prefix = "", result = "unused0" *)
   method Action prdata    ((* port = "PRDATA"  *) Bit #(wd_data) data);

   (* prefix = "", result = "unused1" *)
   method Action pready    ((* port = "PREADY"  *) Bool ready);

   (* prefix = "", result = "unused2" *)
   method Action pslverr   ((* port = "PSLVERR" *) Bool slverr);
endinterface

// ================================================================
// AHB Target signal interface

(* always_ready, always_enabled *)
interface APB_Target_IFC #(numeric type wd_data);
    // Inputs
   (* prefix = "", result = "unused0" *)
   method Action psel      ((* port = "PSEL" *)      Bool            sel);
   (* prefix = "", result = "unused1" *)
   method Action penable   ((* port = "PENABLE" *)   Bool            en);
   (* prefix = "", result = "unused2" *)
   method Action paddr     ((* port = "PADDR" *)     Bit #(32)       addr);
   (* prefix = "", result = "unused3" *)
   method Action pwdata    ((* port = "PWDATA" *)    Bit #(wd_data)  data);
   (* prefix = "", result = "unused4" *)
   method Action pwrite    ((* port = "PWRITE" *)    Bool            write);

   // Outputs
   (* result = "PRDATA" *) method Bit #(wd_data) prdata;
   (* result = "PREADY" *) method Bool           pready;
   (* result = "PSLVERR"*) method Bool           pslverr;
endinterface

// ================================================================
// For debugging

function Action fa_display_bus_signals (Bit #(32)      paddr,
					Bit #(wd_data) pwdata,
					Bool           penable,
					Bool           pwrite,
					Bit #(wd_data) prdata,
					Bool           pready,
					Bool           pslverr);
   action
      $display ("    paddr    : %08h", paddr);
      $display ("    pwrite   : ",     fshow (pwrite));
      $display ("    pwdata   : %08h", pwdata);
      $display ("    penable  : ",     fshow (penable));
      $display ("    prdata   : %08h", prdata);
      $display ("    pready   : ",     fshow (pready));
      $display ("    pslverr  : ",     fshow (pslverr));
   endaction
endfunction

// ================================================================
// Connecting AHB_Initiator_IFC directly AHB_Target_IFC directly (no
// multi-layer interconnect

instance Connectable #(APB_Initiator_IFC #(wd_data),
		       APB_Target_IFC #(wd_data));

   module mkConnection #(APB_Initiator_IFC #(wd_data) initiator,
			 APB_Target_IFC #(wd_data) target)
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
      rule rl_connect_hresp;
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

module mkDummy_APB_Initiator (APB_Initiator_IFC #(wd_data));
   return
   interface APB_Initiator_IFC;
      // Outputs
      method Bit #(32)       paddr     = 'hBAAD_AADD;
      method Bit #(wd_data)  pwdata    = 'hBAAD_BEEF;
      method Bool            pwrite    = False;
      method Bool            penable   = False;

      // Inputs
      method Action prdata  (Bit #(wd_data)  data)  = noAction;
      method Action pready  (Bool            ready) = noAction;
      method Action pslverr (Bool            resp)  = noAction;
   endinterface;
endmodule

// ================================================================
// Dummy Target.  Always drives
// pready = True, pslverr = False and pdata = 'hFADAFADA;

module mkDummy_APB_Target (APB_Target_IFC #(wd_data));
   return
   interface APB_Target_IFC;
      // Inputs
      method Action psel      (Bool           sel)      = noAction;
      method Action penable   (Bool           en)       = noAction;
      method Action paddr     (Bit #(32)      addr)     = noAction;
      method Action pwrite    (Bool           write)    = noAction;
      method Action pwdata    (Bit #(wd_data) data)     = noAction;

      // Outputs
      method Bool            pready  = True;
      method APB_Resp        pslverr = False;
      method Bit #(wd_data)  prdata  = 'hABCD_EF89;
   endinterface;
endmodule

// ================================================================

endpackage
