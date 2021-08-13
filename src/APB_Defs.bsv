// Copyright (c) 2020 Bluespec, Inc.  All Rights Reserved

package APB_Defs;

// ================================================================
// AHB-Lite bus widths and fabric definitions.
//
// ================================================================

// NOTE: Only FABRIC32 is usable in this release.

typedef 32 APB_Wd_Addr;    // APB only supports 32b addresses
typedef 32 APB_Wd_Data;    // APB only supports 32b data (or less)
typedef TDiv#(APB_Wd_Data, 8) APB_Data_Bytes;   // Bytes in APB data

typedef Bit #(APB_Wd_Addr) APB_Fabric_Addr;
typedef Bit #(APB_Wd_Data) APB_Fabric_Data;
typedef Bit #(APB_Data_Bytes) APB_Fabric_Strb;

// Byte-width of the fabric
Integer bytes_per_fabric_data = valueOf (APB_Data_Bytes);

endpackage
