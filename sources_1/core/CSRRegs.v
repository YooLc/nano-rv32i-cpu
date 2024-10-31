`timescale 1ns / 1ps

module CSRRegs (
    input wire clk,
    input wire rst,
    input wire [11:0] raddr,
    input wire [11:0] waddr,
    input wire [31:0] wdata,
    input wire csr_w,
    input wire [1:0] csr_wsc_mode,
    input wire exception_unit_flag,
    input wire [31:0] mcause_w,
    input wire [31:0] mtval_w,
    input wire [31:0] mepc_w,
    input wire mret,
    output wire [31:0] rdata,
    output wire [31:0] mepc_r,
    output wire [31:0] mstatus,
    output wire [31:0] mtvec
);
  // You may need to modify this module for better efficiency

  reg [1:0] priv_mode = 2'b0;
  reg [31:0] CSR[0:15];

  // Address mapping. The address is 12 bits, but only 4 bits are used in this module.
  wire raddr_valid = raddr[11:7] == 5'h6 && raddr[5:3] == 3'h0;
  wire [3:0] raddr_map = (raddr[6] << 3) + raddr[2:0];
  wire waddr_valid = waddr[11:7] == 5'h6 && waddr[5:3] == 3'h0;
  wire [3:0] waddr_map = (waddr[6] << 3) + waddr[2:0];

  assign mstatus = CSR[0];
  assign mtvec   = CSR[5];
  assign mepc_r  = CSR[9];

  assign rdata   = CSR[raddr_map];

  wire [31:0] mstatus_mie = (CSR[0] & ~32'h8) | (CSR[0][3] << 4);  // Clear MIE and set MPIE
  wire [31:0] mstatus_ret = CSR[0] | (CSR[0][7] << 3);  // Set MIE to MPIE

  wire mie = CSR[0][3];
  wire mpie = CSR[0][7];
  wire mpp = CSR[0][12:11];

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      CSR[0] <= 32'h88;  // 0x300 mstatus
      CSR[1] <= 0;  // 0x301 misa
      CSR[2] <= 0;  // 0x3032 medeleg
      CSR[3] <= 0;  // 0x30 mideleg
      CSR[4] <= 32'hfff;  // 0x304 mie
      CSR[5] <= 0;  // 0x305 mtvec
      CSR[6] <= 0;  // 0x306 mcounteren
      CSR[7] <= 0;
      CSR[8] <= 0;  // 0x340 mscratch
      CSR[9] <= 0;  // 0x341 mepc
      CSR[10] <= 0;  // 0x342 mcause
      CSR[11] <= 0;  // 0x343 mtval
      CSR[12] <= 0;  // 0x344 mip
      CSR[13] <= 0;
      CSR[14] <= 0;
      CSR[15] <= 0;
      priv_mode <= 2'b0;
    end else if (csr_w) begin
      if (exception_unit_flag) begin
        // Exception handling
        priv_mode <= 2'b0;
        CSR[0] <= {mstatus_mie[31:13], priv_mode, mstatus_mie[10:0]};  // Set MPP to priv_mode
        CSR[9] <= mepc_w;
        CSR[10] <= mcause_w;
        CSR[11] <= mtval_w;
      end else if (mret) begin
        priv_mode <= CSR[0][12:11];  // priv_mode <- MPP
        CSR[0] <= {mstatus_ret[31:13], 2'b0, mstatus_ret[10:0]};
      end else begin
        // Normal write
        case (csr_wsc_mode)
          2'b01:   CSR[waddr_map] = wdata;
          2'b10:   CSR[waddr_map] = CSR[waddr_map] | wdata;
          2'b11:   CSR[waddr_map] = CSR[waddr_map] & ~wdata;
          default: CSR[waddr_map] = wdata;
        endcase
      end
    end
  end
endmodule
