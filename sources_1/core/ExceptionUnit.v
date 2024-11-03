`timescale 1ns / 1ps

module ExceptionUnit (
    input wire clk,
    input wire rst,
    input wire csr_rw_in,
    // write/set/clear (funct bits from instruction)
    input wire [1:0] csr_wsc_mode_in,
    input wire csr_w_imm_mux,
    input wire [11:0] csr_rw_addr_in,
    input wire [31:0] csr_w_data_reg,
    input wire [4:0] csr_w_data_imm,
    output wire [31:0] csr_r_data_out,

    input wire interrupt,
    input wire illegal_inst,
    input wire l_access_fault,
    input wire s_access_fault,
    input wire ecall_m,

    input wire mret,

    input wire [31:0] epc_cur,
    input wire [31:0] epc_cur_inst,
    input wire [31:0] mem_cur_addr,
    input wire [31:0] epc_next,
    output wire [31:0] PC_redirect,
    output wire redirect_mux,

    output reg  reg_FD_flush,
    output reg  reg_DE_flush,
    output reg  reg_EM_flush,
    output reg  reg_MW_flush,
    output reg RegWrite_cancel,
    output reg MemWrite_cancel
);

  // According to the diagram, design the Exception Unit
  // You can modify any code in this file if needed!
  reg csr_w;
  reg [11:0] csr_waddr;
  reg [31:0] csr_wdata;
  reg [1:0] csr_wsc;  // 01: wdata, 10: set bit, 11: unset bit
  wire [11:0] csr_raddr;

  wire [31:0] mstatus;
  wire [31:0] mtvec;
  wire [31:0] mepc_r;
  wire [31:0] csr_rdata;

  reg exception_flag;
  reg interrupt_flag;
  reg [31:0] mcause_w;
  reg [31:0] mtval_w;
  reg [31:0] mepc_w;

  CSRRegs csr (
      .clk(clk),
      .rst(rst),
      .csr_w(csr_w),
      .mret(mret),
      .raddr(csr_raddr),
      .waddr(csr_waddr),
      .rdata(csr_rdata),
      .wdata(csr_wdata),
      .exception_unit_flag(exception_flag | interrupt_flag),
      .mcause_w(mcause_w),
      .mtval_w(mtval_w),
      .mepc_w(mepc_w),
      .mstatus(mstatus),
      .mtvec(mtvec),
      .mepc_r(mepc_r),
      .csr_wsc_mode(csr_wsc)
  );

  // mcause exception code
  parameter [31:0] MCAUSE_ILLEGAL_INST = 32'd2;
  parameter [31:0] MCAUSE_L_ACCESS_FAULT = 32'd5;
  parameter [31:0] MCAUSE_S_ACCESS_FAULT = 32'd7;
  parameter [31:0] MCAUSE_ECALL_M = 32'd11;

  // mcause interrupt code
  parameter [31:0] MCAUSE_SOFT_INT = 32'd3;
  parameter [31:0] MCAUSE_MACHINE_EXT_INT = 32'd11;

  // CSR register addresses - mapped
  parameter [11:0] CSR_MSTATUS = 12'h300;
  parameter [11:0] CSR_MIE = 12'h304;
  parameter [11:0] CSR_MTVEC = 12'h305;
  parameter [11:0] CSR_MEPC = 12'h341;
  parameter [11:0] CSR_MCAUSE = 12'h342;
  parameter [11:0] CSR_MTVAL = 12'h343;

  assign csr_raddr = csr_rw_addr_in;
  assign csr_r_data_out = csr_rdata;

  assign PC_redirect = mret ? mepc_r : mtvec;
  assign redirect_mux = exception_flag | interrupt_flag | mret;

  always @(*) begin
    if (exception_flag) begin
      RegWrite_cancel = 1'b1;
      MemWrite_cancel = 1'b1;
    end else begin // For interrups, we need the instruction to be executed
      RegWrite_cancel = 1'b0;
      MemWrite_cancel = 1'b0;
    end
  end

  // Generates mcause_w, mtval_w, mepc_w, and exception/interrupt flags
  always @(*) begin
    exception_flag = illegal_inst | ecall_m | l_access_fault | s_access_fault;
    interrupt_flag = interrupt & mstatus[3];  // if MIE is set, then interrupt is enabled
    if (exception_flag) begin
      // Exception at ID stage
      if (illegal_inst) begin
        mcause_w = MCAUSE_ILLEGAL_INST;
        mtval_w = epc_cur_inst;
        reg_DE_flush = 1'b1;
        reg_EM_flush = 1'b1;
        reg_MW_flush = 1'b1;
      end else if (ecall_m) begin
        mcause_w = MCAUSE_ECALL_M;
        mtval_w = 0;
        reg_DE_flush = 1'b1;
        reg_EM_flush = 1'b1;
        reg_MW_flush = 1'b1;
        // Exception at MEM stage
      end else if (l_access_fault) begin
        mcause_w = MCAUSE_L_ACCESS_FAULT;
        mtval_w = mem_cur_addr;
        reg_DE_flush = 1'b1;
        reg_EM_flush = 1'b1;
        reg_MW_flush = 1'b1;
      end else if (s_access_fault) begin
        mcause_w = MCAUSE_S_ACCESS_FAULT;
        mtval_w = mem_cur_addr;
        reg_DE_flush = 1'b1;
        reg_EM_flush = 1'b1;
        reg_MW_flush = 1'b1;
      end
      // For exception, mepc is the current PC
      mepc_w = epc_cur;
      reg_FD_flush = 1'b1;
    end else if (interrupt_flag) begin
      mcause_w = MCAUSE_MACHINE_EXT_INT;
      mtval_w  = 0;
      // For interrupt, mepc is the PC that will be executed next
      mepc_w   = epc_next;
      reg_FD_flush = 1'b1;
      reg_DE_flush = 1'b1;
      reg_EM_flush = 1'b1;
      reg_MW_flush = 1'b1;
    end else if (mret) begin
      mcause_w = 0;
      mtval_w  = 0;
      mepc_w = 32'b0;
      reg_FD_flush = 1'b1;
      reg_DE_flush = 1'b1;
      reg_EM_flush = 1'b1;
      reg_MW_flush = 1'b1;
    end else begin
      mcause_w = 0;
      mtval_w  = 0;
      mepc_w = 32'b0;
      reg_FD_flush = 1'b0;
      reg_DE_flush = 1'b0;
      reg_EM_flush = 1'b0;
      reg_MW_flush = 1'b0;
    end
  end

  always @(*) begin
    if (csr_w_imm_mux) begin
      csr_wdata = {27'b0, csr_w_data_imm};
    end else begin
      csr_wdata = csr_w_data_reg;
    end
  end

  // Write to CSR
  always @(*) begin
    if (exception_flag | interrupt_flag) begin
      csr_w = 1'b0;
      csr_waddr = 12'b0;
      csr_wsc = 2'b00;
    end else begin
      // Normal CSR operation
      csr_w = csr_rw_in;
      csr_waddr = csr_rw_addr_in;
      csr_wsc = csr_wsc_mode_in;
    end
  end

endmodule
