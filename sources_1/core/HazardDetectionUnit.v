`timescale 1ps / 1ps

module HazardDetectionUnit (
    input wire clk,
    input wire Branch_ID,
    input wire rs1use_ID,
    input wire rs2use_ID,
    input wire [1:0] hazard_optype_ID,
    input wire [4:0] rd_EXE,
    input wire [4:0] rd_MEM,
    input wire [4:0] rs1_ID,
    input wire [4:0] rs2_ID,
    input wire [4:0] rs2_EXE,
    output reg PC_EN_IF,
    output reg reg_FD_EN,
    output reg reg_FD_stall,
    output reg reg_FD_flush,
    output reg reg_DE_EN,
    output reg reg_DE_flush,
    output reg reg_EM_EN,
    output reg reg_EM_flush,
    output reg reg_MW_EN,
    output reg forward_ctrl_ls,
    output reg [1:0] forward_ctrl_A,
    output reg [1:0] forward_ctrl_B
);
  //according to the diagram, design the Hazard Detection Unit

  reg [1:0] hazard_optype_EXE = 2'b0;
  reg [1:0] hazard_optype_MEM = 2'b0;
  always @(posedge clk) begin
    hazard_optype_EXE <= hazard_optype_ID;
    hazard_optype_MEM <= hazard_optype_EXE;
  end

  parameter OPTYPE_ALU = 2'b01;
  parameter OPTYPE_LOAD = 2'b10;
  parameter OPTYPE_STORE = 2'b11;

  parameter FORWARD_NONE = 2'b00;
  parameter FORWARD_EXE = 2'b01;
  parameter FORWARD_MEM_ALU = 2'b10;
  parameter FORWARD_MEM_DATA = 2'b11;

  wire rs1_fwd_EXE = (rs1use_ID && rs1_ID == rd_EXE);
  wire rs2_fwd_EXE = (rs2use_ID && rs2_ID == rd_EXE);
  wire rs1_fwd_MEM = (rs1use_ID && rs1_ID == rd_MEM);
  wire rs2_fwd_MEM = (rs2use_ID && rs2_ID == rd_MEM);

  always @(*) begin
    // Data Hazard Detection - ALU Type
    if (hazard_optype_ID != 2'b0) begin
      reg_FD_flush = 1'b0;
      reg_DE_EN = 1'b1;
      reg_DE_flush = 1'b0;
      reg_EM_EN = 1'b1;
      reg_EM_flush = 1'b0;
      reg_MW_EN = 1'b1;

      if (rs1_fwd_EXE) begin
        forward_ctrl_A = FORWARD_EXE;
      end else if (rs1_fwd_MEM) begin
        if (hazard_optype_MEM == OPTYPE_ALU) begin
          forward_ctrl_A = FORWARD_MEM_ALU;
        end else begin
          forward_ctrl_A = FORWARD_MEM_DATA;
        end
      end else begin
        forward_ctrl_A = FORWARD_NONE;
      end

      if (rs2_fwd_EXE) begin
        forward_ctrl_B = FORWARD_EXE;
      end else if (rs2_fwd_MEM) begin
        if (hazard_optype_MEM == OPTYPE_ALU) begin
          forward_ctrl_B = FORWARD_MEM_ALU;
        end else begin
          forward_ctrl_B = FORWARD_MEM_DATA;
        end
      end else begin
        forward_ctrl_B = FORWARD_NONE;
      end
    end else begin
      // Good to Go
      
      reg_DE_EN = 1'b1;
      reg_DE_flush = 1'b0;
      reg_EM_EN = 1'b1;
      reg_EM_flush = 1'b0;
      reg_MW_EN = 1'b1;
      forward_ctrl_A = FORWARD_NONE;
      forward_ctrl_B = FORWARD_NONE;
    end

    // Load-Store Hazards
    if (hazard_optype_EXE == OPTYPE_STORE && hazard_optype_MEM == OPTYPE_LOAD && rs2_EXE == rd_MEM) begin
      // No Stall or Flush, forward MEM to EXE
      forward_ctrl_ls = 1'b1;
    end else begin
      forward_ctrl_ls = 1'b0;
    end

    // Load-Use Hazard, must stall the IF stage
    if (hazard_optype_ID == OPTYPE_ALU && hazard_optype_EXE == OPTYPE_LOAD) begin
      PC_EN_IF = 1'b0;
      reg_FD_EN = 1'b1;
      reg_FD_stall = 1'b1;
      reg_FD_flush = 1'b0;
    end else if (Branch_ID) begin // Branch Hazard, must flush the IF stage
      PC_EN_IF = 1'b1;
      reg_FD_EN = 1'b1;
      reg_FD_stall = 1'b0;
      reg_FD_flush = 1'b1;
    end else begin
      PC_EN_IF = 1'b1;
      reg_FD_EN = 1'b1;
      reg_FD_stall = 1'b0;
      reg_FD_flush = 1'b0;
    end

  end

endmodule
