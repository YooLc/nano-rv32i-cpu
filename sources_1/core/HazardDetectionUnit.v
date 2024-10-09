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
    output wire PC_EN_IF,
    output wire reg_FD_EN,
    output wire reg_FD_stall,
    output wire reg_FD_flush,
    output wire reg_DE_EN,
    output wire reg_DE_flush,
    output wire reg_EM_EN,
    output wire reg_EM_flush,
    output wire reg_MW_EN,
    output wire forward_ctrl_ls,
    output wire [1:0] forward_ctrl_A,
    output wire [1:0] forward_ctrl_B
);
  //according to the diagram, design the Hazard Detection Unit

endmodule
