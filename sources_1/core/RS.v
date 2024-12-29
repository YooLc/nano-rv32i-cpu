`timescale 1ns / 1ps
`include "CtrlDefine.vh"

// Reservation Station

module RS #(
    parameter FU  = `FU_BLANK,
    parameter num = 1
) (
    input wire clk,
    input wire rst,

    // Issue, RS fields
    input wire selected,
    output wire [7:0] free_rs,  // free entry
    input wire [4:0] op,
    input wire [7:0] Qj,
    input wire [7:0] Qk,
    input wire [31:0] Vj,
    input wire [31:0] Vk,
    input wire [31:0] A,
    input wire [31:0] pc_IS,

    // Execute
    output wire en_FU,
    output wire [31:0] vj,
    output wire [31:0] vk,
    output wire [4:0] op_out,
    output wire [31:0] A_o,
    output wire [31:0] pc_FU,  // for JUMP FU calculation, also for debug

    // CDB snooping
    input [7:0] cdb_rs_num,
    input [31:0] cdb_data,
    output wire [7:0] cdb_rs_rd,

    output wire [31:0] pc_debug
);
  integer i;

  reg [`PC_H:0] RS[1:num];

  reg [4:0] free_ptr = 1;
  reg [4:0] ready_ptr = 0;

  // free_rs = {FU (7:5), free_ptr (4:0)}, = 0 means RS full
  assign free_rs = ((free_ptr != 0) ? {FU, free_ptr} : 8'b0);
  assign cdb_rs_rd = ((ready_ptr != 0) ? {FU, ready_ptr} : 8'b0);
  assign en_FU = (ready_ptr != 0);
  assign vj = (ready_ptr != 0 ? RS[ready_ptr][`V1_H:`V1_L] : 0); // I don't want to see X floating around
  assign vk = (ready_ptr != 0 ? RS[ready_ptr][`V2_H:`V2_L] : 0);
  assign op_out = (ready_ptr != 0 ? RS[ready_ptr][`OP_H:`OP_L] : 0);
  assign A_o = (ready_ptr != 0 ? RS[ready_ptr][`A_H:`A_L] : 0);
  assign pc_FU = (ready_ptr != 0 ? RS[ready_ptr][`PC_H:`PC_L] : 0);

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      for (i = 1; i <= num; i = i + 1) begin
        RS[i] <= 0;
      end
      free_ptr  <= 1;
      ready_ptr <= 0;
    end else begin
      // Issue: IF -> Reservation Station
      if (selected && free_ptr != 0) begin
        // Allocate an entry
        // rs1 fields
        // First check from CDB
        if (cdb_rs_num != 0 && cdb_rs_num == Qj) begin
          RS[free_ptr][`Q1_H:`Q1_L] = 0;
          RS[free_ptr][`V1_H:`V1_L] = cdb_data;  // Forward from CDB
        end else begin
          if (Qj == 0) begin
            RS[free_ptr][`Q1_H:`Q1_L] = 0;
            RS[free_ptr][`V1_H:`V1_L] = Vj;
          end else begin
            RS[free_ptr][`Q1_H:`Q1_L] = Qj;
            RS[free_ptr][`V1_H:`V1_L] = 0;
          end
        end

        // rs2 fields
        if (cdb_rs_num != 0 && cdb_rs_num == Qk) begin
          RS[free_ptr][`Q2_H:`Q2_L] = 0;
          RS[free_ptr][`V2_H:`V2_L] = cdb_data;  // Forward from CDB
        end else begin
          if (Qk == 0) begin
            RS[free_ptr][`Q2_H:`Q2_L] = 0;
            RS[free_ptr][`V2_H:`V2_L] = Vk;
          end else begin
            RS[free_ptr][`Q2_H:`Q2_L] = Qk;
            RS[free_ptr][`V2_H:`V2_L] = 0;
          end
        end

        // Other fields
        RS[free_ptr][`BUSY] = 1;
        RS[free_ptr][`OP_H:`OP_L] = op;
        RS[free_ptr][`A_H:`A_L] = A;
        RS[free_ptr][`PC_H:`PC_L] = pc_IS;
      end

      // Find another free entry
      free_ptr = 0;
      for (i = 1; i <= num; i = i + 1) begin
        if (RS[i][`BUSY] == 0) begin
          free_ptr = i;
        end
      end

      // Execution
      // Find a busy entry that can go to execution
      if (ready_ptr == 0) begin
        for (i = 1; i <= num; i = i + 1) begin
          if (RS[i][`BUSY] == 1 && RS[i][`Q1_H:`Q1_L] == 0 && RS[i][`Q2_H:`Q2_L] == 0) begin
            ready_ptr = i;
          end
        end
      end

      // Write Back
      if (cdb_rs_num != 0) begin
        // Update cdb data
        for (i = 1; i <= num; i = i + 1) begin
          if (RS[i][`BUSY] && RS[i][`Q1_H:`Q1_L] == cdb_rs_num) begin
            RS[i][`Q1_H:`Q1_L] = 0;
            RS[i][`V1_H:`V1_L] = cdb_data;
          end
          if (RS[i][`BUSY] && RS[i][`Q2_H:`Q2_L] == cdb_rs_num) begin
            RS[i][`Q2_H:`Q2_L] = 0;
            RS[i][`V2_H:`V2_L] = cdb_data;
          end
        end
        // If cdb broadcasts this FU, then remove the entry
        if (cdb_rs_num[7:5] == FU) begin
          RS[cdb_rs_num[4:0]] = 0;
          ready_ptr = 0;
        end
      end
    end
  end
endmodule
