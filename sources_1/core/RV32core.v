`timescale 1ns / 1ps
`include "CtrlDefine.vh"
module RV32core (
    input debug_en,  // debug enable
    input debug_step,  // debug step clock
    input [6:0] debug_addr,  // debug address
    output [31:0] debug_data,  // debug data
    output [31:0] wb_addr,
    output [31:0] wb_data,
    output reg [31:0] clk_counter,
    input clk,  // main clock
    input rst,  // synchronous reset
    input interrupter  // interrupt source, for future use
);

  wire debug_clk;
  wire [31:0] debug_regs;
  reg [31:0] Test_signal;
  assign debug_data = debug_addr[5] ? Test_signal : debug_regs;

  debug_clk clock (
      .clk(clk),
      .debug_en(debug_en),
      .debug_step(debug_step),
      .debug_clk(debug_clk)
  );

  wire PC_EN_IF, IS_EN, FU_ALU_EN, FU_mem_EN, FU_mul_EN, FU_div_EN, FU_jump_EN;
  wire RegWrite_ctrl, ALUSrcA_ctrl, ALUSrcB_ctrl, mem_w_ctrl;
  wire [2:0] ImmSel_ctrl, bhw_ctrl, DatatoReg_ctrl;
  wire [3:0] ALUControl_ctrl;
  wire [4:0] Jump_ctrl;
  wire [4:0] rs1_addr_ctrl, rs2_addr_ctrl, rd_ctrl;
  wire [31:0] PC_ctrl, imm_ctrl;


  wire [31:0] PC_IF, next_PC_IF, PC_4_IF, inst_IF;

  wire [31:0] inst_IS, PC_IS, Imm_out_IS;

  wire [31:0] rs1_data_RO, rs2_data_RO, ALUA_RO, ALUB_RO;

  wire FU_ALU_finish, FU_mem_finish, FU_mul_finish, FU_div_finish, FU_jump_finish, is_jump_FU;
  wire [31:0] ALUout_FU, mem_data_FU, mulres_FU, divres_FU, PC_jump_FU, PC_wb_FU;

  wire [31:0] ALUout_WB, mem_data_WB, mulres_WB, divres_WB, PC_wb_WB, wt_data_WB;

  wire ALU_use, MEM_use, MUL_use, DIV_use, JUMP_use;
  wire rs_rd_w_en, rs_rd_w_en_ALU, rs_rd_w_en_MEM, rs_rd_w_en_DIV, rs_rd_w_en_MUL, rs_rd_w_en_JUMP;
  wire [4:0] R_addr_rd;
  wire [7:0] rs_num_rd, rs_num_A, rs_num_B, rs_num_rd_ALU, rs_num_rd_MEM, rs_num_rd_DIV, rs_num_rd_MUL, rs_num_rd_JUMP;
  wire [4:0] op_IS;

  wire [31:0] ALU_src1, ALU_src2, ALU_pc, MUL_src1, MUL_src2, MUL_pc, DIV_src1, DIV_src2, DIV_pc, JUMP_src1, JUMP_src2, JUMP_pc, JUMP_pc_cal, MEM_src1, MEM_src2, MEM_pc, JUMP_A, MEM_A;
  wire [4:0] ALU_op, JUMP_op, MEM_op;
  wire [7:0] ALU_rs_rd, MUL_rs_rd, DIV_rs_rd, JUMP_rs_rd, MEM_rs_rd;

  // cdb broadcast, target RS entry and data
  reg [7:0] cdb_rs_num = 0;
  reg [31:0] cdb_data = 0;

  reg ctrl_stall = 0;
  wire FU_stall;

  // IF
  assign PC_EN_IF = IS_EN | IS_flush | (FU_jump_finish & is_jump_FU);

  REG32 REG_PC (
      .clk(debug_clk),
      .rst(rst),
      .CE (PC_EN_IF),
      .D  (next_PC_IF),
      .Q  (PC_IF)
  );

  add_32 add_IF (
      .a(PC_IF),
      .b(32'd4),
      .c(PC_4_IF)
  );

  MUX2T1_32 mux_IF (
      .I0(PC_4_IF),
      .I1(PC_jump_FU),
      .s (FU_jump_finish & is_jump_FU),
      .o (next_PC_IF)
  );

  ROM_D inst_rom (
      .a  (PC_IF[10:2]),
      .spo(inst_IF)
  );

  // Issue
  REG_IF_IS reg_IF_IS (
      .clk(debug_clk),
      .rst(rst),
      .EN(IS_EN | IS_flush),
      .flush(1'b0),
      .PCOUT(PC_IF),
      .IR(inst_IF),
      .IR_IS(inst_IS),
      .PCurrent_IS(PC_IS)
  );

  ImmGen imm_gen (
      .ImmSel(ImmSel_ctrl),
      .inst_field(inst_IS),
      .Imm_out(Imm_out_IS)
  );

  CtrlUnit ctrl (
      .inst(inst_IS),

      .ImmSel(ImmSel_ctrl),
      .rs1_ctrl(rs1_addr_ctrl),
      .rs2_ctrl(rs2_addr_ctrl),
      .dst_ctrl(rd_ctrl),
      .ALU_use_PC(ALUSrcA_ctrl),
      .ALU_use_imm(ALUSrcB_ctrl),

      .op(op_IS),
      .ALU_use(ALU_use),
      .MEM_use(MEM_use),
      .MUL_use(MUL_use),
      .DIV_use(DIV_use),
      .JUMP_use(JUMP_use)
  );

  assign rs_rd_w_en = ALU_use | MEM_use | MUL_use | DIV_use | JUMP_use;

  // u can integrate RAT into registers
  Regs register (
      .clk(debug_clk),
      .rst(rst),

      // for tomasulo IS
      .rs_rd_w_en(rs_rd_w_en & ~ctrl_stall & ~IS_flush), // does this instr need to write reg? If so, we need to update RAT
      .R_addr_rd(rd_ctrl),  // target reg
      .rs_num_rd(rs_num_rd),  // free RS entry, RAT[target reg] = free RS entry
      .rs_num_A(rs_num_A),  // Qj, the reservation station number of source reg A
      .rs_num_B(rs_num_B),  // Qk, the reservation station number of source reg B

      // cdb broadcast
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .R_addr_A(rs1_addr_ctrl),
      .rdata_A(rs1_data_RO),
      .R_addr_B(rs2_addr_ctrl),
      .rdata_B(rs2_data_RO),
      // cdb broadcast will update register, not here
      .Debug_addr(debug_addr[4:0]),
      .Debug_regs(debug_regs)
  );

  MUX2T1_32 mux_imm_ALU_RO_A (
      .I0(rs1_data_RO),
      .I1(PC_IS),
      .s (ALUSrcA_ctrl),
      .o (ALUA_RO)
  );

  MUX2T1_32 mux_imm_ALU_RO_B (
      .I0(rs2_data_RO),
      .I1(Imm_out_IS),
      .s (ALUSrcB_ctrl),
      .o (ALUB_RO)
  );

  // if u don't implement ROB, when encountering a jump instr, just stall as u did in lab5; for structure hazard, stall as well
  reg IS_flush = 0;

  // Structure hazard
  assign FU_stall = (ALU_use && rs_num_rd_ALU == 0)
                    | (MEM_use && rs_num_rd_MEM == 0)
                    | (MUL_use && rs_num_rd_MUL == 0)
                    | (DIV_use && rs_num_rd_DIV == 0)
                    | (JUMP_use && rs_num_rd_JUMP == 0);

  assign IS_EN = ~IS_flush & (~FU_stall & ~ctrl_stall);

  // Control stall - Lab 5
  always @(posedge clk or posedge rst) begin
    if (rst) begin
      ctrl_stall <= 0;
    end else begin
      // IS
      if (IS_EN & JUMP_use) begin
        ctrl_stall <= 1;
      end else if (FU_jump_finish) begin
        ctrl_stall <= 0;
      end
    end
  end

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      IS_flush <= 0;
    end else if (FU_jump_finish & is_jump_FU) begin
      IS_flush <= 1;
    end else begin
      IS_flush <= 0;
    end
  end

  // allocated free RS entry, if any
  assign rs_num_rd = {8{ALU_use}} & rs_num_rd_ALU |
                       {8{MEM_use}} & rs_num_rd_MEM |
                       {8{MUL_use}} & rs_num_rd_MUL |
                       {8{DIV_use}} & rs_num_rd_DIV |
                       {8{JUMP_use}} & rs_num_rd_JUMP;

  // ALU
  // RS() and LSQ(TBD)
  RS #(
      .FU (`FU_ALU),
      .num(3)
  ) rs_alu (
      .clk(debug_clk),
      .rst(rst),

      .selected(ALU_use & IS_EN),  // Only enter RS when Issue
      .free_rs(rs_num_rd_ALU),  // Free RS entry
      .op(op_IS),
      .Qj(rs_num_A),  // RS that will produce src1
      .Qk(rs_num_B),  // RS that will produce src2
      .Vj(ALUA_RO),  // Value of src1
      .Vk(ALUB_RO),  // Value of src2
      .A(0),
      .pc_IS(PC_IS),

      .en_FU(FU_ALU_EN),
      .vj(ALU_src1),  // Input src1 of ALU
      .vk(ALU_src2),  // Input src2 of ALU
      .op_out(ALU_op),
      .A_o(),
      .pc_FU(ALU_pc),

      .cdb_rs_rd (ALU_rs_rd),
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .pc_debug()
  );

  // FU, just those in lab5
  // ALU ctrl use 0001 (ADDI) for AUIPC
  assign ALUControl_ctrl = (ALU_op == `ALU_AUIPC ? 4'b0001 : ALU_op[3:0]);
  FU_ALU alu (
      .clk(debug_clk),
      .EN(FU_ALU_EN & !done_record[`FU_ALU]),
      .finish(FU_ALU_finish),
      .ALUControl(ALUControl_ctrl),
      .ALUA(ALU_src1),
      .ALUB(ALU_src2),
      .res(ALUout_FU),
      .zero(),
      .overflow()
  );

  // For memory access instrs, to ensure in-order memory access:
  //  if u use RS defined in this project, alloc one entry only
  //  u can also implement load store queue
  //  BTW, u can also implement out-of-order memory access (orz 

  // Sorry i can't :(, just let them be in order
  RS #(
      .FU (`FU_MEM),
      .num(1)
  ) rs_mem (
      .clk(debug_clk),
      .rst(rst),
      .selected(MEM_use & IS_EN),
      .free_rs(rs_num_rd_MEM),  // Free RS entry
      .op(op_IS),
      .Qj(rs_num_A),  // RS that will produce src1, used in load
      .Qk(rs_num_B),  // RS that will produce src2, used in store
      .Vj(rs1_data_RO),  // Value of src1
      .Vk(rs2_data_RO),  // Value of src2
      .A(Imm_out_IS),  // Offset
      .pc_IS(PC_IS),

      .en_FU(FU_mem_EN),
      .vj(MEM_src1),
      .vk(MEM_src2),
      .op_out(MEM_op),
      .A_o(MEM_A),
      .pc_FU(MEM_pc),

      .cdb_rs_rd (MEM_rs_rd),
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .pc_debug()
  );
  assign mem_w_ctrl = MEM_op[0];
  assign bhw_ctrl   = MEM_op[3:1];
  FU_mem mem (
      .clk(debug_clk),
      .EN(FU_mem_EN & !done_record[`FU_MEM]),  // disable EN once finish, avoid redo the same job
      .finish(FU_mem_finish),
      .mem_w(mem_w_ctrl),
      .bhw(bhw_ctrl),
      .rs1_data(MEM_src1),
      .rs2_data(MEM_src2),
      .imm(MEM_A),
      .mem_data(mem_data_FU)
  );

  // Multiplication
  RS #(
      .FU (`FU_MUL),
      .num(3)
  ) rs_mul (
      .clk(debug_clk),
      .rst(rst),

      .selected(MUL_use & IS_EN),
      .free_rs(rs_num_rd_MUL),  // Free RS entry
      .op(op_IS),
      .Qj(rs_num_A),  // RS that will produce src1
      .Qk(rs_num_B),  // RS that will produce src2
      .Vj(rs1_data_RO),  // Value of src1
      .Vk(rs2_data_RO),  // Value of src2
      .A(0),
      .pc_IS(PC_IS),

      .en_FU(FU_mul_EN),
      .vj(MUL_src1),  // Input src1 of MUL
      .vk(MUL_src2),  // Input src2 of MUL
      .op_out(),  // We should use MUL_op, but Lab 5 ignores it, never mind
      .A_o(),
      .pc_FU(MUL_pc),

      .cdb_rs_rd (MUL_rs_rd),
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .pc_debug()
  );
  FU_mul mu (
      .clk(debug_clk),
      .EN(FU_mul_EN & !done_record[`FU_MUL]),
      .finish(FU_mul_finish),
      .A(MUL_src1),
      .B(MUL_src2),
      .res(mulres_FU)
  );

  // Division
  RS #(
      .FU (`FU_DIV),
      .num(3)
  ) rs_du (
      .clk(debug_clk),
      .rst(rst),

      .selected(DIV_use & IS_EN),
      .free_rs(rs_num_rd_DIV),  // Free RS entry
      .op(op_IS),
      .Qj(rs_num_A),  // RS that will produce src1
      .Qk(rs_num_B),  // RS that will produce src2
      .Vj(rs1_data_RO),  // Value of src1
      .Vk(rs2_data_RO),  // Value of src2
      .A(0),
      .pc_IS(PC_IS),

      .en_FU(FU_div_EN),
      .vj(DIV_src1),  // Input src1 of DIV
      .vk(DIV_src2),  // Input src2 of DIV
      .op_out(),
      .A_o(),
      .pc_FU(DIV_pc),

      .cdb_rs_rd (DIV_rs_rd),
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .pc_debug()
  );
  FU_div du (
      .clk(debug_clk),
      .EN(FU_div_EN & !done_record[`FU_DIV]),
      .finish(FU_div_finish),
      .A(DIV_src1),
      .B(DIV_src2),
      .res(divres_FU)
  );

  // Jump
  // Just let it be in order
  RS #(
      .FU (`FU_JUMP),
      .num(1)
  ) rs_jump (
      .clk(debug_clk),
      .rst(rst),

      .selected(JUMP_use & IS_EN),
      .free_rs(rs_num_rd_JUMP),  // Free RS entry
      .op(op_IS),
      .Qj(rs_num_A),  // RS that will produce src1, used in load
      .Qk(rs_num_B),  // RS that will produce src2, used in store
      .Vj(rs1_data_RO),  // Value of src1
      .Vk(rs2_data_RO),  // Value of src2
      .A(Imm_out_IS),  // Offset
      .pc_IS(PC_IS),

      .en_FU(FU_jump_EN),
      .vj(JUMP_src1),
      .vk(JUMP_src2),
      .op_out(JUMP_op),
      .A_o(JUMP_A),
      .pc_FU(JUMP_pc),

      .cdb_rs_rd (JUMP_rs_rd),
      .cdb_rs_num(cdb_rs_num),
      .cdb_data  (cdb_data),

      .pc_debug()
  );
  wire jalr = JUMP_op[4];
  wire [3:0] cmp_ctrl = JUMP_op[3:0];
  FU_jump ju (
      .clk(debug_clk),
      .EN(FU_jump_EN & !done_record[`FU_JUMP]),
      .finish(FU_jump_finish),
      .JALR(jalr),
      .cmp_ctrl(cmp_ctrl),
      .rs1_data(JUMP_src1),
      .rs2_data(JUMP_src2),
      .imm(JUMP_A),
      .PC(JUMP_pc),
      .PC_jump(PC_jump_FU),
      .PC_wb(PC_wb_FU),
      .is_jump(is_jump_FU)
  );


  // CDB broadcast 
  reg [5:1] done_record = 0; // similar to lab5, when multi-FU finish, we need to choose one and delay others 
  reg [31:0] cdb_pc = 0;  // for debug

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      done_record[5:1] <= 5'b0;
    end else begin
      // Ex stage can only set done_record, but not clear it
      done_record[`FU_ALU]  <= done_record[`FU_ALU] | FU_ALU_finish;
      done_record[`FU_MEM]  <= done_record[`FU_MEM] | FU_mem_finish;
      done_record[`FU_MUL]  <= done_record[`FU_MUL] | FU_mul_finish;
      done_record[`FU_DIV]  <= done_record[`FU_DIV] | FU_div_finish;
      done_record[`FU_JUMP] <= done_record[`FU_JUMP] | FU_jump_finish;

      // WB stage can clear done_record, but one at a time
      if (done_record[`FU_ALU]) begin
        done_record[`FU_ALU] <= 0;
      end else if (done_record[`FU_MEM]) begin
        done_record[`FU_MEM] <= 0;
      end else if (done_record[`FU_MUL]) begin
        done_record[`FU_MUL] <= 0;
      end else if (done_record[`FU_DIV]) begin
        done_record[`FU_DIV] <= 0;
      end else if (done_record[`FU_JUMP]) begin
        done_record[`FU_JUMP] <= 0;
      end
    end
  end

  // WB stage: CDB broadcast
  // One instruction at a time
  always @(*) begin
    if (done_record[`FU_ALU]) begin
      cdb_data = ALUout_FU;
      cdb_rs_num = ALU_rs_rd;
      cdb_pc = ALU_pc;
    end else if (done_record[`FU_MEM]) begin
      cdb_data = mem_data_FU;
      cdb_rs_num = MEM_rs_rd;
      cdb_pc = MEM_pc;
    end else if (done_record[`FU_MUL]) begin
      cdb_data = mulres_FU;
      cdb_rs_num = MUL_rs_rd;
      cdb_pc = MUL_pc;
    end else if (done_record[`FU_DIV]) begin
      cdb_data = divres_FU;
      cdb_rs_num = DIV_rs_rd;
      cdb_pc = DIV_pc;
    end else if (done_record[`FU_JUMP]) begin
      cdb_data = PC_wb_FU;
      cdb_rs_num = JUMP_rs_rd;
      cdb_pc = JUMP_pc;
    end else begin
      cdb_data = 0;
      cdb_rs_num = 0;
      cdb_pc = 0;
    end
  end

  assign wb_addr = cdb_pc;
  assign wb_data = cdb_data;

  always @(posedge debug_clk) begin
    if (rst) begin
      clk_counter <= 0;
    end else begin
      clk_counter <= clk_counter + 32'd1;
    end
  end

endmodule
