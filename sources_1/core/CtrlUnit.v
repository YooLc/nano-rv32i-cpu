`timescale 1ns / 1ps
`include "CtrlDefine.vh"

module CtrlUnit(
    input[31:0] inst,
    
    // FU
    output  ALU_use, MEM_use, MUL_use, DIV_use, JUMP_use,
       
    // oprands
    output [4:0] rs1_ctrl, rs2_ctrl, dst_ctrl,
    output ALU_use_PC,
    output ALU_use_imm,
    output[2:0] ImmSel,
    
    // ops
    output [4:0] op
);
   
    wire[6:0] funct7 = inst[31:25];
    wire[2:0] funct3 = inst[14:12];
    wire[6:0] opcode = inst[6:0];
    wire[4:0] rd = inst[11:7];
    wire[4:0] rs1 = inst[19:15];
    wire[4:0] rs2 = inst[24:20];

    wire Rop = opcode == 7'b0110011;
    wire Iop = opcode == 7'b0010011;
    wire Bop = opcode == 7'b1100011;
    wire Lop = opcode == 7'b0000011;
    wire Sop = opcode == 7'b0100011;

    wire funct7_0  = funct7 == 7'h0;
    wire funct7_1  = funct7 == 7'h1;
    wire funct7_32 = funct7 == 7'h20;

    wire funct3_0 = funct3 == 3'h0;
    wire funct3_1 = funct3 == 3'h1;
    wire funct3_2 = funct3 == 3'h2;
    wire funct3_3 = funct3 == 3'h3;
    wire funct3_4 = funct3 == 3'h4;
    wire funct3_5 = funct3 == 3'h5;
    wire funct3_6 = funct3 == 3'h6;
    wire funct3_7 = funct3 == 3'h7;

    wire ADD  = Rop & funct3_0 & funct7_0;
    wire SUB  = Rop & funct3_0 & funct7_32;
    wire SLL  = Rop & funct3_1 & funct7_0;
    wire SLT  = Rop & funct3_2 & funct7_0;
    wire SLTU = Rop & funct3_3 & funct7_0;
    wire XOR  = Rop & funct3_4 & funct7_0;
    wire SRL  = Rop & funct3_5 & funct7_0;
    wire SRA  = Rop & funct3_5 & funct7_32;
    wire OR   = Rop & funct3_6 & funct7_0;
    wire AND  = Rop & funct3_7 & funct7_0;

    wire MUL    = Rop & funct3_0 & funct7_1;
    wire MULH   = Rop & funct3_1 & funct7_1;
    wire MULHSU = Rop & funct3_2 & funct7_1;
    wire MULHU  = Rop & funct3_3 & funct7_1;
    wire DIV    = Rop & funct3_4 & funct7_1;
    wire DIVU   = Rop & funct3_5 & funct7_1;
    wire REM    = Rop & funct3_6 & funct7_1;
    wire REMU    = Rop & funct3_7 & funct7_1;

    wire ADDI  = Iop & funct3_0;	
    wire SLTI  = Iop & funct3_2;
    wire SLTIU = Iop & funct3_3;
    wire XORI  = Iop & funct3_4;
    wire ORI   = Iop & funct3_6;
    wire ANDI  = Iop & funct3_7;
    wire SLLI  = Iop & funct3_1 & funct7_0;
    wire SRLI  = Iop & funct3_5 & funct7_0;
    wire SRAI  = Iop & funct3_5 & funct7_32;

    wire BEQ = Bop & funct3_0;
    wire BNE = Bop & funct3_1;
    wire BLT = Bop & funct3_4;
    wire BGE = Bop & funct3_5;
    wire BLTU = Bop & funct3_6;
    wire BGEU = Bop & funct3_7;

    wire LB =  Lop & funct3_0;
    wire LH =  Lop & funct3_1;
    wire LW =  Lop & funct3_2;
    wire LBU = Lop & funct3_4;
    wire LHU = Lop & funct3_5;

    wire SB = Sop & funct3_0;
    wire SH = Sop & funct3_1;
    wire SW = Sop & funct3_2;

    wire LUI   = opcode == 7'b0110111;
    wire AUIPC = opcode == 7'b0010111;

    wire JAL  =  opcode == 7'b1101111;
    wire JALR = (opcode == 7'b1100111) && funct3_0;

    wire R_valid = AND | OR | ADD | XOR | SLL | SRL | SRA | SUB | SLT | SLTU 
        | MUL | MULH | MULHSU | MULHU | DIV | DIVU | REM | REMU;
    wire I_valid = ANDI | ORI | ADDI | XORI | SLLI | SRLI | SRAI | SLTI | SLTIU;
    wire B_valid = BEQ | BNE | BLT | BGE | BLTU | BGEU;
    wire L_valid = LW | LH | LB | LHU | LBU;
    wire S_valid = SW | SH | SB;

    wire use_ALU = AND | OR | ADD | XOR | SLL | SRL | SRA | SUB | SLT | SLTU
        | I_valid | LUI | AUIPC;
    wire use_MEM = L_valid | S_valid;
    wire use_MUL = MUL | MULH | MULHSU | MULHU;
    wire use_DIV = DIV | DIVU | REM | REMU;
    wire use_JUMP = B_valid | JAL | JALR;
    
    assign ALU_use = use_ALU;
    assign MEM_use = use_MEM;
    assign MUL_use = use_MUL;
    assign DIV_use = use_DIV;
    assign JUMP_use = use_JUMP;


    assign op = {5{ADD}}          & `ALU_ADD    |
                   {5{SUB}}          & `ALU_SUB    |
                   {5{AND}}          & `ALU_AND    |
                   {5{OR}}           & `ALU_OR     |
                   {5{XOR}}          & `ALU_XOR    |
                   {5{SLL}}          & `ALU_SLL    |
                   {5{SRL}}          & `ALU_SRL    |
                   {5{SLT}}          & `ALU_SLT    |
                   {5{SLTU}}         & `ALU_SLTU   |
                   {5{SRA}}          & `ALU_SRA    |
                   {5{LUI}}          & `ALU_LUI    |
//                   {5{AUIPC}}        & `ALU_AUIPC  |
                   {5{AUIPC}}        & `ALU_ADDI  |
                
                   {5{ADDI}}         & `ALU_ADDI   |
                   {5{ANDI}}         & `ALU_ANDI   |
                   {5{ORI}}          & `ALU_ORI    |
                   {5{XORI}}         & `ALU_XORI   |
                   {5{SLLI}}         & `ALU_SLLI   |
                   {5{SRLI}}         & `ALU_SRLI   |
                   {5{SLTI}}         & `ALU_SLTI   |
                   {5{SLTIU}}        & `ALU_SLTIU  |
                   {5{SRAI}}         & `ALU_SRAI   |
                   {5{LB}}           & `MEM_LB     |
                   {5{LH}}           & `MEM_LH     |
                   {5{LW}}           & `MEM_LW     |
                   {5{LBU}}          & `MEM_LBU    |
                   {5{LHU}}          & `MEM_LHU    |
                   {5{SB}}           & `MEM_SB     |
                   {5{SH}}           & `MEM_SH     |
                   {5{SW}}           & `MEM_SW     |
                   {5{MUL}}          & `MUL_MUL    |
                   {5{MULH}}         & `MUL_MULH   |
                   {5{MULHSU}}       & `MUL_MULHU  |
                   {5{MULHU}}        & `MUL_MULHSU |
                   {5{DIV}}          & `DIV_DIV    |
                   {5{DIVU}}         & `DIV_DIVU   |
                   {5{REM}}          & `DIV_REM    |
                   {5{REMU}}         & `DIV_REMU   |
                   {5{BEQ}}          & `JUMP_BEQ   |
                   {5{BNE}}          & `JUMP_BNE   |
                   {5{BLT}}          & `JUMP_BLT   |
                   {5{BGE}}          & `JUMP_BGE   |
                   {5{BLTU}}         & `JUMP_BLTU  |
                   {5{BGEU}}         & `JUMP_BGEU  |
                   {5{JAL}}          & `JUMP_JAL   |
                   {5{JALR}}         & `JUMP_JALR  ;

    assign dst_ctrl = {5{R_valid | I_valid | L_valid | LUI | AUIPC | JAL | JALR}} & rd;
    assign rs1_ctrl = {5{R_valid | I_valid | S_valid | L_valid | B_valid | JALR}} & rs1;
    assign rs2_ctrl = {5{R_valid | S_valid | B_valid}} & rs2;
    
    
    assign ALU_use_PC = ~(R_valid | I_valid | S_valid | L_valid | B_valid | JALR);
    assign ALU_use_imm = ~(R_valid | B_valid);


    assign ImmSel = {3{JALR | L_valid | I_valid}} & `Imm_type_I |
                    {3{B_valid}}                  & `Imm_type_B |
                    {3{JAL}}                      & `Imm_type_J |
                    {3{S_valid}}                  & `Imm_type_S |
                    {3{LUI | AUIPC}}              & `Imm_type_U ;

endmodule