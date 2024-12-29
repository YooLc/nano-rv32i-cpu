`timescale 1ns / 1ps
`include "CtrlDefine.vh"
module  RV32core(
        input debug_en,  // debug enable
        input debug_step,  // debug step clock
        input [6:0] debug_addr,  // debug address
        output[31:0] debug_data,  // debug data
        output[31:0] wb_addr,
        output[31:0] wb_data,
        output reg [31:0] clk_counter,
        input clk,  // main clock
        input rst,  // synchronous reset
        input interrupter  // interrupt source, for future use
    );

    wire debug_clk;
    wire[31:0] debug_regs;
    reg[31:0] Test_signal;
	assign debug_data = debug_addr[5] ? Test_signal : debug_regs;

    debug_clk clock(.clk(clk),.debug_en(debug_en),.debug_step(debug_step),.debug_clk(debug_clk));

    wire PC_EN_IF, IS_EN, FU_ALU_EN, FU_mem_EN, FU_mul_EN, FU_div_EN, FU_jump_EN;
    wire RegWrite_ctrl, ALUSrcA_ctrl, ALUSrcB_ctrl, mem_w_ctrl;
    wire[2:0] ImmSel_ctrl, bhw_ctrl, DatatoReg_ctrl;
    wire[3:0] ALUControl_ctrl;
    wire[4:0] Jump_ctrl;
    wire[4:0] rs1_addr_ctrl, rs2_addr_ctrl, rd_ctrl;
    wire[31:0] PC_ctrl, imm_ctrl;


    wire [31:0] PC_IF, next_PC_IF, PC_4_IF, inst_IF;

    wire[31:0]inst_IS, PC_IS, Imm_out_IS;

    wire[31:0]rs1_data_RO, rs2_data_RO, ALUA_RO, ALUB_RO;

    wire FU_ALU_finish, FU_mem_finish, FU_mul_finish, FU_div_finish, FU_jump_finish, is_jump_FU;
    wire[31:0]ALUout_FU, mem_data_FU, mulres_FU, divres_FU, PC_jump_FU, PC_wb_FU;

    wire[31:0]ALUout_WB, mem_data_WB, mulres_WB, divres_WB, PC_wb_WB, wt_data_WB;
    
    wire  ALU_use, MEM_use, MUL_use, DIV_use, JUMP_use;
    wire rs_rd_w_en, rs_rd_w_en_ALU, rs_rd_w_en_MEM, rs_rd_w_en_DIV, rs_rd_w_en_MUL, rs_rd_w_en_JUMP;
	wire  [4:0]R_addr_rd;
	wire [7:0] rs_num_rd, rs_num_A, rs_num_B, rs_num_rd_ALU, rs_num_rd_MEM, rs_num_rd_DIV, rs_num_rd_MUL, rs_num_rd_JUMP;
	wire [4:0] op_IS;
	
	wire [31:0] ALU_src1, ALU_src2, ALU_pc, MUL_src1, MUL_src2, MUL_pc, DIV_src1, DIV_src2, DIV_pc, JUMP_src1, JUMP_src2, JUMP_pc, JUMP_pc_cal, MEM_src1, MEM_src2, MEM_pc, JUMP_A, MEM_A;
	wire [4:0] ALU_op, JUMP_op, MEM_op;
	wire [7:0] ALU_rs_rd, MUL_rs_rd,DIV_rs_rd, JUMP_rs_rd, MEM_rs_rd;
	
	reg [31:0]ALUout_WB_, mem_data_WB_, mulres_WB_, divres_WB_, PC_wb_WB_, wt_data_WB_;
	
	// cdb broadcast, target RS entry and data
	wire [7:0] cdb_rs_num;
	wire [31:0] cdb_data;
	
	reg ctrl_stall=0;
    reg FU_stall=0;


    // IF
    assign PC_EN_IF =  ;

    REG32 REG_PC(.clk(debug_clk),.rst(rst),.CE(PC_EN_IF),.D(next_PC_IF),.Q(PC_IF));
    
    add_32 add_IF(.a(PC_IF),.b(32'd4),.c(PC_4_IF));

    MUX2T1_32 mux_IF(.I0(PC_4_IF),.I1(PC_jump_FU),.s(       ),.o(next_PC_IF));

    ROM_D inst_rom(.a(PC_IF[10:2]),.spo(inst_IF));


    //Issue
    REG_IF_IS reg_IF_IS(.clk(debug_clk),.rst(rst),.EN(IS_EN),
        .flush(     ),.PCOUT(PC_IF),.IR(inst_IF),
        .IR_IS(inst_IS),.PCurrent_IS(PC_IS));
    
    ImmGen imm_gen(.ImmSel(ImmSel_ctrl),.inst_field(inst_IS),.Imm_out(Imm_out_IS));

    CtrlUnit ctrl(
        .inst(inst_IS),
        
        .ImmSel(ImmSel_ctrl),
        .rs1_ctrl(rs1_addr_ctrl),.rs2_ctrl(rs2_addr_ctrl), .dst_ctrl(rd_ctrl) ,
        .ALU_use_PC(ALUSrcA_ctrl),.ALU_use_imm(ALUSrcB_ctrl),
        
        .op(op_IS),
        .ALU_use(ALU_use), .MEM_use(MEM_use), .MUL_use(MUL_use), .DIV_use(DIV_use), .JUMP_use(JUMP_use)
    );

    // u can integrate RAT into registers
    Regs register(.clk(debug_clk),.rst(rst),
    
       // for tomasulo IS
	   .rs_rd_w_en( ), // does this instr need to write reg? If so, we need to update RAT
	   .R_addr_rd(rd_ctrl),    // target reg
	   .rs_num_rd(rs_num_rd),  // free RS entry, RAT[target reg] = free RS entry
	   .rs_num_A(),    // Qj
	   .rs_num_B(),    // Qk
        
        // cdb broadcast
        .cdb_rs_num(cdb_rs_num), 
		.cdb_data(cdb_data),
        
        .R_addr_A(rs1_addr_ctrl),.rdata_A(rs1_data_RO),
        .R_addr_B(rs2_addr_ctrl),.rdata_B(rs2_data_RO),
//        .L_S(RegWrite_ctrl),.Wt_addr(rd_ctrl),.Wt_data(wt_data_WB),   // cdb broadcast will update register, not here
        .Debug_addr(debug_addr[4:0]),.Debug_regs(debug_regs));

    MUX2T1_32 mux_imm_ALU_RO_A(.I0(rs1_data_RO),.I1(PC_IS),.s(ALUSrcA_ctrl),.o(ALUA_RO));

    MUX2T1_32 mux_imm_ALU_RO_B(.I0(rs2_data_RO),.I1(Imm_out_IS),.s(ALUSrcB_ctrl),.o(ALUB_RO));


    // if u don't implement ROB, when encountering a jump instr, just stall as u did in lab5; for structure hazard, stall as well  
    ctrl_stall ;
    FU_stall ;

    assign IS_EN =  ;
         
    // allocated free RS entry, if any
    assign rs_num_rd = {8{ALU_use}}          &     rs_num_rd_ALU |
            

    // RS() and LSQ(TBD)
    RS #(.FU(`FU_ALU), .num(3)) rs_alu(
        .clk(debug_clk),
	    .rst(rst),
		
		.selected(ALU_use),
	    .free_rs(rs_num_rd_ALU),
	    .op(op_IS),
		.Qj(),
		.Qk(),
		.Vj(),
		.Vk(),
		.A(0),
		.pc_IS(PC_IS),
		
		.en_FU(FU_ALU_EN),
	    .vj(),
	    .vk(),
		.op_out(ALU_op),
		.A_o(),
		.pc_FU(),
		
		
		.cdb_rs_rd(ALU_rs_rd),
		.cdb_rs_num(cdb_rs_num),
		.cdb_data(cdb_data),
		
		
		.pc_debug(ALU_pc)
    );
    
   
    // For memory access instrs, to ensure in-order memory access:
    //  if u use RS defined in this project, alloc one entry only
    //  u can also implement load store queue
    //  BTW, u can also implement out-of-order memory access (orz 
    

    // FU, just those in lab5
    FU_ALU alu(.clk(debug_clk),.EN(FU_ALU_EN),.finish(FU_ALU_finish),
        .ALUControl(),.ALUA(),.ALUB(),.res(ALUout_FU),
        .zero(),.overflow());

    FU_mem mem(.clk(debug_clk),.EN(FU_mem_EN),.finish(FU_mem_finish),
        .mem_w(),.bhw(),.rs1_data(),.rs2_data(),
        .imm(),.mem_data(mem_data_FU));

    FU_mul mu(.clk(debug_clk),.EN(FU_mul_EN),.finish(FU_mul_finish),
        .A(),.B(),.res(mulres_FU));

    FU_div du(.clk(debug_clk),.EN(FU_div_EN),.finish(FU_div_finish),
        .A(),.B(),.res(divres_FU));

    FU_jump ju(.clk(debug_clk),.EN(FU_jump_EN),.finish(FU_jump_finish),
        .JALR(),.cmp_ctrl(),.rs1_data(),.rs2_data(),
        .imm(),.PC(),.PC_jump(),.PC_wb(PC_wb_FU),.is_jump());


    // CDB broadcast 
    reg [5:1] done_record; // similar to lab5, when multi-FU finish, we need to choose one and delay others 
    wire [31:0]cdb_pc;  // for debug
    
    assign cdb_data =  
    assign cdb_rs_num = 
    assign cdb_pc = 


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