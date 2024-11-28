`timescale 1ns / 1ps

module core_sim;
reg clk, rst;
wire [31:0] debug_WB_PC;
// save pc at wb
reg [31:0] wb_pc; 

RV32core core(
    .debug_en(1'b0),
    .debug_step(1'b0),
    .debug_addr(7'b0),
    .debug_data(),
    .clk(clk),
    .rst(rst),
    .interrupter(1'b0),
    .wb_pc(debug_WB_PC) //add output here
);

integer traceout;
initial begin
    // open trace file
    traceout = $fopen("trace.out");
    clk = 0;
    rst = 1;
    wb_pc = 0;
    #2 rst = 0;
end

always #1 clk = ~clk;

always@(clk)begin
    if(wb_pc != debug_WB_PC)begin
        // output signal values to file
        $fdisplay(traceout, " WB_PC=0x%8h",debug_WB_PC );
        wb_pc <= debug_WB_PC;
    end
end
endmodule