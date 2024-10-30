`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 10/09/2024 08:59:41 PM
// Design Name: 
// Module Name: core_sim
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module core_sim;
  reg clk, rst;
  wire [31:0] debug_data;
  // wire [7:0] seg_an;
  // wire [7:0] AN;
  RV32core core (
      .debug_en(1'b0),
      .debug_step(1'b0),
      .debug_addr(7'd0),
      .debug_data(debug_data),
      .clk(clk),
      .rst(rst),
      .interrupter(1'b0)
  );

  initial begin
    clk = 0;
    rst = 1;
    #2 rst = 0;
    #180 $finish;
  end
  always #1 clk = ~clk;

endmodule
