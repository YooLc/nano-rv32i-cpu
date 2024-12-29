`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    21:34:44 03/12/2012 
// Design Name: 
// Module Name:    Regs 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////

module     Regs(input clk,
				input rst,
				
				input [4:0] R_addr_A, 
				input [4:0] R_addr_B, 
				output [31:0] rdata_A,
				output [31:0] rdata_B,
				
				// for tomasulo IS
				input rs_rd_w_en,
				input [4:0] R_addr_rd, 
				input [7:0] rs_num_rd,
				output [7:0] rs_num_A,
				output [7:0] rs_num_B,
				
				input [7:0] cdb_rs_num,  // cdb broadcast
			    input [31:0]cdb_data,
				
//				input [4:0] Wt_addr, 
//				input [31:0]Wt_data, 
//				input L_S, 
				input [4:0] Debug_addr,         // debug address
				output[31:0] Debug_regs        // debug data
			  );

reg [31:0] register [1:31]; 				// r1 - r31

// | 7:5    | 4:0       |
//  FU type  rs number 
reg [7:0] rat [0:31];  

integer i;

	assign rdata_A = (R_addr_A == 0)? 0 : register[R_addr_A]; 	 // read
	assign rdata_B = (R_addr_B == 0)? 0 : register[R_addr_B];    // read
	
	
	 assign rs_num_A =  
	 assign rs_num_B =  
	
	
	always @(negedge clk or posedge rst) 
      begin
		if (rst) 	 begin 			// reset
		    for (i=1; i<32; i=i+1) begin
		      register[i] <= 0;	
		      rat[i]<=0;
		    end
		    rat[0]<=0;
		end 
		else begin		          
		          
//		     if ((Wt_addr != 0) && (L_S == 1)) 	// write
//		     register[Wt_addr] <= Wt_data;
		end
	end
    	
    assign Debug_regs = (Debug_addr == 0) ? 0 : register[Debug_addr];               //TEST

endmodule


