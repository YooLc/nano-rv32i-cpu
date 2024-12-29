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

module Regs(input clk,
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
			
			input [7:0] cdb_rs_num, // cdb broadcast
			input [31:0]cdb_data,
			input [4:0] Debug_addr, // debug address
			output[31:0] Debug_regs // debug data
	);

	reg [31:0] register [1:31]; 				// r1 - r31

	// Register Alias Table
	// | 7:5    | 4:0       |
	//  FU type  rs number 
	reg [7:0] rat [0:31];  

	integer i;

	wire A_cdb_forward = (cdb_rs_num != 0 && rat[R_addr_A] == cdb_rs_num);
	wire B_cdb_forward = (cdb_rs_num != 0 && rat[R_addr_B] == cdb_rs_num);
	assign rdata_A = (R_addr_A == 0) ? 0 : (A_cdb_forward ? cdb_data : register[R_addr_A]); // read
	assign rdata_B = (R_addr_B == 0) ? 0 : (B_cdb_forward ? cdb_data : register[R_addr_B]); // read

	assign rs_num_A = rat[R_addr_A]; // Reservation Station ID of reg A
	assign rs_num_B = rat[R_addr_B]; // Reservation Station ID of reg B
	
	// Don't use negedge clk
	// Consecutive same FU may cause rat deadlock (waiting for itself forever)
	always @(posedge clk or posedge rst) 
      begin
		if (rst) begin // reset
		    for (i=1; i<32; i=i+1) begin
		      register[i] <= 0;	
		      rat[i]<=0;
		    end
		    rat[0]<=0;
		end 
		else begin
			if (rs_rd_w_en && rs_num_rd != 0 && R_addr_rd != 0) begin
				// Reservation Station RD will produce R_addr_rd
				rat[R_addr_rd] <= rs_num_rd;
			end
			// CDB Snooping
			if (cdb_rs_num != 0) begin
				for (i = 1; i < 32; i = i + 1) begin
					// If the result is ready, update the register
					if (rat[i] == cdb_rs_num) begin
						rat[i] <= 0; // Clear RAT
						register[i] <= cdb_data;
					end
				end
			end
		end
	end
	
    assign Debug_regs = (Debug_addr == 0) ? 0 : register[Debug_addr]; // TEST

endmodule


