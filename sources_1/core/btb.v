`timescale 1ns / 1ps

module BTB(
    input wire clk,
    input wire rst,
    input wire [31:0] IF_PC,
    input wire [31:0] IF_PC_4,
    input wire [31:0] ID_PC,
    input wire [31:0] ID_Jump_PC,
    input wire ID_branch,
    input wire ID_taken,
    output wire predict_taken,
    output wire predict_do_branch,
    output reg btb_hit,
    output reg [31:0] Predict_PC
);
    // 64 Entries BHT, direct map
    reg [1:0] bht[0:255];
    // 16 Entries BTB, fully associative
    reg [63:0] btb[0:63];

    parameter [1:0] BHT_NT_00 = 2'b00;
    parameter [1:0] BHT_NT_01 = 2'b01;
    parameter [1:0] BHT_T_10 = 2'b10;
    parameter [1:0] BHT_T_11 = 2'b11;

    integer i;
    // Lookup BHT with IF
    wire [7:0] bht_idx_IF = IF_PC[9:2];
    assign predict_taken = bht[bht_idx_IF][1];

    // Update BHT with ID
    wire [7:0] bht_idx_ID = ID_PC[9:2];
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            for (i = 0; i < 256; i = i + 1) begin
                bht[i] <= BHT_NT_00;
            end
        end else if (ID_branch) begin
            if (ID_taken) begin
                case (bht[bht_idx_ID])
                    BHT_NT_00: bht[bht_idx_ID] <= BHT_NT_01;
                    BHT_NT_01: bht[bht_idx_ID] <= BHT_T_11;
                    BHT_T_10: bht[bht_idx_ID] <= BHT_T_11;
                    BHT_T_11: bht[bht_idx_ID] <= BHT_T_11;
                endcase
            end else begin
                case (bht[bht_idx_ID])
                    BHT_NT_00: bht[bht_idx_ID] <= BHT_NT_00;
                    BHT_NT_01: bht[bht_idx_ID] <= BHT_NT_00;
                    BHT_T_10: bht[bht_idx_ID] <= BHT_NT_00;
                    BHT_T_11: bht[bht_idx_ID] <= BHT_T_10;
                endcase
            end
        end
    end

    // Lookup BTB with IF
    reg [31:0] btb_target;
    assign predict_do_branch = (predict_taken & btb_hit);
    always @(*) begin
        btb_hit = 1'b0; // 阻塞赋值
        btb_target = 32'b0;
        for (i = 0; i < 64; i = i + 1) begin
            if (btb[i][63:32] == IF_PC) begin
                btb_hit = 1'b1;
                btb_target = btb[i][31:0];
            end
        end
        Predict_PC = (predict_do_branch ? btb_target : IF_PC_4);
    end

    // Update BTB with ID
    reg [5:0] replace_ptr;
    reg [3:0] match_index;
    reg match_found;
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            for (i = 0; i < 64; i = i + 1) begin
                btb[i] <= 64'h0;
            end
            replace_ptr <= 4'h0;
        end else begin
            if (ID_taken) begin
                // Check if ID_PC is already in BTB
                match_found = 1'b0;
                match_index = 4'h0;
                for (i = 0; i < 64; i = i + 1) begin
                    if (btb[i][63:32] == ID_PC) begin
                        match_found = 1'b1;
                        match_index = i;
                    end
                end
                if (match_found) begin
                    // Update the matching entry's target PC
                    btb[match_index][31:0] <= ID_Jump_PC;
                end else begin
                    // Replace the entry at replace_ptr
                    btb[replace_ptr][63:32] <= ID_PC;
                    btb[replace_ptr][31:0] <= ID_Jump_PC;
                    // Increment replace_ptr
                    replace_ptr <= replace_ptr + 4'd1;
                end
            end
        end
    end
endmodule