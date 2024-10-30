`timescale 1ns / 1ps

module ROM_D (
    input  [ 6:0] a,
    output [31:0] spo
);

  (* ram_style = "block" *) reg [31:0] inst_data[0:127];

  initial begin
    $readmemh("rom.mem", inst_data);
  end

  assign spo = inst_data[a];

endmodule
