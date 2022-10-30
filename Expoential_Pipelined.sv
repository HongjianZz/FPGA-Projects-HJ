module lab1#
(
	parameter WIDTHIN = 16,		// Input format is Q2.14 (2 integer bits + 14 fractional bits = 16 bits)
	parameter WIDTHOUT = 32,	// Intermediate/Output format is Q7.25 (7 integer bits + 25 fractional bits = 32 bits)
	// Taylor coefficients for the first five terms in Q2.14 format
	parameter [WIDTHIN-1:0] A0 = 16'b01_00000000000000, // a0 = 1
	parameter [WIDTHIN-1:0] A1 = 16'b01_00000000000000, // a1 = 1
	parameter [WIDTHIN-1:0] A2 = 16'b00_10000000000000, // a2 = 1/2
	parameter [WIDTHIN-1:0] A3 = 16'b00_00101010101010, // a3 = 1/6
	parameter [WIDTHIN-1:0] A4 = 16'b00_00001010101010, // a4 = 1/24
	parameter [WIDTHIN-1:0] A5 = 16'b00_00000010001000  // a5 = 1/120
)
(
	input clk,
	input reset,	
	input i_valid,
	input i_ready,
	output o_valid,
	output o_ready,
	
	input [WIDTHIN-1:0] i_x,
	output [WIDTHOUT-1:0] o_y
);
//Output value could overflow (32-bit output, and 16-bit inputs multiplied
//together repeatedly).  Don't worry about that -- assume that only the bottom
//32 bits are of interest, and keep them.
logic [WIDTHIN-1:0] x;	// Register to hold input X
logic [WIDTHOUT-1:0] y_Q;	// Register to hold output Y

logic valid_Q1;		// Restore the i_valid for the corresponding input x
logic valid_Q2;		// Restore the i_valid for the corresponding input x
logic valid_Q3;		// Restore the i_valid for the corresponding input x
logic valid_Q4;		// Restore the i_valid for the corresponding input x
logic valid_Q5;		// Restore the i_valid for the corresponding input x
logic valid_Q6;		// Restore the i_valid for the corresponding input x
logic valid_Q7;		// Restore the i_valid for the corresponding input x
logic valid_Q8;		// Restore the i_valid for the corresponding input x
logic valid_Q9;		// Restore the i_valid for the corresponding input x
logic valid_Q10;		// Restore the i_valid for the corresponding input x
logic valid_Q11;
logic valid_Result;  //To determine if the computation result is valid based on its i_valid at the input time
// signal for enabling sequential circuit elements
logic enable;

// Signals for computing the y output
logic [WIDTHOUT-1:0] m0_out; // A5 * x
logic [WIDTHOUT-1:0] a0_out; // A5 * x + A4
logic [WIDTHOUT-1:0] m1_out; // (A5 * x + A4) * x
logic [WIDTHOUT-1:0] a1_out; // (A5 * x + A4) * x + A3
logic [WIDTHOUT-1:0] m2_out; // ((A5 * x + A4) * x + A3) * x
logic [WIDTHOUT-1:0] a2_out; // ((A5 * x + A4) * x + A3) * x + A2
logic [WIDTHOUT-1:0] m3_out; // (((A5 * x + A4) * x + A3) * x + A2) * x
logic [WIDTHOUT-1:0] a3_out; // (((A5 * x + A4) * x + A3) * x + A2) * x + A1
logic [WIDTHOUT-1:0] m4_out; // ((((A5 * x + A4) * x + A3) * x + A2) * x + A1) * x
logic [WIDTHOUT-1:0] a4_out; // ((((A5 * x + A4) * x + A3) * x + A2) * x + A1) * x + A0
logic [WIDTHOUT-1:0] y_D; // Temeperate register to restore intermediate value
// Temperate pipeline register to sync the input 
logic [WIDTHIN-1:0] x0_in; // temperate register to synchronize the input matches with operation
logic [WIDTHIN-1:0] x1_in; // input x will be hold from x0_in and then shift to x_7 by clk
logic [WIDTHIN-1:0] x2_in;
logic [WIDTHIN-1:0] x3_in;
logic [WIDTHIN-1:0] x4_in;
logic [WIDTHIN-1:0] x5_in;
logic [WIDTHIN-1:0] x6_in;
logic [WIDTHIN-1:0] x7_in;


// compute y value
mult16x16 Mult0 (.clk,	.i_ready,	.i_dataa(A5), 		.i_datab(x), 	.o_res(m0_out));
addr32p16 Addr0 (.clk,	.i_ready,	.i_dataa(m0_out), 	.i_datab(A4), 	.o_res(a0_out));

mult32x16 Mult1 (.clk,	.i_ready,	.i_dataa(a0_out), 	.i_datab(x1_in), 	.o_res(m1_out));
addr32p16 Addr1 (.clk,	.i_ready,	.i_dataa(m1_out), 	.i_datab(A3), 	.o_res(a1_out));

mult32x16 Mult2 (.clk,	.i_ready,	.i_dataa(a1_out), 	.i_datab(x3_in), 	.o_res(m2_out));
addr32p16 Addr2 (.clk,	.i_ready,	.i_dataa(m2_out), 	.i_datab(A2), 	.o_res(a2_out));

mult32x16 Mult3 (.clk,	.i_ready,	.i_dataa(a2_out), 	.i_datab(x5_in), 	.o_res(m3_out));
addr32p16 Addr3 (.clk,	.i_ready,	.i_dataa(m3_out), 	.i_datab(A1), 	.o_res(a3_out));

mult32x16 Mult4 (.clk,	.i_ready,	.i_dataa(a3_out), 	.i_datab(x7_in), 	.o_res(m4_out));
addr32p16 Addr4 (.clk,	.i_ready,	.i_dataa(m4_out), 	.i_datab(A0), 	.o_res(a4_out));

assign y_D = a4_out; // y_D now holds the calculation result

// Combinational logic
always_comb begin
	// signal for enable
	enable = i_ready;
end

// Infer the registers
always_ff @(posedge clk or posedge reset) begin
	if (reset) begin
	//initialize the register to prevent garbage value interapt the operation
		valid_Q1 <= 1'b0;
		valid_Q2 <= 1'b0;
		valid_Q3 <= 1'b0;
		valid_Q4 <= 1'b0;
		valid_Q5 <= 1'b0;
		valid_Q6 <= 1'b0;
		valid_Q7 <= 1'b0;
		valid_Q8 <= 1'b0;
		valid_Q9 <= 1'b0;
		valid_Q10 <= 1'b0;
		valid_Q11 <= 1'b0;
		valid_Result <=1'b0;
		x <= 0;
		x0_in <=0;
		x1_in <=0;
		x2_in <=0;
		x3_in <=0;
		x4_in <=0;
		x5_in <=0;
		x6_in <=0;
		x7_in <=0;
		y_Q <= 0;
	end else if (enable) begin
		// propagate the valid value
		//need to adjust for pipeline structure
		//move the i_valid signal from the input side to the output 
		valid_Result <= valid_Q11;
		valid_Q11 <=valid_Q10;
		valid_Q10 <= valid_Q9;
		valid_Q9 <= valid_Q8;
		valid_Q8 <= valid_Q7;
		valid_Q7 <= valid_Q6;
		valid_Q6 <= valid_Q5;
		valid_Q5 <= valid_Q4;
		valid_Q4 <= valid_Q3;
		valid_Q3 <= valid_Q2;
		valid_Q2 <= valid_Q1;
		valid_Q1 <= i_valid;
		
		// read in new x value	
		
		x7_in <= x6_in;
		x6_in <= x5_in;
		x5_in <= x4_in;
		x4_in <= x3_in;
		x3_in <= x2_in;
		x2_in <= x1_in;
		x1_in <= x0_in;
		x0_in <= x;
		x <= i_x;
		y_Q <= y_D;
	end
end

// assign outputs
assign o_y = y_Q;
// ready for inputs as long as receiver is ready for outputs */
assign o_ready = i_ready;   		
// the output is valid as long as the corresponding input was valid and 
//	the receiver is ready. If the receiver isn't ready, the computed output
//	will still remain on the register outputs and the circuit will resume
//  normal operation with the receiver is ready again (i_ready is high)*/
// o_valid will be valid only when the compute x at the input is valid and is ready to take the next input
assign o_valid = valid_Result && o_ready;	 

endmodule

/*******************************************************************************************/

// Multiplier module for the first 16x16 multiplication
module mult16x16 (
	input clk,
	input i_ready,
	input  [15:0] i_dataa, 
	input  [15:0] i_datab,
	output [31:0] o_res
);

//temperate resigter to hold intermediate result
logic [31:0] result;


always_ff @(posedge clk) begin
// only compute when the clk is at positive edge
	if (i_ready) begin
		result <= i_dataa * i_datab;
	end
end

// The result of Q2.14 x Q2.14 is in the Q4.28 format. Therefore we need to change it
// to the Q7.25 format specified in the assignment by shifting right and padding with zeros.
assign o_res = {3'b000, result[31:3]};

endmodule

/*******************************************************************************************/

// Multiplier module for all the remaining 32x16 multiplications
module mult32x16 (
	input clk,
	input i_ready,
	input  [31:0] i_dataa,
	input  [15:0] i_datab,
	output [31:0] o_res
);

logic [47:0] result;
//logic [31:0] prev_input;
always_ff @(posedge clk) begin
// only compute when the clk is at positive edge
	if(i_ready) begin
		result <= i_dataa * i_datab;
	end	
end

// The result of Q7.25 x Q2.14 is in the Q9.39 format. Therefore we need to change it
// to the Q7.25 format specified in the assignment by selecting the appropriate bits
// (i.e. dropping the most-significant 2 bits and least-significant 14 bits).
assign o_res = result[45:14];

endmodule

/*******************************************************************************************/

// Adder module for all the 32b+16b addition operations 
module addr32p16 (
	input clk,
	input i_ready,
	input [31:0] i_dataa,
	input [15:0] i_datab,
	output [31:0] o_res
);
logic [31:0] result;
always_ff @(posedge clk) begin
	if (i_ready) begin
// only compute when the clk is at positive edge
//need to match with Q7.24 format
		result <= i_dataa + {5'b00000, i_datab, 11'b00000000000};
	end 

	
end
assign o_res = result;
endmodule

/*******************************************************************************************/
