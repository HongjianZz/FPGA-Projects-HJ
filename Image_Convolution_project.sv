// This module implements 2D covolution between a 3x3 filter and a 512-pixel-wide image of any height.
// It is assumed that the input image is padded with zeros such that the input and output images have
// the same size. The filter coefficients are symmetric in the x-direction (i.e. f[0][0] = f[0][2], 
// f[1][0] = f[1][2], f[2][0] = f[2][2] for any filter f) and their values are limited to integers
// (but can still be positive of negative). The input image is grayscale with 8-bit pixel values ranging
// from 0 (black) to 255 (white).
module lab2 (
	input  clk,			// Operating clock
	input  reset,			// Active-high reset signal (reset when set to 1)
	input  [71:0] i_f,		// Nine 8-bit signed convolution filter coefficients in row-major format (i.e. i_f[7:0] is f[0][0], i_f[15:8] is f[0][1], etc.)
	
	// Potential improvement! start program @ f[3][2], because of symmetric
	
	input  i_valid,			// Set to 1 if input pixel is valid
	input  i_ready,			// Set to 1 if consumer block is ready to receive a new pixel
	
	input  [7:0] i_x,		// Input pixel value (8-bit unsigned value between 0 and 255)
	output o_valid,			// Set to 1 if output pixel is valid
	output o_ready,			// Set to 1 if this block is ready to receive a new pixel
	output [7:0] o_y		// Output pixel value (8-bit unsigned value between 0 and 255)
);

localparam FILTER_SIZE = 3;	// Convolution filter dimension (i.e. 3x3)
localparam PIXEL_DATAW = 8;	// Bit width of image pixels and filter coefficients (i.e. 8 bits)
localparam PIXEL_WIDTH = 514;
localparam INPUT_REG_ROW = 3;
localparam INPUT_REG_COL = 514;


wire signed [21:0] o_output;// intermediate step of output
logic signed [PIXEL_DATAW-1:0] i_xin [0:2][0: PIXEL_WIDTH-1]; // restore 3 rows of as the buffer


// The following code is intended to show you an example of how to use paramaters and
// for loops in SytemVerilog. It also arrages the input filter coefficients for you
// into a nicely-arranged and easy-to-use 2D array of registers. However, you can ignore
// this code and not use it if you wish to.
logic r_valid;
logic next_ready;// valid to receive next pixel

logic signed[PIXEL_DATAW-1:0] r_f [0:FILTER_SIZE-1][0:FILTER_SIZE-1]; // 2D array of registers for filter coefficients
logic unsigned[PIXEL_DATAW-1:0] r_i [0:FILTER_SIZE-1][0:FILTER_SIZE-1];// 2D array of registers for input piexels
integer col, row; // variables to use in the for loop
integer col_in, row_in;// integer pointer for the 3x514 array, for reset
integer col_dynamic = 0; // dynamic pointer for insert input to i_xin
integer row_dynamic = 0;
integer buf_count = 0; // 1 when the buf is full
integer sel_row = 1; // initial position for center of selected input pixel
integer sel_prev = 0; // initial position for upper row of selected input pixel
integer sel_next = 2; // initial position for bottom of row of selected input pixel
integer sel_col = 1;//dynamic pointer for the center of 3x3 pixel frame

logic unsigned[7:0] out_put; //temperate register 

wire signed [16:0] m_result0; //temperate register to retore product of multiplications
wire signed [16:0] m_result1;
wire signed [16:0] m_result2;
wire signed [16:0] m_result3;
wire signed [16:0] m_result4;
wire signed [16:0] m_result5;
wire signed [16:0] m_result6;
wire signed [16:0] m_result7;
wire signed [16:0] m_result8;



multiply multi0(.clk, .i_ready,	.pix_x(r_i[0][0]), .coef_f(r_f[0][0]), .pix_conv(m_result0));
multiply multi1(.clk, .i_ready,	.pix_x(r_i[0][1]), .coef_f(r_f[0][1]), .pix_conv(m_result1));
multiply multi2(.clk, .i_ready,	.pix_x(r_i[0][2]), .coef_f(r_f[0][2]), .pix_conv(m_result2));
multiply multi3(.clk, .i_ready,	.pix_x(r_i[1][0]), .coef_f(r_f[1][0]), .pix_conv(m_result3));
multiply multi4(.clk, .i_ready,	.pix_x(r_i[1][1]), .coef_f(r_f[1][1]), .pix_conv(m_result4));
multiply multi5(.clk, .i_ready,	.pix_x(r_i[1][2]), .coef_f(r_f[1][2]), .pix_conv(m_result5));
multiply multi6(.clk, .i_ready,	.pix_x(r_i[2][0]), .coef_f(r_f[2][0]), .pix_conv(m_result6));
multiply multi7(.clk, .i_ready,	.pix_x(r_i[2][1]), .coef_f(r_f[2][1]), .pix_conv(m_result7));
multiply multi8(.clk, .i_ready,	.pix_x(r_i[2][2]), .coef_f(r_f[2][2]), .pix_conv(m_result8));

add add0(.clk, .i_ready, .pix_conv0(m_result0), .pix_conv1(m_result1), .pix_conv2(m_result2), .pix_conv3(m_result3),
			.pix_conv4(m_result4), .pix_conv5(m_result5), .pix_conv6(m_result6), .pix_conv7(m_result7), .pix_conv8(m_result8), .pix_out(o_output));

valid_o valido(.clk, .i_ready, .out_in(o_output), .out_o(out_put), .result_valid(r_valid));

always_ff @ (posedge clk) begin
	// If reset signal is high, set all the filter coefficient registers to zeros
	// We're using a synchronous reset, which is recommended style for recent FPGA architectures
	if(reset)begin
		for(row = 0; row < FILTER_SIZE; row = row + 1) begin
			for(col = 0; col < FILTER_SIZE; col = col + 1) begin
				r_f[row][col] <= 0;
			end
		end
	// Otherwise, register the input filter coefficients into the 2D array signal
	end else begin
		for(row = 0; row < FILTER_SIZE; row = row + 1) begin
			for(col = 0; col < FILTER_SIZE; col = col + 1) begin
				// Rearrange the 72-bit input into a 3x3 array of 8-bit filter coefficients.
				// signal[a +: b] is equivalent to signal[a+b-1 : a]. You can try to plug in
				// values for col and row from 0 to 2, to understand how it operates.
				// For example at row=0 and col=0: r_f[0][0] = i_f[0+:8] = i_f[7:0]
				//	       at row=0 and col=1: r_f[0][1] = i_f[8+:8] = i_f[15:8]
				r_f[row][col] <= i_f[(row * FILTER_SIZE * PIXEL_DATAW)+(col * PIXEL_DATAW) +: PIXEL_DATAW];
			end
		end
	end
end
// input of filter coefficient done


// Start of your code
// restore the input into a buffer, then every when the buffer is ful, replace the new i_x to the first
// inserted element.

always_ff @ (posedge clk) begin
	
	if(reset)begin
	// initialize the register
		next_ready <= 1'b1;
		/*
		for (row_in = 0; row_in < INPUT_REG_ROW; row_in = row_in +1) begin
			for (col_in = 0; col_in < INPUT_REG_COL; col_in = col_in+1) begin
				i_xin[row_in][col_in] <= 8'b0;
			end
		end
		*/
	// case when the input is valid, insert valid inputs to the i_xin array	
	// this ensure that all value in r_i is valid
	end else if (i_valid) begin
	
		i_xin[row_dynamic][col_dynamic] <= i_x; // insert new input i_x to the array buffer
		
		if (col_dynamic == 513 && row_dynamic < 2) begin // case when a row is full
			row_dynamic <= row_dynamic + 1; // jump to the next row
			col_dynamic <= 0;
		end else if (col_dynamic == 513 && row_dynamic == 2) begin //overwirte the first row
			row_dynamic <= 0;
			col_dynamic <= 0; 
			//buf_count <= 1; // buf is full, now can start conv
		end else begin
			col_dynamic <= col_dynamic + 1;
			if (col_dynamic == 6 && row_dynamic == 2) begin
				buf_count <= 1;
			end
		end
	// computation process, if i_ready, then compute, false then pause	
	end 
	if (i_ready) begin
		if(buf_count == 1 && (sel_col != 0 && sel_col != 513)) begin // the buf has enough pixel to do convolution
			
			r_i[0][0] <= i_xin[sel_prev][sel_col-1];
			r_i[0][1] <= i_xin[sel_prev][sel_col];
			r_i[0][2] <= i_xin[sel_prev][sel_col+1];
			r_i[1][0] <= i_xin[sel_row][sel_col-1];
			r_i[1][1] <= i_xin[sel_row][sel_col];
			r_i[1][2] <= i_xin[sel_row][sel_col+1];
			r_i[2][0] <= i_xin[sel_next][sel_col-1];
			r_i[2][1] <= i_xin[sel_next][sel_col];
			r_i[2][2] <= i_xin[sel_next][sel_col+1];
			sel_col <= sel_col + 1;

		end else if (buf_count == 1 && (sel_col == 0 || sel_col == 513)) begin
			// if buf is full and compute the padding zero
			// output is invalid
			r_i[0][0] <= i_xin[sel_prev][sel_col-1];
			r_i[0][1] <= i_xin[sel_prev][sel_col];
			r_i[0][2] <= i_xin[sel_prev][sel_col+1];
			r_i[1][0] <= i_xin[sel_row][sel_col-1];
			r_i[1][1] <= i_xin[sel_row][sel_col];
			r_i[1][2] <= i_xin[sel_row][sel_col+1];
			r_i[2][0] <= i_xin[sel_next][sel_col-1];
			r_i[2][1] <= i_xin[sel_next][sel_col];
			r_i[2][2] <= i_xin[sel_next][sel_col+1];
			if(sel_col == 513 && sel_row == 1)begin
				sel_col <= 0;
				sel_row <= 2;
				sel_next <= 2;
				sel_prev <= 2;
			end else if(sel_col == 0 && sel_row == 1)begin
				sel_col <= 1;
				sel_row <= 1;
				sel_next <= 2;
				sel_prev <= 0;
			end else if (sel_col == 513 && sel_row == 2)begin
				sel_col <= 0;
				sel_row <= 0;
				sel_prev <= 0;
				sel_next <= 0;
			end else if (sel_col == 0 && sel_row == 2) begin
				sel_col <= 1;
				sel_row <= 2;
				sel_prev <= 1;
				sel_next <= 0;
			end else if (sel_col == 513 && sel_row == 0) begin
				sel_col <= 0;
				sel_row <= 1;
				sel_prev <= 1;
				sel_next <= 1;
			end else if (sel_col == 0 && sel_row == 0) begin
				sel_col <= 1;
				sel_row <= 0;
				sel_prev <= 2;
				sel_next <= 1;
			end
		end
	end	
end
assign o_ready = next_ready & i_ready; // o_ready if the consumer is ready to take output and itself is reset
assign o_valid = r_valid & i_ready;
assign o_y = out_put;
// End of your code

endmodule

// 

// add module, take input of clk, and i_ready and all the product from multipliers
// output a signed 22 bit output

module add(
	input clk,
	input i_ready,
	input signed [16:0] pix_conv0,
	input signed [16:0] pix_conv1,
	input signed [16:0] pix_conv2,
	input signed [16:0] pix_conv3,
	input signed [16:0] pix_conv4,
	input signed [16:0] pix_conv5,
	input signed [16:0] pix_conv6,
	input signed [16:0] pix_conv7,
	input signed [16:0] pix_conv8,
	output signed [21:0] pix_out
	);
logic signed [21:0] result_add;
logic conv_finish = 1;


always_ff @ (posedge clk) begin
	if (i_ready) begin
		result_add <= pix_conv0 + pix_conv1 + pix_conv2 + pix_conv3 + pix_conv4 + pix_conv5 + pix_conv6 + pix_conv7 + pix_conv8;
	end
	
end

assign pix_out = result_add;
assign conv_status = conv_finish;
endmodule

//multiplier, takes 1 signed filter coefficient and 1 unsigned pixel

module multiply(
	input clk,
	input i_ready,
	input unsigned [7:0] pix_x,
	input signed [7:0] coef_f,
	output signed [16:0] pix_conv
	)/* synthesis multstyle = "logic" */;

logic signed [16:0] result_mult;
logic signed [8:0] sign_pix;

assign sign_pix = {1'b0,pix_x};

always_ff @ (posedge clk) begin
	if (i_ready) begin
		result_mult <= sign_pix * coef_f;
	end
end

assign pix_conv = result_mult;
endmodule


//convert the 22 bit signed to 0-255 unsigned value
module valid_o(
	input clk,
	input i_ready,
	input signed [21:0] out_in,
	output [7:0] out_o,
	output result_valid
	);
logic signed [21:0] temp;
logic result_is_valid;

always_ff @ (posedge clk) begin
	if (i_ready) begin
		if (out_in) begin
			result_is_valid <= 1;
			if (out_in < 0) begin
				temp <= 22'b0;
			end else if (out_in > 255) begin
				temp <= 22'd255;
			end else begin
				temp <= out_in;
			end
		end else if (out_in == 0) begin
			temp <= out_in;
			result_is_valid <= 1;
		
		end else begin
			result_is_valid <= 0;
			temp <= 13;
		end
	end
end
//check if the output is valid number of x, if x not valid, o_valid is 
assign out_o = temp [7:0];
assign result_valid = result_is_valid;
endmodule
