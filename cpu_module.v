
`include "def_ex3.v"


/*********************** synchronous ROM model *************************/

module rom_sync_4kx32 (clk, addr, dout);

/// mem_size = 4096;
/// addr_width = 12;
/// data_width = 32;

	input  clk;         ///	clock
	input  [11:0] addr; /// address input
	output [31:0] dout; /// data output

	reg  [31:0] dout, mem[0:4095];

	initial $readmemh(`PROB_FILE, mem);

	always @ (posedge clk) begin
		dout <= mem[addr];
	end
endmodule

/*********************** synchronous RAM model *************************/

module ram_sync_4kx16 (clk, we, addr, din, dout);

/// mem_size = 4096;
/// addr_width = 12;
/// data_width = 16;

    input  clk;         /// clock
    input  we;          /// write enable
    input  [11:0] addr; /// address input
    input  [15:0] din;  /// data input
    output [15:0] dout; /// data output

    reg    [15:0] dout, mem[0:4095];

    initial $readmemh(`MEM_INIT_FILE, mem);

    always @ (posedge clk) begin
        if(we) mem[addr] <= din;
        dout <= mem[addr];
    end
endmodule


/*********************** load/clear/increment/clock-enable register model *************************/

module reg_lci (clk, en, din, dout, ld, clr, inr);

parameter DATA_WIDTH = 16;

    input clk;          /// clock
    input en;           /// clock enable
    input ld;           /// load
    input clr;          /// clear
    input inr;          /// increment
    input  [DATA_WIDTH - 1:0] din;   /// data input
    output [DATA_WIDTH - 1:0] dout;  /// data output

    wire   [DATA_WIDTH - 1:0] dout_nxt; /// not connected to output

    reg_lci_nxt #(DATA_WIDTH) R0 (clk, en, din, dout, dout_nxt, ld, clr, inr);

endmodule

/// reg_lci_nxt has same functionality as reg_lci, but also outputs dout_nxt (dout at next clock)

module reg_lci_nxt (clk, en, din, dout, dout_nxt, ld, clr, inr);

parameter DATA_WIDTH = 16;

    input clk;          /// clock
    input en;           /// clock enable
    input ld;           /// load
    input clr;          /// clear
    input inr;          /// increment
    input  [DATA_WIDTH - 1:0] din;       /// data input
    output [DATA_WIDTH - 1:0] dout;      /// data output
    output [DATA_WIDTH - 1:0] dout_nxt;  /// data output at the next cycle

    assign dout_nxt = (clr) ? 0 : (ld) ? din : (inr) ? dout + 1 : dout;

    reg_dff #(DATA_WIDTH) R0 (clk, en, dout_nxt, dout);
endmodule

/*********************** clock-enable register model *************************/

module reg_dff (clk, en, din, dout);

parameter DATA_WIDTH = 16;

    input clk;                      /// clock
    input en;                       /// enable
    input  [DATA_WIDTH - 1:0] din;  /// data input
    output [DATA_WIDTH - 1:0] dout; /// data output

    reg    [DATA_WIDTH - 1:0] dout;

    always @ (posedge clk)
    	if(en) dout <= din;         /// update dout only when (en == 1)
endmodule

/*********************** edge-to-pulse converter *************************/

module edge_to_pulse (clk, din, dout);

    parameter DATA_WIDTH = 1;
	parameter EDGE_TYPE  = 0;  /// 0 : negative-edge (1->0), 1 : positive-edge (0->1)

    input                        clk;
	input   [DATA_WIDTH - 1:0]   din;
    output  [DATA_WIDTH - 1:0]   dout;

    reg     [DATA_WIDTH - 1:0]   prev_din2;

	wire    [DATA_WIDTH - 1:0]   din2 = (EDGE_TYPE == 1) ? din : ~din;
    always @ (posedge clk) prev_din2 <= din2;

    assign dout = ~prev_din2 & din2;

endmodule

/*********************** 8-master bus model *************************/

module bus (bus_ctl, b0, b1, b2, b3, b4, b5, b6, b7, bout);

parameter DATA_WIDTH = 16;

    input  [2:0]  bus_ctl;
    input  [DATA_WIDTH - 1:0] b0, b1, b2, b3, b4, b5, b6, b7;
    output [DATA_WIDTH - 1:0] bout;

    reg    [DATA_WIDTH - 1:0] bout; /// bout is reg-type but is actually combinational

    always @ (bus_ctl or b0 or b1 or b2 or b3 or b4 or b5 or b6 or b7) begin
    	case (bus_ctl)
            3'b000 : bout = b0;
            3'b001 : bout = b1;
            3'b010 : bout = b2;
            3'b011 : bout = b3;
            3'b100 : bout = b4;
            3'b101 : bout = b5;
            3'b110 : bout = b6;
            3'b111 : bout = b7;
        endcase
    end
endmodule

/*********************** ALU model *************************/

module alu (dr, inpr, ac, e, ac_nxt, e_nxt, ac_and, ac_add, ac_dr, ac_inpr, ac_cmp, ac_shr, ac_shl, e_clr, e_cmp);

    input  [15:0] dr, ac;
    input  [7:0]  inpr;
    input         e, ac_and, ac_add, ac_dr, ac_inpr, ac_cmp, ac_shr, ac_shl, e_clr, e_cmp;

    output [15:0] ac_nxt;
    output        e_nxt;

    wire   [15:0] o_and  = (ac_and)  ? (dr & ac)                 : 16'b0;
    wire   [16:0] o_add  = (ac_add)  ? ({1'b0, dr} + {1'b0, ac}) : 17'b0;
    wire   [15:0] o_dr   = (ac_dr)   ? (dr)                      : 16'b0;
    wire   [15:0] o_inpr = (ac_inpr) ? ({ac[15:8], inpr})        : 16'b0;
    wire   [15:0] o_cmp  = (ac_cmp)  ? (~ac)                     : 16'b0;
    wire   [15:0] o_shr  = (ac_shr)  ? ({e, ac[15:1]})           : 16'b0;
    wire   [15:0] o_shl  = (ac_shl)  ? ({ac[14:0], e})           : 16'b0;

    assign ac_nxt = o_and | o_add[15:0] | o_dr | o_inpr | o_cmp | o_shr | o_shl;

    assign e_nxt =  (ac_add) ? o_add[16] :
                    (ac_shr) ? ac[0]     :
                    (ac_shl) ? ac[15]    :
                    (e_clr)  ? 1'b0      :
                    (e_cmp)  ? ~e        : e;
endmodule

/*********************** 2to4 decoder model *************************/

module dec_2to4 (din, dout, en);

    input  [1:0] din;
    input        en;
    output [3:0] dout;

    assign dout[0]  = en & (din == 2'b00);
    assign dout[1]  = en & (din == 2'b01);
    assign dout[2]  = en & (din == 2'b10);
    assign dout[3]  = en & (din == 2'b11);
endmodule

/*********************** 3to8 decoder model *************************/

module dec_3to8 (din, dout, en);

    input  [2:0] din;
    input        en;
    output [7:0] dout;

    dec_2to4 D0 (din[1:0], dout[3:0], en & ~din[2]);
    dec_2to4 D1 (din[1:0], dout[7:4], en & din[2]);
endmodule

/*********************** 4to16 decoder model *************************/

module dec_4to16 (din, dout, en);

    input  [3:0] din;
    input        en;
    output [15:0] dout;

    dec_3to8 D0 (din[2:0], dout[7:0], en & ~din[3]);
    dec_3to8 D1 (din[2:0], dout[15:8], en & din[3]);
endmodule

/*********************** 7-segment encoder/decoder *************************/

module seg7_enc (data, enc_out);
    input  [3:0] data;
    output [6:0] enc_out;
    reg    [6:0] enc_out_n;

    assign enc_out = ~enc_out_n;

    always @(data)
    case(data)
        4'h0:	 enc_out_n = `SEG_7_0;
        4'h1:	 enc_out_n = `SEG_7_1;
        4'h2:	 enc_out_n = `SEG_7_2;
        4'h3:	 enc_out_n = `SEG_7_3;
        4'h4:	 enc_out_n = `SEG_7_4;
        4'h5:	 enc_out_n = `SEG_7_5;
        4'h6:	 enc_out_n = `SEG_7_6;
        4'h7:	 enc_out_n = `SEG_7_7;
        4'h8:	 enc_out_n = `SEG_7_8;
        4'h9:	 enc_out_n = `SEG_7_9;
        4'ha:	 enc_out_n = `SEG_7_A;
        4'hb:	 enc_out_n = `SEG_7_B;
        4'hc:	 enc_out_n = `SEG_7_C;
        4'hd:	 enc_out_n = `SEG_7_D;
        4'he:	 enc_out_n = `SEG_7_E;
        4'hf:	 enc_out_n = `SEG_7_F;
        default: enc_out_n = `SEG_7_UNDEFINED;
    endcase
endmodule

module seg7_dec (hex, dec_out);
    input  [6:0] hex;
    output [4:0] dec_out;
    reg    [4:0] dec_out;

    always @(hex)
    case(~hex)
        `SEG_7_0 : dec_out = 5'h00;
        `SEG_7_1 : dec_out = 5'h01;
        `SEG_7_2 : dec_out = 5'h02;
        `SEG_7_3 : dec_out = 5'h03;
        `SEG_7_4 : dec_out = 5'h04;
        `SEG_7_5 : dec_out = 5'h05;
        `SEG_7_6 : dec_out = 5'h06;
        `SEG_7_7 : dec_out = 5'h07;
        `SEG_7_8 : dec_out = 5'h08;
        `SEG_7_9 : dec_out = 5'h09;
        `SEG_7_A : dec_out = 5'h0a;
        `SEG_7_B : dec_out = 5'h0b;
        `SEG_7_C : dec_out = 5'h0c;
        `SEG_7_D : dec_out = 5'h0d;
        `SEG_7_E : dec_out = 5'h0e;
        `SEG_7_F : dec_out = 5'h0f;
        default  : dec_out = 5'h1f;	///	error
    endcase
endmodule

/*********************** UART parameter ************************/

    /// CLK_FREQ = 27000000;               /// 27MHz
    /// BAUD_RATE = 9600;                  /// 9.6KHz

/*********************** UART parity generator *************************/

module uart_parity (xor_parity, parity_bit);

    input        xor_parity;
    output       parity_bit;

    assign parity_bit    = (`UART_PARITY_DEFAULT == `UART_PARITY_ODD) ? ~xor_parity :
                           (`UART_PARITY_DEFAULT == `UART_PARITY_EVEN) ? xor_parity :
                           (`UART_PARITY_DEFAULT == `UART_PARITY_STICK_1) ? 1       :
                           (`UART_PARITY_DEFAULT == `UART_PARITY_STICK_0) ? 0       :
						   1'bx;
endmodule

/*********************** UART RX model *************************/

module uart_rx (clk, reset, rx_din, rx_byte_out, rx_error, rx_rdy);

    parameter      ID0 = 0;
    wire[2:0]      ID = ID0;

    input          clk, reset;
    input          rx_din;      /// serial input
    output [7:0]   rx_byte_out;
    output         rx_error, rx_rdy;

    reg    [7:0]   rx_byte_out;
    reg            rx_error, rx_rdy;

    parameter HALF_BAUD_PHASE = 2;               /// 2 phases per 0.5 bit
    parameter ONE_BAUD_PHASE = HALF_BAUD_PHASE * 2;  /// 4 phases per 1 bit
    parameter BAUD_PERIOD = 703; /* 703.125 */   /// = CLK_FREQ / (BAUD_RATE * ONE_BAUD_PHASE)

    reg            rx_start;                     /// 0 : IDLE, 1 : RX_ACTIVE
    reg    [1:0]   rx_din_synced;                /// synchronized rx_din (using 2 DFFs)
    reg    [1:0]   baud_phase;                   /// 0 to ONE_BAUD_PHASE - 1
    reg    [`UART_BIT_COUNT - 1:0]  rx_shift;    /// start-bit, 8-bit data, parity-bit, end-bit
    reg    [9:0]   baud_tick;                    /// 0 to BAUD_PERIOD - 1
    reg    [3:0]   bit_count;                    /// 0 to BIT_COUNT - 1

    wire   rx_xor_parity = ^rx_byte_out;         /// XOR all bits in rx_byte_out[7:0]
    wire   rx_parity;
    uart_parity UP (rx_xor_parity, rx_parity);

    wire   rx_parity_error = `UART_PARITY_ON & (rx_parity != rx_shift[1]);
    wire   [7:0] rx_byte = rx_shift[`UART_BIT_COUNT - 3 : `UART_BIT_COUNT - 10];

    always@(posedge clk)
        ////////////// reset //////////////
        if(reset) begin
            rx_start      <= 0;
            rx_byte_out   <= 0;
            rx_rdy        <= 0;
            rx_din_synced <= 2'b11;
            rx_error      <= 0;
            baud_phase    <= 2'b0;
            rx_shift      <= 0;
            baud_tick     <= 10'b0;
            bit_count     <= 4'b0;
        end
        else begin
            rx_din_synced <= {rx_din, rx_din_synced[1]};  /// rx_din synchronization (right shift)
            if(~rx_start) begin
                if(~rx_din_synced[0]) begin  /// start bit = 0
                    rx_start <= 1;           /// start receiving data
                    rx_rdy <= 0;             /// now busy...
`ifdef ENABLE_UART_MONITORING
                    if(ID == 0) $display($stime, " : UART_RX[%d] (start!)", ID);
`endif
                end
            end
            ////////////// baud_tick : count until BAUD_PERIOD - 1 //////////////
            else if(baud_tick < BAUD_PERIOD - 1)
                baud_tick   <= baud_tick + 1;
            else begin  /// end of baud cycle
                baud_tick   <= 10'b0;
                ////////////// baud_phase : count until ONE_BAUD_PHASE - 1 //////////////
                if(baud_phase == HALF_BAUD_PHASE - 1) /// sample rx_din_synced[0] at middle of baud_phase
                    rx_shift <= {rx_din_synced[0], rx_shift[`UART_BIT_COUNT - 1 : 1]}; /// right shift, MSB = rx_din_synced[0]
                if(baud_phase < ONE_BAUD_PHASE - 1)
                    baud_phase <= baud_phase + 1;
                else begin  /// end of baud phase
                    baud_phase <= 2'b0;
                    ////////////// bit_count : count until UART_BIT_COUNT - 1 //////////////
                    if(bit_count < `UART_BIT_COUNT - 1)
                        bit_count <= bit_count + 1;
                    else begin /// all bits shifted
                        bit_count   <= 4'b0;
                        rx_rdy      <= 1;
                        rx_byte_out <= rx_byte;
                        rx_error    <= ~rx_shift[`UART_BIT_COUNT - 1] | rx_parity_error; /// end-bit must be 1
                        rx_start    <= 0; /// goto IDLE state
`ifdef ENABLE_UART_MONITORING
                        if(ID == 0)
                            $display($stime, " : UART_RX[%d] (stop!!) : rx_shift = %b_%b_%b (%h:%s)",
                                 ID, rx_shift[`UART_BIT_COUNT - 1:`UART_BIT_COUNT - 2],
                                 rx_byte, rx_shift[0], rx_byte, `GET_CHAR(rx_byte));
`endif
                    end
                end
            end
        end
    /// end always
endmodule

/*********************** UART TX model *************************/

module uart_tx (clk, reset, tx_dout, tx_byte_in, fgo, tx_rdy);

    parameter      ID0 = 0;
    wire[2:0]      ID = ID0;

    input          clk, reset;
    output         tx_dout;      /// serial input
    input [7:0]    tx_byte_in;
    input          fgo;
    output         tx_rdy;

    parameter BAUD_PERIOD = 2812; /* 2812.5 */   /// = CLK_FREQ / BAUD_RATE

    reg            tx_start;                     /// 0 : IDLE, 1 : TX_ACTIVE
    reg    [`UART_BIT_COUNT - 1:0]  tx_shift;    /// start-bit, 8-bit data, parity-bit, end-bit
    reg    [11:0]  baud_tick;                    /// 0 to BAUD_PERIOD - 1
    reg    [3:0]   bit_count;                    /// 0 to BIT_COUNT - 1

    assign tx_dout = tx_shift[0];
    assign tx_rdy = ~tx_start & ~reset;
    wire   tx_xor_parity = ^tx_byte_in;          /// XOR all bits in tx_byte_in[7:0]
    wire   tx_parity;
    uart_parity UP (tx_xor_parity, tx_parity);

	wire   tx_en;
	edge_to_pulse #(1,0) TX_EN (clk, fgo, tx_en);

    always@(posedge clk)
        ////////////// reset //////////////
        if(reset) begin
            tx_start      <= 0;
            tx_shift      <= {(`UART_BIT_COUNT){1'b1}}; /// all 1s
            baud_tick     <= 12'b0;
            bit_count     <= 4'b0;
        end
        else begin
            if(~tx_start) begin
                if(tx_en) begin
                    tx_start <= 1;
                    tx_shift <= {1'b1, tx_parity, tx_byte_in, 1'b0};
`ifdef ENABLE_UART_MONITORING
                    if(ID == 0) $display($stime, " : UART_TX[%d] (start!) : tx_shift = %b_%b_%b (%h:%s)",
                             ID, {1'b1, tx_parity}, tx_byte_in, 1'b0, tx_byte_in, `GET_CHAR(tx_byte_in));
`endif
                end
            end
            ////////////// baud_tick : count until BAUD_PERIOD - 1 //////////////
            else if(baud_tick < BAUD_PERIOD - 1)
                baud_tick   <= baud_tick + 1;
            else begin
                baud_tick   <= 10'b0;
                tx_shift <= {1'b1, tx_shift[`UART_BIT_COUNT - 1 : 1]}; /// right shift, MSB = 1
                ////////////// bit_count : count until UART_BIT_COUNT - 1 //////////////
                if(bit_count < `UART_BIT_COUNT - 1)
                    bit_count <= bit_count + 1;
                else begin /// all bits shifted
                    bit_count <= 4'b0;
                    tx_start <= 0; /// goto IDLE state
`ifdef ENABLE_UART_MONITORING
                    if(ID == 0) $display($stime, " : UART_TX[%d] (stop!!)", ID);
`endif
                end
            end
        end
    /// end always
endmodule

