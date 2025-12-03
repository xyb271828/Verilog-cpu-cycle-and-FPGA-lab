
`define PROGRAM_ENTRY_POINT	16'h0010

`define ENABLE_CPU_MONITORING
`define ENABLE_MONITOR_FILE
`define ENABLE_UART_MONITORING

/// simulation stops at these values
/// modify this if your program runs longer than this...
`define MAX_CYCLE       32'hb00000
`define MAX_INSN_COUNT  32'h400000

/*********************** Input files *************************/

`define PROGRAM_NAME        "test1"
`define MEM_INIT_FILE       {`PROGRAM_NAME, ".mem"}
`define PROB_FILE           {`PROGRAM_NAME, ".prb"}
`define MONITOR_FILE        {`PROGRAM_NAME, ".mon"}
`define INPUT_VECTOR_FILE   {`PROGRAM_NAME, "_in.log"}
`define OUTPUT_VECTOR_FILE  {`PROGRAM_NAME, "_out.log"}

`define MAX_FILENAME_LENGTH 20

`define INPUT_VECTOR_SIZE  256
`define OUTPUT_VECTOR_SIZE 1024 /// 256

`define MONITOR_VECTOR_SIZE  256

/*************************************************************/

/*********************** CPU control *************************/
`define COM_RST 2'b00
`define COM_RUN 2'b01
`define COM_STP 2'b10

/*********************** memory probe *************************/
`define MEM_DATA  4'h0
`define MEM_END   4'hf

/*********************** 7-segment LED ************************
7-segment LED bit assignment :
      00000
     5     1
     5     1
     5     1
      66666
     4     2
     4     2
     4     2
      33333
***************************************************************/
`define SEG_7_0	7'h3f			///	 543210 : 0
`define SEG_7_1 7'h06			///	    21  : 1
`define SEG_7_2 7'h5b			///	6 43 10 : 2
`define SEG_7_3 7'h4f			///	6  3210 : 3
`define SEG_7_4 7'h66			///	65  21  : 4
`define SEG_7_5 7'h6d			///	65 32 0 : 5
`define SEG_7_6 7'h7d			///	65432 0 : 6
`define SEG_7_7 7'h07			///	    210 : 7
`define SEG_7_8 7'h7f			///	6543210 : 8
`define SEG_7_9 7'h6f			///	65 3210 : 9
`define SEG_7_A	7'h77			///	654 210 : A
`define SEG_7_B 7'h7c			///	65432   : b
`define SEG_7_C 7'h39			///	 543  0 : C
`define SEG_7_D 7'h5e			///	6 4321  : d
`define SEG_7_E 7'h79			///	6543  0 : E
`define SEG_7_F 7'h71			///	654   0 : F
`define SEG_7_UNDEFINED	7'h40	///	6       : -

/*********************** FPGA SW options *************************/
////////////       SW[9:8] : system GPIO mode       //////////////
`define G0  2'b00   /// disable GPIO
`define G1  2'b01   /// enable GPIO (GPIO[35:18] = GP_OUT[17:0], GPIO[17:0] = GP_IN[17:0])
`define G2  2'b10   /// enable GPIO (GPIO[35:18] = GP_IN[17:0],  GPIO[17:0] = GP_OUT[17:0])
////////////       SW[7:6] : system operation mode  //////////////
`define M0  2'b00   /// run continuous
`define M1  2'b01   /// run until interrupt
`define M2  2'b10   /// step single clock
`define M3  2'b11   /// step single insn
////////////       SW[5:2] : system probe mode       //////////////
`define P0  4'b0000 /// probe ac[15:0]
`define P1  4'b0001 /// probe e[0]
`define P2  4'b0010 /// probe pc[11:0]
`define P3  4'b0011 /// probe ir[15:0]
`define P4  4'b0100 /// probe ar[11:0]
`define P5  4'b0101 /// probe dr[15:0]
`define P6  4'b0110 /// probe mem_dout[15:0]
`define P7  4'b0111 /// probe bus[15:0]
`define P8  4'b1000 /// probe sc[3:0]
`define P9  4'b1001 /// probe {iosel, ien, imask[1:0]}
`define P10 4'b1010 /// probe {fgi, inpr[7:0]}
`define P11 4'b1011 /// probe {fgo, outr[7:0]}

/*********************** UART parity options *************************/
`define UART_PARITY_NONE      3'b000
`define UART_PARITY_ODD       3'b001   /// XNOR all bits (1 if even number of 1s)
`define UART_PARITY_EVEN      3'b011   /// XOR  all bits (1 if odd number of 1s)
`define UART_PARITY_STICK_1   3'b101   /// 1
`define UART_PARITY_STICK_0   3'b111   /// 0

`define UART_PARITY_DEFAULT	  `UART_PARITY_EVEN  /// change this to switch to other parity types
`define UART_PARITY_ON        (`UART_PARITY_DEFAULT != `UART_PARITY_NONE)
`define UART_BIT_COUNT        (1 + 8 + `UART_PARITY_ON + 1) /// start-bit, 8-bit data (LSB-first!), parity-bit, end-bit

`define GET_CHAR(ch)          ((ch != 8'h0a) ? { 8'h20, ch } : { 8'h5c, 8'h6e })