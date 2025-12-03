
`include "def_ex3.v"

module cpu_ex3 (clk, com_ctl, com_addr,
            fgi_bsy, fgi, inpr_in, /// input ports
            fgo_bsy, fgo, outr,    /// output ports
            ien, imsk, iot,
            s, r, e, bus_data, mem_data, dr, ac,
            ir, ar, pc, sc, sc_clr);

    input         clk;
    input  [1:0]  com_ctl;
    input  [11:0] com_addr;

    input  [1:0]  fgi_bsy;
    output [1:0]  fgi;
    input  [7:0]  inpr_in;

    input  [1:0]  fgo_bsy;
    output [1:0]  fgo;
    output [7:0]  outr;

    output        ien, iot;
    output [3:0]  imsk;

    output        s, r, e;

    output [15:0] bus_data, mem_data, dr, ac, ir;
    output [11:0] ar, pc;
    output [2:0]  sc;
    output        sc_clr;

    wire          com_rst    = (com_ctl == `COM_RST); /// reset (pc, sc, ar, 1-bit FFs)
    wire          com_start  = (com_ctl == `COM_RUN); /// release all resets
    wire          com_stop   = (com_ctl == `COM_STP); /// stop (freeze all)

    wire   [2:0]  bus_ctl;
    wire   [15:0] ac_nxt;

    wire          e_nxt, i15, r_nxt, ien_nxt, s_nxt, iot_nxt;
    wire   [3:0]  imsk_nxt;
    wire   [1:0]  fgi_nxt, fgo_nxt, fgi_set, fgo_set;
	wire   [7:0]  inpr;

    wire          ar_ld, ar_inr, ar_clr;
    wire          pc_ld, pc_inr, pc_clr;
    wire          dr_ld, dr_inr, dr_clr;
    wire          ac_ld, ac_inr, ac_clr;
    wire          ac_and, ac_add, ac_dr, ac_inpr, ac_cmp, ac_shr, ac_shl;
    wire          e_clr, e_cmp;

    wire          mem_we;

    wire          ir_ld;
    wire          outr_ld;

    wire [11:0]   ar_nxt;    /// ar_nxt is the value of ar at next clock (needed for synchronous RAM)
    wire [11:0]   mem_addr = (com_stop) ? com_addr : (mem_we) ? ar : ar_nxt;
    wire [15:0]   mem_din  = bus_data;

    ram_sync_4kx16 MEM   (clk, mem_we & ~com_stop, mem_addr, mem_din, mem_data);

    reg_lci_nxt #12 AR   (clk, ~com_stop, bus_data[11:0], ar, ar_nxt, ar_ld, ar_clr, ar_inr);
    reg_lci     #12 PC   (clk, ~com_stop, bus_data[11:0], pc, pc_ld | com_rst, pc_clr, pc_inr);

    reg_lci     #16 DR   (clk, ~com_stop, bus_data, dr, dr_ld, dr_clr, dr_inr);
    reg_lci     #16 AC   (clk, ~com_stop, ac_nxt,   ac, ac_ld, ac_clr, ac_inr);
    reg_lci     #16 IR   (clk, ~com_stop, bus_data, ir, ir_ld, 1'b0,   1'b0);

    reg_lci     #8  OUTR (clk, ~com_stop, bus_data[7:0], outr, outr_ld, com_rst, 1'b0);
    reg_dff     #8  INPR (clk, ~com_stop, inpr_in, inpr);

    reg_lci     #3  SC   (clk, ~com_stop, 3'b0, sc, 1'b0, sc_clr | com_rst, ~sc_clr); /// if(sc_clr == 0) sc ++;

    reg_dff     #1  I    (clk, ~com_stop, ir[15]                   , i15);
    reg_dff     #1  E    (clk, ~com_stop, e_nxt & ~com_rst         , e);    /// reset value = 0;
    reg_dff     #1  R    (clk, ~com_stop, r_nxt & ~com_rst         , r);    /// reset value = 0;
    reg_dff     #1  S    (clk, ~com_stop, s_nxt | com_rst          , s);    /// reset value = 1;
    reg_dff     #1  IEN  (clk, ~com_stop, ien_nxt & ~com_rst       , ien);  /// reset value = 0;
    reg_dff     #2  FGI  (clk, ~com_stop, fgi_nxt & {2{~com_rst}}  , fgi);  /// reset value = 00;
    reg_dff     #2  FGO  (clk, ~com_stop, fgo_nxt | {2{com_rst}}   , fgo);  /// reset value = 11;
    reg_dff     #1  IOT  (clk, ~com_stop, iot_nxt & ~com_rst       , iot);  /// reset value = 0;
    reg_dff     #4  IMSK (clk, ~com_stop, imsk_nxt & {4{~com_rst}} , imsk); /// reset value = 0000;

	edge_to_pulse #(4,0) FGP (clk, {fgi_bsy, fgo_bsy}, {fgi_set, fgo_set});

    alu    ALU (dr, inpr, ac, e, ac_nxt, e_nxt, ac_and, ac_add, ac_dr, ac_inpr, ac_cmp, ac_shr, ac_shl, e_clr, e_cmp);

    bus    BUS (bus_ctl, `PROGRAM_ENTRY_POINT, {4'b0, ar}, {4'b0, pc}, dr, ac, ir, 16'b0, mem_data, bus_data);

    wire  [7:0] t;
    wire  [7:0] d;

    dec_3to8  DEC_T  (sc, t, 1'b1);         /// (t[k] == 1) implies sc = k;
    dec_3to8  DEC_D  (ir[14:12], d, 1'b1);  /// (d[k] == 1) implies ir[14:12] = k;
                                            /// (d[7] == 1) implies non-memory-insn type

    wire        rt = d[7] & ~i15 & t[3]; /// @ t[3] : implies register-insn type
    wire        pt = d[7] &  i15 & t[3]; /// @ t[3] : implies IO register-insn type

/// interrupt cycle:
/// r & t[0]  : ar <- 0;
/// r & t[1]  : mem[ar] <- pc; pc <- 0;
/// r & t[2]  : pc <- pc + 1; r <- 0; ien <- 0; sc <- 0;

/// instruction fetch cycle:
/// ~r & t[0] : ar <- pc;
/// ~r & t[1] : ir <- mem[ar]; pc <- pc + 1;
/// ~r & t[2] : ir15 <- ir[15]; ar <- ir[11:0];

/// indirect memory access:
/// ~d[7] & i15 & t[3] : ar <- mem[ar];

/// memory instruction cycle:
/// AND :   d[0] & t[4] : dr <- mem[ar];
///         d[0] & t[5] : ac <- ac & dr; sc <- 0;
/// ADD :   d[1] & t[4] : dr <- mem[ar];
///         d[1] & t[5] : ac <- ac + dr; sc <- 0;
/// LDA :   d[2] & t[4] : dr <- mem[ar];
///         d[2] & t[5] : ac <- dr;	     sc <- 0;
/// STA :   d[3] & t[4] : mem[ar] <- ac; sc <- 0;
/// BUN :   d[4] & t[4] : pc <- ar;      sc <- 0;
/// BSA :   d[5] & t[4] : mem[ar] <- pc; ar <- ar + 1;
///         d[5] & t[5] : pc <- ar;      sc <- 0;
/// ISZ :   d[6] & t[4] : dr <- mem[ar];
///         d[6] & t[5] : dr <- dr + 1;
///         d[6] & t[6] : mem[ar] <- dr; (dr == 0) ? pc <- pc + 1; sc <- 0;

/// register instruction cycle:
/// rt = d[7] & ~i15 & t[3];		/// implies normal register-insn type
/// pt = d[7] &  i15 & t[3];		/// implies IO register-insn type

/// rt : sc <- 0;
/// pt : sc <- 0;

/// CLA :   rt & ir[11] : ac <- 0;
/// CLE :   rt & ir[10] :  e <- 0;
/// CMA :   rt & ir[9]  : ac <- ~ac;
/// CME :   rt & ir[8]  :  e <- ~e;
/// CIR :   rt & ir[7]  : ac[14:0] <- ac[15:1]; ac[15] <- e; e <- ac[0];
/// CIL :   rt & ir[6]  : ac[15:1] <- ac[14:0]; ac[0]  <- e; e <- ac[15];
/// INC :   rt & ir[5]  : ac <- ac + 1;
/// SPA :   rt & ir[4]  : (ac[15] == 0) ? pc <- pc + 1;
/// SNA :   rt & ir[3]  : (ac[15] == 1) ? pc <- pc + 1;
/// SZA :   rt & ir[2]  : (ac == 0)     ? pc <- pc + 1;
/// SZE :   rt & ir[1]  : (e == 0)      ? pc <- pc + 1;
/// HLT :   rt & ir[0]  : s <- 0;

/// INP	:   pt & ir[11] : ac[7:0] <- inpr;
/// OUT	:   pt & ir[10] : outr <- ac[7:0];
/// SKI :   pt & ir[9]  : (fgi[iot] == 1) ? pc <- pc + 1;
/// SKO	:   pt & ir[8]  : (fgo[iot] == 1) ? pc <- pc + 1;
/// ION	:   pt & ir[7]  : ien <- 1;
/// IOF :   pt & ir[6]  : ien <- 0;

/// SIO :   pt & ir[5]  : iot <- 1;
/// PIO :   pt & ir[4]  : iot <- 0;
/// IMK :   pt & ir[3]  : imsk <- ac[3:0];

    assign mem_we  =    r & t[1] |   /// interrupt @ t[1] : mem[ar] <- pc;
                     d[3] & t[4] |   /// STA @ t[4]       : mem[ar] <- ac;
                     d[5] & t[4] |   /// BSA @ t[4]       : mem[ar] <- pc;
                     d[6] & t[6];    /// ISZ @ t[6]       : mem[ar] <- dr;

    assign ac_and  = d[0] & t[5];    /// AND @ t[5] : ac <- ac & dr;
    assign ac_add  = d[1] & t[5];    /// ADD @ t[5] : ac <- ac + dr;
    assign ac_dr   = d[2] & t[5];    /// LDA @ t[5] : ac <- dr;
    assign ac_inpr = pt & ir[11];    /// INP : ac[7:0] <- inpr;
    assign ac_cmp  = rt & ir[9];     /// CMA : ac <- ~ac;
    assign ac_shr  = rt & ir[7];     /// CIR : ac[14:0] <- ac[15:1], ac[15] <- e, e_nxt <- ac[0];
    assign ac_shl  = rt & ir[6];     /// CIL : ac[15:1] <- ac[14:0], ac[0]  <- e, e_nxt <- ac[15];

/// ac_ld : all of above operations
    assign ac_ld   = ac_and | ac_add | ac_dr | ac_inpr | ac_cmp | ac_shr | ac_shl;
    assign ac_inr  = rt & ir[5];     /// INC : ac <- ac + 1;
    assign ac_clr  = rt & ir[11];    /// CLA : ac <- 0;
	
    assign e_clr   = rt & ir[10];    /// CLE : e <- 0;
    assign e_cmp   = rt & ir[8];     /// CME : e <- ~e

    assign ar_ld   = ~r & t[0]          |   /// fetch @ t[0] : ar <- pc;
                     ~r & t[2]          |   /// memory address : ar <- ir[11:0];
                     ~d[7] & i15 & t[3];    /// indirect memory access : ar <- mem[ar];
    assign ar_inr  = d[5] & t[4];           /// BSA @ t[4] : mem[ar] <- pc, ar <- ar + 1;
    assign ar_clr  = r & t[0];              /// interrupt @ t[0] : ar <- 0;

    assign pc_ld   = d[4] & t[4] |                 /// BUN @ t[4] : pc <- ar;
                     d[5] & t[5];                  /// BSA @ t[5] : pc <- ar;
    assign pc_inr  = ~r & t[1]                  |  /// fetch @ t[1]
                     r & t[2]                   |  /// interrupt @ t[2]
                     d[6] & t[6] & (dr == 16'b0)|  /// ISZ @ t[6] & (dr == 0)
                     rt & ir[4] & ~ac[15]       |  /// SPA @ (ac[15] == 0)
                     rt & ir[3] & ac[15]        |  /// SNA @ (ac[15] == 1)
                     rt & ir[2] & (ac == 16'b0) |  /// SZA @ (ac == 0)
                     rt & ir[1] & ~e            |  /// SZE @ (e == 0)
                     pt & ir[9] & fgi[iot]      |  /// SKI @ (fgi[iot] == 1)
                     pt & ir[8] & fgo[iot];        /// SKO @ (fgo[iot] == 1)
    assign pc_clr  = r & t[1];                     /// interrupt @ t[1]

    assign dr_ld   = d[0] & t[4] |  /// AND @ t[4] : dr <- mem[ar];
                     d[1] & t[4] |  /// ADD @ t[4] : dr <- mem[ar];
                     d[2] & t[4] |  /// LDA @ t[4] : dr <- mem[ar];
                     d[6] & t[4];   /// ISZ @ t[4] : dr <- mem[ar];
    assign dr_inr  = d[6] & t[5];   /// ISZ @ t[5] : dr <- dr + 1;
    assign dr_clr  = 0;

    assign sc_clr  = r & t[2]    |                                            /// end of interrupt cycle
                     d[0] & t[5] | d[1] & t[5] | d[2] & t[5] | d[3] & t[4] |  /// end of AND/ADD/LDA/STA
                     d[4] & t[4] | d[5] & t[5] | d[6] & t[6] |                /// end of BUN/BSA/ISZ
                     rt | pt |                                                /// end of reg-insn/io-insn
                     ~s;                                                      /// halt

    assign ir_ld   = ~r & t[1];     /// fetch @ t[1]

    assign outr_ld = pt & ir[10]; /// OUT : outr <- ac[7:0]

    wire bus_ar    = d[4] & t[4]          |   /// BUN @ t[4] : pc <- ar;
                     d[5] & t[5];             /// BSA @ t[5] : pc <- ar;
    wire bus_pc    = ~r & t[0] & ~com_rst |   /// fetch @ t[0] : ar <- pc; (com_rst = 0)
                     r & t[1]             |   /// interrupt @ t[1] : mem[ar] <- pc;
                     d[5] & t[4];             /// BSA @ t[4] : mem[ar] <- pc;
    wire bus_dr    = d[6] & t[6];             /// ISZ @ t[6] : mem[ar] <- dr;
    wire bus_ac    = d[3] & t[4]          |   /// STA @ t[4] : mem[ar] <- ac;
                     pt & ir[10];             /// OUT : outr <- ac[7:0]
    wire bus_ir    = ~r & t[2];               /// fetch @ t[2] : ar <- ir[11:0];
    wire bus_mem   = ~r & t[1]            |   /// fetch @ t[1] : ir <- mem[ar];
                     ~d[7] & i15 & t[3]   |   /// indirect : ar <- mem[ar];
                     d[0] & t[4]          |   /// AND @ t[4] : dr <- mem[ar];
                     d[1] & t[4]          |   /// ADD @ t[4] : dr <- mem[ar];
                     d[2] & t[4]          |   /// LDA @ t[4] : dr <- mem[ar];
                     d[6] & t[4];             /// ISZ @ t[4] : dr <- mem[ar];

    assign bus_ctl[0]  = bus_ar | bus_dr | bus_ir | bus_mem;  /// b1 | b3 | b5 | b7
    assign bus_ctl[1]  = bus_pc | bus_dr | bus_mem;           /// b2 | b3 | b7
    assign bus_ctl[2]  = bus_ac | bus_ir | bus_mem;           /// b4 | b5 | b7

    wire   detect_intr = ~t[0] & ~t[1] & ~t[2] & ien & 
	                     ((fgi[1] & imsk[3]) | (fgo[1] & imsk[2]) |
                          (fgi[0] & imsk[1]) | (fgo[0] & imsk[0]));
    assign r_nxt   = (detect_intr) ? 1 :  /// detect interrupt
                     (r & t[2])    ? 0 :  /// disable interrupt
                     r;                   /// unchanged

    assign ien_nxt = (pt & ir[7]) ? 1                 :  /// ION                            : ien <- 1
                     ((pt & ir[6]) | (r & t[2])) ? 0  :  /// IOF | (end of interrupt cycle) : ien <- 0
                     ien;                                /// unchanged

    assign fgi_nxt[0] = (fgi_set[0]) ? 1         :  /// fgi_set[0] : fgi[0] <- 1
                        (pt & ir[11] & ~iot) ? 0 :  /// INP        : fgi[0] <- 0
                        fgi[0];                     /// unchanged
    assign fgi_nxt[1] = (fgi_set[1]) ? 1         :  /// fgi_set[1] : fgi[1] <- 1
                        (pt & ir[11] &  iot) ? 0 :  /// INP        : fgi[1] <- 0
                        fgi[1];                     /// unchanged

    assign fgo_nxt[0] = (fgo_set[0]) ? 1         :  /// fgo_set[0] : fgo[0] <- 1
                        (pt & ir[10] & ~iot) ? 0 :  /// OUT        : fgo[0] <- 0
                        fgo[0];                     /// unchanged
    assign fgo_nxt[1] = (fgo_set[1]) ? 1         :  /// fgo_set[1] : fgo[1] <- 1
                        (pt & ir[10] &  iot) ? 0 :  /// OUT        : fgo[1] <- 0
                        fgo[1];                     /// unchanged

    assign iot_nxt = (pt & ir[5]) ? 1 :             /// SIO : iot  <- 1
	                 (pt & ir[4]) ? 0 :             /// PIO : iot  <- 0
                     iot;                           /// unchanged

    assign imsk_nxt = (pt & ir[3]) ? ac[3:0] : imsk;  /// IMK : imsk <- ac[3:0]

    assign s_nxt = (rt & ir[0]) ? 0 : s;            /// HLT : s <- 0;

endmodule

