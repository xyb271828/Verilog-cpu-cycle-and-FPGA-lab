	ORG 0		/ interrupt entry point
ST0,HEX 0		/ interrupt return address
	BUN I_HND	/ goto I_HND (interrupt handler)

	ORG 10		/ program entry point
INI, / initialize data
	CLA			/ AC <- 0
	STA STT		/ M[STT] <- 0
	STA SFG		/ M[SFG] <- 0
/ IMSK[3:0] : { S_IN, S_OUT, P_IN, P_OUT }
	LDA VH8		/ AC <- M[VH8] (1000)
	IMK			/ IMSK <- (1000) (S_IN enabled)
	SIO			/ IOT <- 1 (serial-IO selected)
	ION			/ enable interrupt
L0, LDA STT		/ AC <- M[STT]
	SNA			/ (M[STT] < 0) ? skip next
	BUN L0
	HLT
/////////// subroutine (check end-character) ////////
CEC,HEX 0
/ arg0 (AC) : output character
/ end-character = 0x4 (ctrl-D)
	ADD VM4		/ AC <- AC - 4
	SZA			/ (AC == 0) ? skip next
	BUN CEC I	/ return from CEC
/ output character matches (ctrl-D)
	LDA VM1		/ AC <- -1
	STA STT		/ M[STT] <- -1
	CLA			/ AC <- 0
	IMK			/ IMSK <- 0 (all interrupts disabled)
	BUN CEC I	/ return from CEC
/////////// interrupt handler /////////
/ 1. store AC & E to memory
I_HND, STA BA	/ M[BA] <- AC	(store AC)
	CIL			/ AC[0] <- E	(AC[15:1] is not important here...)
	STA BE		/ M[BE] <- AC	(store E)
/ 2. check SFG and S_IN
SIN,
	LDA SFG		/ AC <- M[SFG]
	SZA			/ (M[SFG] == 0) ? skip next
	BUN SOU		/ goto SOU
	SKI			/ (S_IN ready) ? skip next
	BUN IRT		/ goto IRT
/ S_IN is ready --> update IMSK (disable S_IN, enable S_OUT)
	LDA VH4		/ AC   <- (0100)
	IMK			/ IMSK <- (0100) (enable S_OUT)
/ read S_IN data
	INP			/ AC(7:0) <- INPR
	STA SDT		/ M[SDT] <- AC
	ISZ SFG		/ ++M[SFG]
/ 3. check GP_OUT
SOU, / M[SFG] != 0
	SKO			/ (S_OUT ready) ? skip next
	BUN IRT		/ goto IRT
/ S_OUT is ready --> update IMSK (disable S_OUT, enable S_IN)
	LDA VH8		/ AC   <- (1000) 
	IMK			/ IMSK <- (1000) (enable S_IN)
/ output to S_OUT
	LDA SDT		/ AC <- M[SDT]
	OUT			/ OUTR <- AC
	BSA CEC		/ call CEC (check end-character)
	CLA			/ AC <- 0
	STA SFG		/ M[SFG] <- 0
/ 4. restore AC & E from memory
IRT,LDA BE		/ AC <- M[BE]
	CIR			/ E <- AC[0]	(restore E)
	LDA BA		/ AC <- M[BA]	(restore AC)
	ION			/ IEN <- 1		(enable interrupt)
	BUN ST0 I	/ indirect return (return address stored in ST0)
/ data (no initialization)
BA, DEC 000		/ backup storage for AC during interrupt handling
BE, DEC 000		/ backup storage for  E during interrupt handling
SDT,DEC 0		/ S_IN data
/ data (need initialization code)
STT,DEC 0       / state
SFG,DEC 0       / S_IN flag
/ data (read-only)
VH4,HEX 4       / VH4 = 0x4 (0100)
VH8,HEX 8       / VHA = 0x8 (1000)
VM1,DEC -1      / VM1 = -1
VM4,DEC -4		/ VM4 = -4
END

