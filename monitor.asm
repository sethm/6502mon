;;;
;;; Retrochallenge Summer 2014
;;; 6502 ROM Monitor
;;;
;;; Last Updated: 2014-JUL-03
;;;

;;;************************************************************************
;;; Macro Definitions.
;;;************************************************************************

;;;
;;; STR <ADDR>
;;;
;;; Print out the null-terminated string located at address <ADDR>
;;;
;;; Modifies: Accumulator, STRLO, STRHI
;;;
.macro	STR	ADDR
	LDA	#<ADDR		; Grab the low byte of the address
	STA	STRLO
	LDA	#>ADDR		; ... and the high byte
	STA	STRHI
	JSR	STOUT		; then call STOUT.
.endmacro


;;;************************************************************************
;;; Non-monitor code, e.g. BASIC, utilities, etc., resides
;;; in the bottom 14KB of ROM
;;;************************************************************************
.segment "CODE"
	.org	$C000

;;; ----------------------------------------------------------------------
;;; Memory Definitions
;;; ----------------------------------------------------------------------

	STRLO	= $20		; Low byte of STRING (used by STR macro)
	STRHI	= $21		; Hi byte of STRING (used by STR macro)
	IBLEN	= $22		; Input buffer length

	IBUF	= $0200		; Input buffer base

;;; ----------------------------------------------------------------------
;;; Constants
;;; ----------------------------------------------------------------------

	CR	= $0A
	LF	= $0D
	BS	= $08

	PROMPT	= '*'

;;; ----------------------------------------------------------------------
;;; IO Addresses
;;; ----------------------------------------------------------------------

	IORW	= $8800		; ACIA base address, R/W registers
	IOST	= IORW+1	; ACIA status register
	IOCMD	= IORW+2	; ACIA command register
	IOCTL	= IORW+3	; ACIA control register

;;;************************************************************************
;;; ROM monitor code resides in the top 2KB of ROM
;;;************************************************************************
.segment "MONITOR"
	.org	$FB00

;;; ----------------------------------------------------------------------
;;; Main ROM Entry Point
;;; ----------------------------------------------------------------------

START:	CLI
	CLD
	LDX	#$FF		; Init stack pointer to $FF
	TXS

	;;
	;; Initialize IO
	;;
IOINIT: LDA	#$1D		; Set ACIA to 8N1, 9600 baud
	STA	IOCTL		;   ($1D = 8 bits, 1 stop bit, 9600)
	LDA	#$0B		;   ($0B = no parity, irq disabled)
	STA	IOCMD		;

	;;
	;; Hard Reset. Initialize page 2.
	;;
HRESET:	LDA	#$02		; Clear page 2
	STA	$01
	LDA	#$00
	STA	$00
	TAY			; Pointer into page 2
@loop:	DEY
	STA	($00),Y
	BNE	@loop

	;;
	;; Start the monitor by printing a welcome message.
	;;
	STR	BANNR


	;;
	;; Eval Loop - Get input, act on it, return here.
	;;
EVLOOP:	LDA	#CR
	JSR	COUT
	LDA	#LF
	JSR	COUT
	LDA	#PROMPT		; Print the prompt
	JSR	COUT

	LDA	#$00		; Initialize registers
	TAY
	TAX

	;; NXTCHR is responsible for getting the next character of
	;; input and handling it.
	;;
	;; If the character is a CR, LF, or BS, there's special
	;; handling. Otherwise, the character is added to the IBUF
	;; input buffer, and then echoed to the screen.
	;;
	;; This routine uses Y as the IBUF pointer.

NXTCHR:	JSR	CIN		; Get a character
	CMP	#CR		; Is it a carriage-return?
	BEQ	PARSE		; Done. Parse buffer.
	CMP	#LF		; Is it a line-feed?
	BEQ	PARSE		; Done. Parse buffer.
	CMP	#BS		; Is it a backspace?
	BEQ	BSPACE		; Yes, handle it.
	;; It wasn't a CR,LF, or BS
	JSR	COUT		; Echo it
	STA	IBUF,Y		; Store the character into $200,Y
	INY			; Move the pointer
	BNE	NXTCHR		; Go get the next character.

	;;
	;; Handle a backspace by decrementing Y (the IBUF pointer)
	;; unless Y is already 0.
	;;
BSPACE:	CPY	#0	       ; If Y is already 0, don't
	BEQ	NXTCHR	       ;   do anything.
	DEY
	LDA	#BS
	JSR	COUT
	JMP	NXTCHR

	;;
	;; Parse the command currently in the IBUF, with length
	;; stored in Y
	;;
PARSE:	TYA			; Save Y to IBLEN
	STA	IBLEN
	JMP	EVLOOP		; Return to the eval loop.

	BRK			; Catch-all for emulator debugging.

;;; ----------------------------------------------------------------------
;;; Print the character in the Accumulator to the ACIA's output
;;; ----------------------------------------------------------------------


COUT:	PHA			; Save accumulator
@loop:	LDA	IOST		; Is TX register empty?
	AND	#$10
	BEQ	@loop		; No, wait for empty
	PLA			; Yes, restore char & print
	STA	IORW
	RTS			; Return

;;; ----------------------------------------------------------------------
;;; Read a character from the ACIA and put it into the accumulator
;;; ----------------------------------------------------------------------

CIN:	LDA	IOST
	AND	#$08		; Is RX register full?
	BEQ	CIN		; No, wait for it to fill up.
	LDA	IORW		; Yes, load character.
	;;
	;; If the char is 'a' to 'z', inclusive, mask to upper case.
	;;
	CMP	#'a'		; < 'a'?
	BCC	@done		; Yes, done.
	CMP	#'{'		; >= '{'?
	BCS	@done		; Yes, done.
	AND	#$5f		; No, convert lower case -> upper case,
@done:	RTS			; and return.


;;; ----------------------------------------------------------------------
;;; Print the null-terminated string located at STRLO,STRHI
;;; ----------------------------------------------------------------------

STOUT:	LDY	#$00		; Initialize string pointer
@loop:	LDA	(STRLO),Y	; Get character
	BEQ	@done		; If char == 0, we're done
	JSR	COUT		; Otherwise, print it
	INY			; Increment pointer
	BNE	@loop		; Continue
@done:	RTS			; Return

;;; ----------------------------------------------------------------------
;;; Data
;;; ----------------------------------------------------------------------

BANNR:	.byte	"RETROCHALLENGE 2014 ROM MONITOR",0

;;;************************************************************************
;;; Reset and Interrupt vectors
;;;************************************************************************
.segment    "VECTORS"

	.org	$FFFA
	.word	START		; NMI vector
	.word	START		; Reset vector
	.word	START		; IRQ vector
