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

;;; Zero Page
	STRLO	= $20		; Low byte of STRING (used by STR macro)
	STRHI	= $21		; Hi byte of STRING (used by STR macro)

;;; Other

;;; ----------------------------------------------------------------------
;;; Constants
;;; ----------------------------------------------------------------------

	CR	= $0A
	LF	= $0D
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

IOINIT: LDA	#$1D		; Set ACIA to 8N1, 9600 baud
	STA	IOCTL		;   ($1D = 8 bits, 1 stop bit, 9600)
	LDA	#$0B		;   ($0B = no parity, irq disabled)
	STA	IOCMD		;

	STR	BANNR		; Print out our welcome banner

PRLOOP:	LDA	#PROMPT		; Print the prompt
	JSR	COUT
NXTCHR:	JSR	CIN		; Get a character, and
	JSR	COUT		; just echo it.
	LDA	#CR
	JSR	COUT		; Then echo CR+LF
	LDA	#LF
	JSR	COUT
	BNE	PRLOOP		; and wait for next input


;;;  Catch-all infinite loop
END:	LDA	#$00		; Infinite Loop
	BEQ	*

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

BANNR:	.byte	"RETROCHALLENGE 2014 ROM MONITOR",CR,LF,0

;;;************************************************************************
;;; Reset and Interrupt vectors
;;;************************************************************************
.segment    "VECTORS"

	.org	$FFFA
	.word	START		; NMI vector
	.word	START		; Reset vector
	.word	START		; IRQ vector
