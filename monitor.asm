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

;;;
;;; CRLF
;;;
;;; Print a Carriage Return / Line Feed pair
;;;
;;; Modifies: Accumulator
;;;
.macro	CRLF
	LDA	#CR
	JSR	COUT
	LDA	#LF
	JSR	COUT
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
	HTMP	= $23		; Hex parsing temp
	OPADDRL = $25		; Addr of current operand (low)
	OPADDRH	= $26		; Addr of current operand (high)

	OPBYT	= $0200		; # of bytes parsed in 16-bit operands
	TKCNT	= $0201		; Count of parsed tokens
	IBLEN	= $0202		; Input buffer length
	CMD	= $0203		; Last parsed command
	TKST	= $0204		; Token start pointer
	TKND	= $0205		; Token end pointer
	OPBASE	= $0206		; Operand base
	IBUF	= $0220		; Input buffer base

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

	;; Start the monitor by printing a welcome message.
	STR	BANNR

;;; ----------------------------------------------------------------------
;;; Main Eval Loop - Get input, act on it, and return here.
;;; ----------------------------------------------------------------------

EVLOOP:	CRLF
	LDA	#PROMPT		; Print the prompt
	JSR	COUT

	LDA	#$00		; Reset state by zeroing out
	TAX			;  all registers and temp storage.
	TAY
	STA	IBLEN
	STA	HTMP

	;; NXTCHR is responsible for getting the next character of
	;; input.
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

	;; Handle a backspace by decrementing Y (the IBUF pointer)
	;; unless Y is already 0.
BSPACE:	CPY	#0	       ; If Y is already 0, don't
	BEQ	NXTCHR	       ;   do anything.
	DEY
	LDA	#BS
	JSR	COUT
	JMP	NXTCHR


;;; ----------------------------------------------------------------------
;;; Parse the input buffer (IBUF) by tokenizing it into a command and
;;; a series of operands.
;;;
;;; When the code reaches this point, Y will hold the length of the
;;; input buffer.
;;; ----------------------------------------------------------------------

PARSE:	TYA			; Save Y to IBLEN.
	STA	IBLEN
	BEQ	EVLOOP		; No command? Short circuit.

	;; Clear operand storage
	LDY	#$10
	LDA	#$00
@loop:	STA	OPBASE,Y	; Clear operands.
	DEY
	BPL	@loop

	;; Reset parsing state
	LDX	#$FF		; Reset Token Pointer
	LDA	#$00
	STA	TKCNT		; Number of tokens we've parsed
	STA	CMD		; Clear command register.
	TAY			; Reset IBUF pointer.

	;;
	;; Tokenize the command and operands
	;;

	;; First character is the command.
	LDA	IBUF,Y
	STA	CMD

	;; Now start looking for the next token. Read from
	;; IBUF until the character is not whitespace.
SKIPSP:	INY
	CPY	IBLEN		; Is Y now pointing outside the buffer?
	BCS	EXEC		; Yes, nothing else to parse

	LDA	IBUF,Y
	CMP	#' '
	BEQ	SKIPSP		; The character is a space, skip.

	;; Here, we've found a non-space character. We can
	;; walk IBUF until we find the first non-digit (hex),
	;; at which point we'll be at the end of an operand

	STY	TKST		; Hold Y value for comparison

TKNEND:	INY
	CPY	IBLEN		; >= IBLEN?
	BCS	TKSVPTR
	LDA	IBUF,Y

	CMP	#'0'		; < '0'?
	BCC	TKSVPTR		; It's not a digit, we're done.
	CMP	#'9'+1		; < '9'?
	BCC	TKNEND		; Yup, it's a digit. Keep going.
	CMP	#'A'		; < 'A'
	BCC	TKSVPTR		; It's not a digit, we're done.
	CMP	#'F'+1		; < 'Z'?
	BCC	TKNEND		; Yup, it's a digit. Keep going.
	;; Fall through.

	;; Y is currently pointing at the end of a token, so we'll
	;; remember this location.
TKSVPTR:
	STY	TKND

	;; Now we're going to parse the operand and turn it into
	;; a number.
	;;
	;; This routine will walk the operand backward, from the least
	;; significant to the most significant digit, placing the
	;; value in OPBASE,X and OPBASE,X+1 as it "fills up" the value

	LDA	#$02
	STA	OPBYT

	;; Token 2 Binary
TK2BIN:	INX
	;; low nybble
	DEY			; Move the digit pointer back 1.
	CPY	TKST		; Is pointer < TKST?
	BCC	TKDONE		; Yes, we're done.

	LDA	IBUF,Y		; Grab the digit being pointed at.
	JSR	H2BIN		; Convert it to an int.
	STA	OPBASE,X	; Store it in OPBASE + X

	;; high nybble
	DEY			; Move the digit pointer back 1.
	CPY	TKST		; Is pointer < TKST?
	BCC	HIBCHK		; We might be done

	LDA	IBUF,Y		; Grab the digit being pointed at.
	JSR	H2BIN		; Convert it to an int.
	ASL			; Shift it left 4 bits.
	ASL
	ASL
	ASL
	ORA	OPBASE,X	; OR it with the value from the
	STA	OPBASE,X	;   last digit, and re-store it.

	;; Next byte - only if we're parsing the first two
	;; operands, which are treated as 16-bit values.
	;;
	;; (Operands 2 through F are treated as 8-bit values)

HIBCHK:	LDA	TKCNT		; If TKCNT is > 2, we can skip
	CMP	#$02		;   the low byte
	BCS	TKDONE

	DEC	OPBYT		; Have we done 2 bytes?
	BNE	TK2BIN		; If not, do next byte

	;; We've finished converting a token.

TKDONE:	INC	TKCNT		; Increment the count of tokens parsed

	CMP	#$0F		; Have we hit our maximum # of
				;    operands? (16 max)
	BCS	EXEC		; Yes, we're absolutely done, no more.

	LDA	TKND		; No, keep going. Restore Y to end of
	TAY			;   token position
	CPY	IBLEN		; Is there more in the buffer?
	BCC	SKIPSP		; Yes, try to find another token.

	;; No, the buffer is now empty. Fall through to EXEC

;;; ----------------------------------------------------------------------
;;; Execute the current command with the decoded operands.
;;; ----------------------------------------------------------------------

EXEC:	CRLF
	LDA	CMD
	CMP	#'H'		; Help requires no arguments,
	BEQ	HELP		;    so comes first.

	LDA	TKCNT		; Now we check operand count.
	BEQ	@err		; No operands? Error.

	LDX	#$00		; Reset X
	LDA	CMD		; Dispatch to the appropriate command.
	CMP	#'E'
	BEQ	EXDEP
	CMP	#'D'
	BEQ	EXDEP
	CMP	#'G'
	BEQ	GO

	;; No idea what to do, fall through
@err:	JSR	PERR
	JMP	EVLOOP

;;; HELP command
HELP:	STR	HELPS
	JMP	EVLOOP

;;; GO command
GO:	JMP	(OPBASE)	; Just jump to the appropriate address

;;; EXAMINE and DEPOSIT commands share code, and start the same way.
EXDEP:	LDA	OPBASE		; Transfer the address we want to load
	STA	OPADDRL		;    from to OPADDRL
	LDA	OPBASE+1	;
	STA	OPADDRH		;    and OPADDRH

	LDA	CMD		; Are we in DEPOSIT?
	CMP	#'D'
	BEQ	DEP		; Yes. Go there.

	;; Examine. How many operands?
	LDA	TKCNT
	CMP	#$02		; Two?
	BEQ	PRANGE		; Print a range.
	;; If just one,
	JSR	PRADDR		; Print the address
	LDA	(OPADDRL,X)	; Get data pointed at by address

	JSR	PRBYT		; Print it.
	JMP	EVLOOP		; Done.

;;; DEPOSIT command
DEP:	JSR	PRADDR

	LDA	OPBASE+2	; Grab the data to store
	STA	(OPADDRL,X)	; Store it

	JSR	PRBYT		; Then print it back out
	JMP	EVLOOP		; Done.

;;; Print a range
PRANGE:
	;; Do a 16-bit signed comparison of START and END addresses
	;; to make sure the range is valid. If it's not, error out and
	;; don't continue.
	SEC
	LDA	OPBASE+3	; Compare high bytes
	SBC	OPBASE+1
	BVC	@l1
	EOR	#$80
@l1:	BMI	@err
	BVC	@l2
	EOR	#$80
@l2:	BNE	@start
	LDA	OPBASE+2	; Compare low bytes
	SBC	OPBASE
	BCC	@err		; START is > END, no good.

@start:	JSR	PRADDR
@loop:	LDA	(OPADDRL,X)
	JSR	PR1B

	;; Are we at the end of the range?
	LDA	OPBASE+3	; Compare high
	CMP	OPADDRH
	BNE	@next
	LDA	OPBASE+2	; Compare low
	CMP	OPADDRL
	BEQ	@done

@next:	INC	OPADDRL		; Read next location in memory.
	BNE	@skip		; If L was incremented to 00,
	INC	OPADDRH		;   inc H too.
@skip:
	;; Insert a carriage return and print
	;; next address if needed.
	LDA	OPADDRL
	AND	#$0F
	BNE	@loop		; No CRLF/ADDR needed
	CRLF
	JSR	PRADDR
	LDY	#$10
	JMP	@loop

@done:	JMP	EVLOOP
@err:	JSR	PERR
	JMP	EVLOOP

;;; Print a space and the byte in A
PR1B:	PHA
	LDA	#' '
	JSR	COUT
	PLA
	JSR	PRBYT
	RTS

;;; ----------------------------------------------------------------------
;;; Print the last stored address as four consecutive ASCII hex
;;; characters.
;;;
;;; Input:  EMEMH/EMEML
;;; Output: ACIA
;;; ----------------------------------------------------------------------

PRADDR:	LDA	OPADDRH 	; Load the byte at OPBASE+1.
	JSR	PRBYT		; Print it out.
	LDA	OPADDRL		; Load the byte at OPBASE.
	JSR	PRBYT		; Print it out.
	LDA	#':'		; Print a ": " separator.
	JSR	COUT
	LDA	#' '
	JSR	COUT
	RTS			; Return.

;;; ----------------------------------------------------------------------
;;; Convert a single ASCII hex character to an unsigned int
;;; (from 0 to 16).
;;;
;;; Input:  Accumulator
;;; Output: Accumulator
;;; ----------------------------------------------------------------------

H2BIN:	SEC
	SBC	#'0'		; Subtract '0' from the digit.
	CMP	#10		; Is the result <= 10? Digit was 0-9.
	BCC	@done		; We're done.

	CMP	#23		; Is this a hex digit? (<= 'F' - 30)
	BCS	@err		; No, it's not a hex digit.
	SEC
	SBC	#7		; OK, it's a hex digit.
@done:	RTS
@err:	CRLF
	JSR	PERR
	RTS

;;; ----------------------------------------------------------------------
;;; Parse Error
;;;
;;; Abort the current operation, print an error prompt ("?") and
;;; get next line.
;;; ----------------------------------------------------------------------

PERR:	LDA	#'?'
	JSR	COUT
	JMP	EVLOOP
	RTS

;;; ----------------------------------------------------------------------
;;; Print the content of the accumulator as two consecutive ASCII
;;; hex characters.
;;;
;;; Input:  Accumulator
;;; Output: ACIA
;;; ----------------------------------------------------------------------

PRBYT:	PHA			; We'll need A later.
	LSR			; Shift high nybble to low nybble
	LSR
	LSR
	LSR
	JSR	PRHEX		; Print it as a single hex char
	PLA			; Get A back
	;; Fall through to PRHEX

;;; ----------------------------------------------------------------------
;;; Print the low nybble of of the accumulator as a single
;;; ASCII hex character.
;;;
;;; Input:  Accumulator
;;; Output: ACIA
;;; ----------------------------------------------------------------------

PRHEX:	AND	#$0F		; Mask out the high nybble
	CMP	#$0A		; Is it less than 10?
	BCC	@done
	ADC	#6
@done:	ADC	#'0'
	JSR	COUT
	RTS

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
HELPS:	.byte	"COMMANDS ARE:",CR,LF
	.byte	"H                             HELP",CR,LF
	.byte	"E <addr> [<addr>]             EXAMINE",CR,LF
	.byte	"D <addr> <val> [<val> ...]    DEPOSIT",CR,LF
	.byte	"G <addr>                      GO",CR,LF,0

;;;************************************************************************
;;; Reset and Interrupt vectors
;;;************************************************************************
.segment    "VECTORS"

	.org	$FFFA
	.word	START		; NMI vector
	.word	START		; Reset vector
	.word	START		; IRQ vector
