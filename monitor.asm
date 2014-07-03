;;;
;;; Retrochallenge Summer 2014
;;; 6502 ROM Monitor
;;;


;;;************************************************************************
;;; Non-monitor code, e.g. BASIC, utilities, etc., resides
;;; in the bottom 14KB of ROM
;;;************************************************************************
.segment "CODE"
	.org	$C000

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


START:
INITIO:	LDA	#$1D		; Set ACIA to 8N1, 9600 baud
	STA	IOCTL		;   ($1D = 8 bits, 1 stop bit, 9600)
	LDA	#$0B		;   ($0B = no parity, irq disabled)
	STA	IOCMD		;

A_LOOP:	LDA	#'@'		; Print "@" to the terminal, forever.
	JSR	COUT		; Call COUT
	BNE	A_LOOP		; Accumulator is "@", so not 0 -- loop.

;;;
;;; Print the character in the Accumulator to the ACIA's output
;;;

COUT:	PHA			; Save accumulator
@loop:	LDA	IOST		; Is TX register empty?
	AND	#$10
	BEQ	@loop		; No, wait for empty
	PLA			; Yes, restor char & print
	STA	IORW
	RTS			; Return

;;;************************************************************************
;;; Reset and Interrupt vectors
;;;************************************************************************
.segment    "VECTORS"

	.org	$FFFA
	.word	START		; NMI vector
	.word	START		; Reset vector
	.word	START		; IRQ vector
