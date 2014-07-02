;;
;; Retrochallenge Summer 2014
;; 6502 ROM Monitor
;;


;;************************************************************************
;; Non-monitor code, e.g. BASIC, utilities, etc., resides
;; in the bottom 14KB of ROM
;;************************************************************************
.segment	"CODE"
		.org 	$C000

;;************************************************************************
;; ROM monitor code resides in the top 2KB of ROM
;;************************************************************************
.segment	"MONITOR"
		.org	$FB00

START:		LDA	#$00	; Set zero flag
		BEQ	*	; Loop forever

;;************************************************************************
;; Reset and Interrupt vectors
;;************************************************************************
.segment	"VECTORS"
		.org	$FFFA

		.word	START	; NMI vector
		.word	START	; Reset vector
		.word	START	; IRQ vector
