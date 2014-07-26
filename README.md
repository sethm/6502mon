# Loomcom 6502 ROM Monitor

**Version:** 0.1-alpha  
**Last Updated:** July 1, 2014

## 1.0 About

This project contains an experimental 6502 ROM monitor being developed
for the [Symon 6502 Simulator] (http://github.com/sethm/symon) and
associated hardware.

## 2.0 Assembly

Assembled with [CA65] (http://www.cc65.org/doc/ca65.html). Just
type `make`!

## 3.0 To Do

- Implement 'Q' command that will jump to a pre-defined QUIT
  routine, e.g. to jump back to BASIC from the monitor.

- Enhance the 'E' and 'D' commands to take '/' as the first
  argument, which will automatically increment the previously
  used Examine or Deposit address. e.g.:

     *D 0300 01   ; Deposit "01" to 0x0300
     *D / 02      ; Deposit "02" to 0x0301
     *D / 03      ; Deposit "03" to 0x0302

## 4.0 License

This project is free software. It is distributed under the MIT
License. Please see the file 'COPYING' for full details of the
license.
