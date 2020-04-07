{
  Copyright (c) 1998-2001,2014  Karoly Balogh <charlie@amigaspirit.hu>

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
  THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
}

{ * ISS_CPU .PAS - CPU/Endian specific functions                          }
{             OS - Platform Independent                                   }
{            CPU - i386, m68k and generic                                 }

{$MODE FPC}
Unit ISS_CPU;

Interface

Function CLEW(LEW:Word)  : Word;
Function CLEL(LEL:DWord) : DWord; 
Function CBEW(BEW:Word)  : Word;
Function CBEL(BEL:DWord) : DWord; 

Implementation


{$IFDEF CPUI386}
{$ASMMODE INTEL}
{/--------------------------------------------------------------------------\}
{| i386 version                                                             |}
{\--------------------------------------------------------------------------/}

{ * Correct Little Endian Word for i386 * }
Function CLEW(LEW:Word) : Word; Assembler;
Asm
 xor eax,eax
 mov ax,LEW
End;

{ * Correct Little Endian Long for i386 * }
Function CLEL(LEL:DWord) : DWord; Assembler;
Asm
 mov eax,LEL
End;

{ * Correct Big Endian Word for i386 * }
Function CBEW(BEW:Word) : Word; Assembler;
Asm
 xor eax,eax
 mov ax,BEW
 xchg al,ah
End;

{ * Correct Big Endian Long for i386 * }
Function CBEL(BEL:DWord) : DWord; Assembler;
Asm
 mov   eax,BEL
 bswap eax
End;

{$ENDIF}


{$IFDEF CPUM68K}
{/--------------------------------------------------------------------------\}
{| m68k version                                                             |}
{\--------------------------------------------------------------------------/}

{ * Correct Little Endian Word for m68k * }
Function CLEW(LEW:Word) : Word; Assembler;
Asm
 move.w LEW,d0
 ror.w  #8,d0
End ['d0'];

{ * Correct Little Endian Long for m68k * }
Function CLEL(LEL:DWord) : DWord; Assembler; 
Asm
 move.l LEL,d0
 ror.w  #8,d0
 swap   d0
 ror.w  #8,d0
End ['d0'];

{ * Correct Big Endian Word for m68k * }
Function CBEW(BEW:Word) : Word; Assembler;
Asm
 sub.l  d0,d0
 move.w BEW,d0
End ['d0'];

{ * Correct Big Endian Long for m68k * }
Function CBEL(BEL:DWord) : DWord; Assembler;
Asm
 move.l BEL,d0
End ['d0'];

{$ENDIF}


{$IFDEF ENDIAN_BIG}
{$IFNDEF CPUM68K}
{$INFO Compiling for generic Big Endian }
{/--------------------------------------------------------------------------\}
{| generic big endian version                                               |}
{\--------------------------------------------------------------------------/}

{ * Correct Little Endian Word for Big Endian CPU * }
Function CLEW(LEW:Word) : Word; Inline;
Begin
 CLEW:=(Lo(LEW) Shl 8)+Hi(LEW);
End;

{ * Correct Little Endian Long for Big Endian CPU * }
Function CLEL(LEL:DWord) : DWord; Inline;
Begin
  CLEL:=(Lo(Lo(LEL)) Shl 24)+(Hi(Lo(LEL)) Shl 16)+(Lo(Hi(LEL)) Shl 8)+Hi(Hi(LEL));
End;

{ * Correct Big Endian Word for Big Endian CPU * }
Function CBEW(BEW:Word) : Word; Inline;
Begin
  CBEW:=BEW;
End;

{ * Correct Big Endian Long for Big Endian CPU * }
Function CBEL(BEL:DWord) : DWord; Inline;
Begin
  CBEL:=BEL;
End;
{$ENDIF}
{$ENDIF}


{$IFDEF ENDIAN_LITTLE}
{$IFNDEF CPUI386}
{$INFO Compiling for generic Little Endian }
{/--------------------------------------------------------------------------\}
{| generic little endian version                                            |}
{\--------------------------------------------------------------------------/}

{ * Correct Little Endian Word for Little Endian CPU * }
Function CLEW(LEW:Word) : Word; Inline;
Begin
 CLEW:=LEW;
End;

{ * Correct Little Endian Long for Little Endian CPU * }
Function CLEL(LEL:DWord) : DWord; Inline;
Begin
  CLEL:=LEL;
End;

{ * Correct Big Endian Word for Little Endian CPU * }
Function CBEW(BEW:Word) : Word; Inline;
Begin
  CBEW:=(Lo(BEW) Shl 8)+Hi(BEW);
End;

{ * Correct Big Endian Long for Little Endian CPU * }
Function CBEL(BEL:DWord) : DWord; Inline;
Begin
  CBEL:=(Lo(Lo(BEL)) Shl 24)+(Hi(Lo(BEL)) Shl 16)+(Lo(Hi(BEL)) Shl 8)+Hi(Hi(BEL));
End;

{$ENDIF}
{$ENDIF}

End.
