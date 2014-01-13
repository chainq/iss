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

{ * ISS_HARD.PAS - Routines for IRQ and DMA and other system hardware     }
{             OS - GO32V2 only.                                           }

{$INCLUDE ISS_SET.INC}
{$ASMMODE INTEL}
{$MODE FPC}
Unit ISS_Hard;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     GO32;    { * Uses GO32 unit, because DOS-only driver * }

Const ISS_IRQMapping : Array[$00..$0F] Of Byte =(
                       $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,  { * 1st controller * }
                       $70,$71,$72,$73,$74,$75,$76,$77); { * 2nd controller * }

      ISS_DMARead       = %00000100;
      ISS_DMAWrite      = %00001000;
      ISS_DMAAutoInit   = %00010000;
      ISS_DMAModeDemand = %00000000;
      ISS_DMAModeSingle = %01000000;

Var  ISS_DMASegment  : Word; { * DMA Buffer Segment Address * }
     ISS_DMAAddress  : Pointer; { * DMA Buffer Pascal Pointer * }
     ISS_DMASelector : Word; { * DMA Buffer Selector * }

Procedure ISS_IRQEnable(IRQNum : Byte);
Procedure ISS_IRQDisable(IRQNum : Byte);

Procedure ISS_DMAAllocBuffer;
Procedure ISS_DMAFreeBuffer;

Procedure ISS_DMAStart(DMAChannel : DWord; Address: Pointer;
                       BlockLength,Mode : DWord);
Procedure ISS_DMAStop(DMAChan : DWord);

Implementation

Const DMACPage   : Array[0..7] Of Byte = ($087,$083,$081,$082,$08F,$08B,$089,$08A);
      DMACOffset : Array[0..7] Of Byte = ($000,$002,$004,$006,$0C0,$0C4,$0C8,$0CC);
      DMACLength : Array[0..7] Of Byte = ($001,$003,$005,$007,$0C2,$0C6,$0CA,$0CE);
      DMACMask   : Array[0..7] Of Byte = ($00A,$00A,$00A,$00A,$0D4,$0D4,$0D4,$0D4);
      DMACMode   : Array[0..7] Of Byte = ($00B,$00B,$00B,$00B,$0D6,$0D6,$0D6,$0D6);
      DMACClear  : Array[0..7] Of Byte = ($00C,$00C,$00C,$00C,$0D8,$0D8,$0D8,$0D8);

{ * Enables the specified IRQ * }
Procedure ISS_IRQEnable(IRQNum : Byte); Assembler;
Asm
 MOV CL,IRQNum
 MOV BX,Not 1
 ROL BX,CL

 CMP CL,7
 JA  @EnableOnPic2

 { * Specified IRQ is on controller 1 * }
 IN  AL,$21
 AND AL,BL
 OUT $21,AL
 JMP @Exit

 @EnableOnPIC2:

 { * Specified IRQ is on controller 2 * }
 IN  AL,$0A1
 AND AL,BH
 OUT $0A1,AL

 IN  AL,$21
 AND AL,11111011B { * Enable IRQ 2 cascade * }
 OUT $21,AL

 @Exit:
End ['EAX','EBX','ECX'];

{ * Disables the specified IRQ * }
Procedure ISS_IRQDisable(IRQNum : Byte); Assembler;
Asm
 MOV CL,IRQNum
 MOV BX,1
 SHL BX,CL

 CMP CL,7
 JA  @DisableOnPIC2

 { * Specified IRQ is on controller 1 * }
 IN  AL,$21
 OR  AL,BL
 OUT $21,AL
 JMP @Exit

 @DisableOnPIC2:

 { * Specified IRQ is on controller 2 * }
 IN  AL,$0A1
 OR  AL,BH
 OUT $0A1,AL

 @Exit:
End ['EAX','EBX','ECX'];

{ * Allocates the DMA Buffer * }
Procedure ISS_DMAAllocBuffer;
Var Linear   : DWord;
Begin
 { * We allocate 32kb for DMA buffer, but we using only 16kb of it. * }
 { * One 16kb piece of DMA buffer will not cross DMA Page for sure. * }
 { * Yepp, this hacking another good example to show, how "good"    * }
 { * the PC architecture is... * }
 Linear:=Global_DOS_Alloc(32768);
 ISS_DMASegment :=Linear Shr 16;    { * High Word is Segment Address * }
 ISS_DMAAddress :=Pointer((Linear And $FFFF0000) Shr 12);
 ISS_DMASelector:=Linear And $FFFF; { * Low Word is Selector * }

 { * Clearing DMA buffer * }
 Asm
  MOV EDI,ISS_DMAAddress
  MOV ECX,32768/4
  XOR EAX,EAX
  @ClearLoop:
   MOV FS:[EDI],EAX
   ADD EDI,4
  LOOP @ClearLoop
 End ['EAX','ECX','EDI'];

 { * Checking DMA Page Limit * }
 If (((DWord(ISS_DMAAddress) And $0FFFF)+16384)>$10000) Then Begin
   Inc(DWord(ISS_DMAAddress),16384);
  End;

End;

{ * Free up the DMA Buffer  * }
Procedure ISS_DMAFreeBuffer;
Begin
 Unlock_Linear_Region(DWord(ISS_DMAAddress)+
        Get_Segment_Base_Address(DosMemSelector),32768);
 Global_DOS_Free(ISS_DMASelector);
 ISS_DMASelector:=0;
 ISS_DMASegment:=0;
End;

{ * This routine is by Aleksey V. Vaneev, from his FPSound API. * }
Procedure ISS_DMAStart(DMAChannel : DWord; Address: Pointer;
                       BlockLength,Mode : DWord); Assembler;
Asm
 CMP [DMAChannel],7
 JA  @Done

 AND [Mode],$0FC

 MOV EAX,4
 MOV ESI,[DMAChannel]
 ADD EAX,ESI

 CMP ESI,4
 JB  @LowDMA1

 SHR [BlockLength],1
 SHR [Address],1
 SHL WORD PTR [Address+2],1

 SUB EAX,4

 @LowDMA1:
 CMP [BlockLength],0
 JE  @Done

 XOR EDX,EDX
 MOV DL,[DMACMask+ESI]
 OUT DX,AL

 MOV DL,[DMACClear+ESI]
 OUT DX,AL

 MOV EAX,[Mode]
 ADD EAX,ESI

 CMP ESI,4
 JB  @LowDMA2

 SUB EAX,4

 @LowDMA2:
 MOV DL,[DMACMode+ESI]
 OUT DX,AL

 MOV AL,BYTE PTR [Address+2]
 MOV DL,[DMACPage+ESI]
 OUT DX,AL

 MOV AX,WORD PTR [Address]
 MOV DL,[DMACOffset + esi]
 OUT DX,AL
 MOV AL,AH
 OUT DX,AL

 MOV DL,[DMACLength + esi]
 MOV EAX, [BlockLength]
 DEC EAX
 OUT DX,AL
 MOV AL,AH
 OUT DX,AL

 MOV EAX,ESI
 CMP EAX,4
 JB  @LowDMA3

 SUB EAX,4

 @LowDMA3:
 MOV DL,[DMACMask+ESI]
 OUT DX,AL

 @Done:
End ['EAX', 'EDX', 'ESI'];

Procedure ISS_DMAStop(DMAChan : DWord);
Begin
 DMAChan:=DMAChan And 7;
 OutPortB(DMACClear[DMAChan],0);
 OutPortB(DMACMask[DMAChan],DMAChan Or 4);
End;

Begin
End.
