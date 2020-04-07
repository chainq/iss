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

{ * ISS_MIX .PAS - Software mixer for non-wavetable devices               }
{             OS - Platform Independent                                   }
{            CPU - Motorola 680x0                                         }

{ * NOTE: The code contained by this unit is in ALPHA stage. May and WILL }
{         change without warning. Make sure your own software don't rely  }
{         on this code.                                                   }

{$INCLUDE ISS_SET.INC}
{$WARNINGS ON}
{$MODE FPC}
Unit ISS_Mix;

Interface

Uses ISS_Var;

Type ISS_TMixChannel = Record
       MixSmpPtr  : ISS_PSample;
       MixSmpPos  : DWord; { * Current sample position * }
       MixSmpRate : DWord;
       MixSmpHighStep : DWord;
       MixSmpLowStep  : DWord;
       MixSmpInc      : DWord;
       MixSmpPan  : Word;
       MixSmpVol  : Word;
       MixSmpVolL : Word;
       MixSmpVolR : Word;
       MixSmpEnd  : DWord;
       MixDebug   : DWord;
      End;

     ISS_TMixer = Record
       MixRate      : DWord;   { * Mixing rate * }
       MixBufSize   : DWord;   { * Mixing buffer size * }
       MixBufCount  : DWord;   { * Current bufferpos counter * }
       MixBufPtr    : Pointer; { * Address of the buffer * }
       MixLenPos    : DWord;   { * Current position between the syscalls * }
       MixBufOffs   : DWord;   { * Mixer targetbuffer offset * }
       MixBufSel    : Word;    { * Mixer targetbuffer selector * }
       MixBufType   : Word;    { * Mixer targetbuffer type * }
       MixRevStereo : Boolean; { * Reversing stereo * }
       { * Independent data for each channel * }
       MixChannels : Array[0..ISS_MaxSSChannels-1] Of ISS_TMixChannel;
      End;
     ISS_PMixer = ^ISS_TMixer;

Var ISS_MixerData : ISS_PMixer;
    ISS_MixerOK   : Boolean;
    ISS_MixVolTab : Array[0..64,0..255] Of Word;

Function ISS_MixerInit(MixFreq : DWord; BufSize : DWord;
                       TSelector : Word; TOffset : DWord;
                       Mode : Word) : Boolean;
Function ISS_MixerDone : Boolean;

Function ISS_MixerLoadSample(SStruc : ISS_PSample) : Boolean;
Function ISS_MixerFreeSample(SStruc : ISS_PSample) : Boolean;

Procedure ISS_MixerUpdateOutput;

Implementation

Procedure MixCalcVolumeTable;
Var Counter,Counter2 : DWord;
Begin
 For Counter:=0 To 64 Do Begin
   For Counter2:=0 To 255 Do Begin
     ISS_MixVolTab[Counter,Counter2]:=((Counter2 Shl 8)*Counter) Div 64;
    End;
  End;
End;

Function ISS_MixerInit(MixFreq : DWord; BufSize : DWord;
                       TSelector : Word; TOffset : DWord;
                       Mode : Word) : Boolean;
Begin
 ISS_MixerInit:=False;
 If ISS_MixerOK Then Exit;

 New(ISS_MixerData);
 FillChar(ISS_MixerData^,SizeOf(ISS_TMixer),#0);

 With ISS_MixerData^ Do Begin
   MixRate:=MixFreq;
   MixBufSize:=BufSize;
   { * Allocating Memory for mixer buffer * }
   GetMem(MixBufPtr,MixBufSize*8);
   MixBufOffs:=TOffset;
   MixBufSel:=TSelector;
   MixBufType:=Mode;
  End;

 MixCalcVolumeTable;

 ISS_MixerOK:=True;
 ISS_MixerInit:=True;
End;

Function ISS_MixerDone : Boolean;
Begin
 ISS_MixerDone:=False;
 If Not ISS_MixerOK Then Exit;

 With ISS_MixerData^ Do Begin
   { * Freeing up memory for mixer buffer * }
   FreeMem(MixBufPtr,MixBufSize*8);
  End;

 Dispose(ISS_MixerData);

 ISS_MixerOK:=False;
 ISS_MixerDone:=True;
End;

Function ISS_MixerLoadSample(SStruc : ISS_PSample) : Boolean;
Type PByte    = ^Byte;
     PInteger = ^Integer;
Var SmpRealLength : DWord;
    SmpLoopLength : DWord;
    SmpPointer    : Pointer;
    Counter       : DWord;
Begin
 ISS_MixerLoadSample:=False;
 With SStruc^ Do Begin
   { * Unrolling bidirectional loops, so main mixing routine will be * }
   { * much simpler (and faster) * }
   If (SType And (ISS_SmpPingPongLoop-ISS_SmpForwardLoop))>0 Then Begin

     { * Calculating unrolled sample size * }
     SmpLoopLength:=SLoopEnd-SLoopStart;
     SmpRealLength:=SLoopEnd+SmpLoopLength;
     { * Allocating memory * }
     GetMem(SmpPointer,SmpRealLength); If SmpPointer=Nil Then Exit;

     { * Unrolling loop * }
     If (SType And ISS_Smp16BitData)>0 Then Begin
       For Counter:=0 To (SLoopEnd Div 2)-1 Do Begin
         PInteger(SmpPointer)[Counter]:=PInteger(SData)[Counter];
        End;
       For Counter:=0 To (SmpLoopLength Div 2)-1 Do Begin
         PInteger(SmpPointer)[(SLoopEnd Div 2)+Counter]:=PInteger(SData)[(SLoopEnd Div 2)-Counter];
        End;
      End Else Begin
       For Counter:=0 To SLoopEnd-1 Do Begin
         PByte(SmpPointer)[Counter]:=PByte(SData)[Counter];
        End;
       For Counter:=0 To SmpLoopLength-1 Do Begin
         PByte(SmpPointer)[SLoopEnd+Counter]:=PByte(SData)[SLoopEnd-Counter];
        End;
      End;

     { * Converting loop values * }
     SLoopEnd:=SLoopEnd+SmpLoopLength;

     { * When unrolling is done, we assign the samplepointer * }
     SDRAMOffs:=DWord(SmpPointer);

    End Else Begin
     { * If there is no bidi loop, it's not needed to unroll the sampledata * }
     { * so we simply assign the pointer * }
     SDRAMOffs:=DWord(SData);
    End;

  End;
 ISS_MixerLoadSample:=True;
End;

Function ISS_MixerFreeSample(SStruc : ISS_PSample) : Boolean;
Var SmpRealLength : DWord;
    SmpLoopLength : DWord;
    SmpPointer    : Pointer;
Begin
 ISS_MixerFreeSample:=False;
 With SStruc^ Do Begin
   { * We only free up the samples with unrolled loops * }
   If (SType And (ISS_SmpPingPongLoop-ISS_SmpForwardLoop))>0 Then Begin

     { * Calculating rolled sample size * }
     SmpLoopLength:=SLoopEnd-SLoopStart;
     SmpLoopLength:=SmpLoopLength Div 2;
     SLoopEnd:=SLoopStart+SmpLoopLength;
     SmpRealLength:=SLoopEnd+SmpLoopLength;

     DWord(SmpPointer):=SDRAMOffs;
     SDRAMOffs:=0;
     If SmpPointer=Nil Then Exit;

     { * Deallocating memory * }
     FreeMem(SmpPointer,SmpRealLength);

    End;
  End;
 ISS_MixerFreeSample:=True;
End;

{ * >>> A D D I T I O N A L  M I X I N G  F U N C T I O N S * <<< }

Procedure ISS_MixerClearBuffer(OutputType : Word); Assembler;
Asm
 move.l  ISS_MixerData,a0
 move.l  12(a0),a1
 move.l  4(a0),d1

 moveq.l #0,d0
 move.w  OutputType,d0
 andi.l  #ISS_DevStereo,d0
 beq    @NotStereo
    lsl.l  #1,d1
 @NotStereo:

 moveq.l #0,d0
 @ClearLoop:
    move.l d0,(a1)+
    move.l d0,(a1)+
    move.l d0,(a1)+
    move.l d0,(a1)+
    subq.l #4,d1
 bne    @ClearLoop 
End;


{ * Possible buffer conversions * }
{ * 16bit signed -> 16bit signed (no conversion) * }
{ * 16bit signed -> 8bit signed * }
{ * 16bit signed -> 8bit unsigned * }

Procedure ISS_MixerMakeClip(OutputType : Word); Assembler;
Asm
 move.l	ISS_MixerData,a0

 move.l	4(a0),d7  { MixBufSize }
 move.l	12(a0),a1 { MixBufPtr  }
 move.l  20(a0),a2 { MixBufOffs }

 moveq	#0,d0	 
 move.w	OutputType,d0
 move.l	d0,d1
 andi.l	#ISS_DevStereo,d1
 beq		@NotStereo
    lsl.l	#1,d7
 @NotStereo:

 move.l	#$00008000,d5
 move.l	#$FFFF8000,d6
 moveq.l	#0,d4

 move.l 	d0,d1
 andi.l	#ISS_Dev16Bit,d1
 beq		@Dev8Bit

 @LoopHead16Bit: { * 16bit signed -> 16bit signed * }

    move.l	(a1)+,d1
	 move.l	(a1)+,d2
    asr.l	#1,d1
    asr.l	#1,d2
    add.l   d4,d1    { Noise reduction code! }
    move.b 	d1,d4
    asr.l	#8,d1
    add.l   d4,d2
    move.b  d2,d4
	 asr.l	#8,d2

	 move.l  d1,d0
	 move.l	d2,d3
    ext.l   d0
	 ext.l	d3

    cmp.l   d0,d1
    beq     @CheckOk16_01
       and.l   d5,d1
       sne		d1
		 extb.l	d1
		 eor.l	d6,d1
    @CheckOk16_01:

	 cmp.l	d3,d2
	 beq		@CheckOk16_02
		 and.l	d5,d2
		 sne		d2
		 extb.l	d2
		 eor.l	d6,d2
	 @CheckOk16_02:

	 ror.w 	#8,d1
	 ror.w	#8,d2

    swap		d1
    move.w	d2,d1
    move.l	d1,(a2)+
    
	 subq.l	#2,d7
 bne @LoopHead16Bit
 bra @Exit

 @Dev8Bit:

 move.l  d0,d1 
 andi.l  #ISS_DevSigned,d1
 beq     @LoopHead8BitUnsigned

 @LoopHead8BitSigned:
    move.l	(a1)+,d1
	 move.l	(a1)+,d2
    asr.l	#1,d1
	 asr.l	#1,d2
    add.l   d4,d1    { Noise reduction code! }
    move.b 	d1,d4
    asr.l	#8,d1
    add.l   d4,d2
    move.b  d2,d4
	 asr.l	#8,d2

	 move.l  d1,d0
	 move.l	d2,d3
    ext.l   d0
    ext.l   d3

    cmp.l   d0,d1
    beq     @CheckOk8S_01
       and.l   d5,d1
       sne		d1
		 extb.l	d1
		 eor.l	d6,d1
    @CheckOk8S_01:

	 cmp.l	d3,d2
	 beq		@CheckOk8S_02
		 and.l	d5,d2
		 sne		d2
		 extb.l	d2
		 eor.l	d6,d2
	 @CheckOk8S_02:

    lsr.l   #8,d2  
    move.b  d2,d1

    swap    d1
    move.l  d1,a3    { !!! Store previous word... }

    move.l	(a1)+,d1
	 move.l	(a1)+,d2
    asr.l	#1,d1
    asr.l	#1,d2
    add.l   d4,d1    { Noise reduction code! }
    move.b 	d1,d4
    asr.l	#8,d1
    add.l   d4,d2
    move.b  d2,d4
	 asr.l	#8,d2

	 move.l  d1,d0
	 move.l	d2,d3
    ext.l   d0
    ext.l   d3

    cmp.l   d0,d1
    beq     @CheckOk8S_03
       and.l   d5,d1
       sne		d1
		 extb.l	d1
		 eor.l	d6,d1
    @CheckOk8S_03:

	 cmp.l	d3,d2
	 beq		@CheckOk8S_04
		 and.l	d5,d2
		 sne		d2
		 extb.l	d2
		 eor.l	d6,d2
	 @CheckOk8S_04:

    lsr.l   #8,d2  
    move.l  a3,d0
    move.b  d2,d1
    move.w  d1,d0
    move.l	d0,(a2)+

    subq.l  #4,d7
 bne     @LoopHead8BitSigned
 bra     @Exit

 @LoopHead8BitUnsigned:

    move.l	(a1)+,d1
	 move.l	(a1)+,d2
    asr.l	#1,d1
    asr.l	#1,d2
    add.l   d4,d1    { Noise reduction code! }
    move.b 	d1,d4
    asr.l	#8,d1
    add.l   d4,d2
    move.b  d2,d4
	 asr.l	#8,d2

	 move.l  d1,d0
	 move.l	d2,d3
    ext.l   d0
    ext.l   d3

    cmp.l   d0,d1
    beq     @CheckOk8US_01
       and.l   d5,d1
       sne		d1
		 extb.l	d1
		 eor.l	d6,d1
    @CheckOk8US_01:

	 cmp.l	d3,d2
	 beq		@CheckOk8US_02
		 and.l	d5,d2
		 sne		d2
		 extb.l	d2
		 eor.l	d6,d2
	 @CheckOk8US_02:

    lsr.l   #8,d2  
    add.w   #$8000,d1
    add.b   #128,d2
    move.b  d2,d1

    swap    d1
    move.l  d1,a3    { !!! Store previous word... }

    move.l	(a1)+,d1
	 move.l	(a1)+,d2
    asr.l	#1,d1
    asr.l	#1,d2
    add.l   d4,d1    { Noise reduction code! }
    move.b 	d1,d4
    asr.l	#8,d1
    add.l   d4,d2
    move.b  d2,d4
	 asr.l	#8,d2

	 move.l  d1,d0
	 move.l	d2,d3
    ext.l   d0
    ext.l   d3

    cmp.l   d0,d1
    beq     @CheckOk8US_03
       and.l   d5,d1
       sne		d1
		 extb.l	d1
		 eor.l	d6,d1
    @CheckOk8US_03:

	 cmp.l	d3,d2
	 beq		@CheckOk8US_04
		 and.l	d5,d2
		 sne		d2
		 extb.l	d2
		 eor.l	d6,d2
	 @CheckOk8US_04:

    lsr.l   #8,d2  
    add.w   #$8000,d1
    add.b   #128,d2
    move.l  a3,d0
    move.b  d2,d1
    move.w  d1,d0
    move.l	d0,(a2)+

    subq.l  #4,d7
 bne @LoopHead8BitUnsigned

 @Exit:
End;


{ * >>> S T E R E O  M I X I N G  F U N C T I O N S <<< * }

{ * Mixes a 8bit sample into a stereo buffer * }
Function MixChannel8BitSampleSTEREO(MChIndexPtr : Pointer) : DWord; Assembler;
Asm
 move.l  MChIndexPtr,a0
 move.l  (a0),a1          { * a1 = MixSmpPtr * }
 move.l  48(a1),a2        { * SDRAMOffs * }
 move.l  ISS_MixerData,a3
 move.l	12(a3),a4        { * a4 = MixBufPtr * }

 move.l  4(a0),d2    { * MixSmpPos * }
 move.l  20(a0),d6   { * MixSmpInc * }
 move.l	12(a0),d3   { * MixSmpHighStep * }
 move.l  16(a0),d5   { * MixSmpLowStep  * }
 move.l  4(a3),d7  { 680x0 mixer uses d7 register as MixCounter instead of a variable } 

 move.l  d5,d0   { * MixSmpLowStep * }
 move.l  d3,d1   { * MixSmpHighStep * }
 mulu.l  d7,d0
 swap    d1
 mulu.l  d7,d1
 add.l   d0,d1
 add.l   d6,d1
 move.l  d1,d0
 lsr.l   #8,d1
 lsr.l   #8,d1
 add.l   d2,d1
 cmp.l   32(a0),d1
 ble     @NoChk

 @LoopHead:
  { * Getting out samplevalue * }
  move.b  (a2,d2),d0
  extb.l  d0
  asl.l   #8,d0

  { * Mix sample with volume * }
  move.w  28(a0),d1     { * MixSmpVolL * }
  muls.w  d0,d1
  move.w  30(a0),d4     { * MixSmpVolR * }
  muls.w  d0,d4

  { * Adding current sample value to buffer position * }
  add.l   d1,(a4)+
  add.l   d4,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  add.w   d5,d6   { * MixSmpLowStep  * }
  addx.l	 d3,d2   { * Increasing sample position /w MixSmpHighStep + Carry * }

  { * Checking sample limits * }
  cmp.l   32(a0),d2     { * MixSmpEnd * }
  blt     @SmpOk
     move.b   38(a1),d1    { * SType * }
     andi.b   #ISS_SmpPingPongLoop,d1
     beq      @SmpEnd
        move.l   28(a1),d2    { * SLoopStart * }
        bra      @SmpOk
     @SmpEnd:  
        move.l   24(a1),d2    { * SLength * }
        move.l   #$10000,d0
        bra      @LoopExit
  @SmpOk:

  subq.l   #1,d7
 bne   @LoopHead
 bra   @LoopExit

 @NoChk:
 { * Checking for zero volume * }
 move.w 28(a0),d1
 move.w 30(a0),d4
 add.w  d1,d4
 bne @NoChkLoopHead
    andi.l #$0000FFFF,d0
    move.l d1,d2
    move.l d0,d6
    bra @LoopExit

 @NoChkLoopHead:
  { * Getting out samplevalue * }
  move.b  (a2,d2),d0
  extb.l  d0
  asl.l   #8,d0

  { * Mix sample with volume * }
  move.w  28(a0),d1     { * MixSmpVolL * }
  muls.w  d0,d1
  move.w  30(a0),d4     { * MixSmpVolR * }
  muls.w  d0,d4

  { * Adding current sample value to buffer position * }
  add.l   d1,(a4)+
  add.l   d4,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  add.w   d5,d6   { * MixSmpLowStep  * }
  addx.l	 d3,d2   { * Increasing sample position /w MixSmpHighStep + Carry * }

  { * No need for checking sample limits here... * }
  subq.l   #1,d7
 bne   @NoChkLoopHead

 @LoopExit:
 { * Moving new sample pos into the record * }
 move.l   d6,20(a0)  { * MixSmpInc * }
 move.l   d2,4(a0)   { * MixSmpPos * }
End;

{ * Mixes a 16bit sample into a stereo buffer * }
Function MixChannel16BitSampleSTEREO(MChIndexPtr : Pointer) : DWord; Assembler;
Asm
 move.l  MChIndexPtr,a0
 move.l  (a0),a1          { * a1 = MixSmpPtr * }
 move.l  48(a1),a2        { * SDRAMOffs * }
 move.l  ISS_MixerData,a3
 move.l	12(a3),a4        { * a4 = MixBufPtr * }

 move.l  4(a0),d2    { * MixSmpPos * }
 move.l  20(a0),d6   { * MixSmpInc * }
 move.l  12(a0),d3   { * MixSmpHighStep * }
 move.l  16(a0),d5   { * MixSmpLowStep * }
 move.l  4(a3),d7  { 680x0 mixer uses d7 register as MixCounter instead of a variable } 

 move.l  d5,d0   { * MixSmpLowStep * }
 move.l  d3,d1   { * MixSmpHighStep * }
 mulu.l  d7,d0
 swap    d1
 mulu.l  d7,d1
 add.l   d0,d1
 add.l   d6,d1
 move.l  d1,d0
 lsr.l   #8,d1
 lsr.l   #8,d1
 add.l   d2,d1
 lsl.l   #1,d1
 cmp.l   32(a0),d1
 ble     @NoChk

 @LoopHead:
  { * Getting out samplevalue * }
  move.w  (a2,d2),d0
  ext.l   d0

  { * Mix sample with volume * }
  move.w  28(a0),d1     { * MixSmpVolL * }
  muls.w  d0,d1
  move.w  30(a0),d4     { * MixSmpVolR * }
  muls.w  d0,d4

  { * Adding current sample value to buffer position * }
  add.l   d1,(a4)+
  add.l   d4,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  moveq.l #0,d0
  add.w   d5,d6   { * MixSmpLowStep  * }
  addx.l	 d3,d0   { * MixSmpHighStep + Carry * }
  lsl.l   #1,d0

  { * Increasing sample position * }
  add.l   d0,d2

  { * Checking sample limits * }
  cmp.l   32(a0),d2     { * MixSmpEnd * }
  blt     @SmpOk
     move.b   38(a1),d1    { * SType * }
     andi.b   #ISS_SmpPingPongLoop,d1
     beq      @SmpEnd
        move.l   28(a1),d2    { * SLoopStart * }
        bra      @SmpOk
     @SmpEnd:  
        move.l   24(a1),d2    { * SLength * }
        move.l   #$10000,d0
        bra      @LoopExit
  @SmpOk:

  subq.l   #1,d7
 bne   @LoopHead
 bra   @LoopExit

 @NoChk:
 { * Checking for zero volume * }
 move.w 28(a0),d1  
 move.w 30(a0),d4
 add.w  d1,d4
 bne @NoChkLoopHead
    andi.l #$0000FFFF,d0
    move.l d1,d2
    move.l d0,d6
    bra @LoopExit

 @NoChkLoopHead:
  { * Getting out samplevalue * }
  move.w  (a2,d2),d0
  ext.l   d0

  { * Mix sample with volume * }
  move.w  28(a0),d1     { * MixSmpVolL * }
  muls.w  d0,d1
  move.w  30(a0),d4     { * MixSmpVolR * }
  muls.w  d0,d4

  { * Adding current sample value to buffer position * }
  add.l   d1,(a4)+
  add.l   d4,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  moveq.l #0,d0
  add.w   d5,d6   { * MixSmpLowStep  * }
  addx.l	 d3,d0   { * MixSmpHighStep + Carry * }
  lsl.l   #1,d0

  { * Increasing sample position * }
  add.l   d0,d2

  { * No need for checking sample limits here... * }
  subq.l   #1,d7
 bne   @NoChkLoopHead

 @LoopExit:
 { * Moving new sample pos into the record * }
 move.l   d6,20(a0)  { * MixSmpInc * }
 move.l   d2,4(a0)   { * MixSmpPos * }
End;

Procedure ISS_MixerUpdateBufferSTEREO;
Var ChannelCounter : DWord;
    MChIndexPtr    : Pointer;
    MixedLength    : DWord;
Begin
 With ISS_MixerData^ Do Begin

   { * Clearing mixer buffer * }
   ISS_MixerClearBuffer(ISS_DevStereo);

   { * Main mixing loop * }
   { * Going through all the active channels * }
   For ChannelCounter:=0 To ISS_ActiveSSChannels-1 Do Begin
     With ISS_VirtualChannels^[ChannelCounter] Do Begin

       { * If channel is active, then updating... * }
       If (VChControl And ISS_CCActive)>0 Then Begin
         With MixChannels[ChannelCounter] Do Begin

           With MixSmpPtr^ Do Begin
             If (SType And ISS_SmpPingPongLoop)>0 Then Begin
               MixSmpEnd:=SLoopEnd;
              End Else Begin
               MixSmpEnd:=SLength;
              End;

             MChIndexPtr:=@MixChannels[ChannelCounter];
             If (SType And ISS_Smp16BitData)>0 Then Begin
               MixedLength:=MixChannel16BitSampleSTEREO(MChIndexPtr);
              End Else Begin
               MixedLength:=MixChannel8BitSampleSTEREO(MChIndexPtr);
              End;

             If MixedLength=$10000 Then Dec(VChControl,ISS_CCActive);

            End;

          End;
        End;
      End;

    End;

  End;
End;


{ * >>> M O N O  M I X I N G  F U N C T I O N S <<< * }

{ * Mixes a 8bit sample into a mono buffer * }
Function MixChannel8BitSampleMONO(MChIndexPtr : Pointer) : DWord; Assembler;
Asm
 move.l  MChIndexPtr,a0
 move.l  (a0),a1          { * a1 = MixSmpPtr * }
 move.l  48(a1),a2        { * SDRAMOffs * }
 move.l  ISS_MixerData,a3
 move.l	12(a3),a4        { * a4 = MixBufPtr * }

 move.l  4(a0),d2         { * MixSmpPos * }
 move.l  20(a0),d6        { * MixSmpInc * }
 moveq.l #0,d7  { 680x0 mixer uses d7 register as MixCounter instead of a variable } 

 @LoopHead:
  { * Getting out samplevalue * }
  move.b  (a2,d2),d0
  extb.l  d0
  asl.l   #8,d0

  { * Mix sample with volume * }
  moveq.l #0,d1
  move.w  26(a0),d1     { * MixSmpVol * }
  muls.l  d1,d0

  { * Adding current sample value to buffer position * }
  add.l   d0,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  add.l   16(a0),d6   { * MixSmpLowStep * }
  move.l  d6,d0
  lsr.l   #8,d0
  lsr.l   #8,d0
  and.l   #$0000FFFF,d6 
  add.l   12(a0),d0   { * MixSmpHighStep * }

  { * Increasing sample position * }
  add.l   d0,d2

  { * Checking sample limits * }
  cmp.l   32(a0),d2     { * MixSmpEnd * }
  blt     @SmpOk
     move.b   38(a1),d3    { * SType * }
     andi.b   #ISS_SmpPingPongLoop,d3
     beq      @SmpEnd
        move.l   28(a1),d2    { * SLoopStart * }
        bra      @SmpOk
     @SmpEnd:  
        move.l   24(a1),d2    { * SLength * }
        move.l   #$0FFFF,d7  
  @SmpOk:
  addq.l   #1,d7

  cmp.l    4(a3),d7
 blt   @LoopHead

 { * Moving new sample pos into the record * }
 move.l   d6,20(a0)  { * MixSmpInc * }
 move.l   d2,4(a0)   { * MixSmpPos * }
 move.l   d7,d0
End;

{ * Mixes a 16bit sample into a mono buffer * }
Function MixChannel16BitSampleMONO(MChIndexPtr : Pointer) : DWord; Assembler;
Var MixCounter     : DWord;
Asm
 move.l  MChIndexPtr,a0
 move.l  (a0),a1          { * a1 = MixSmpPtr * }
 move.l  48(a1),a2        { * SDRAMOffs * }
 move.l  ISS_MixerData,a3
 move.l	12(a3),a4        { * a4 = MixBufPtr * }

 move.l  4(a0),d2    { * MixSmpPos * }
 move.l  20(a0),d6   { * MixSmpInc * }
 moveq.l #0,d7  { 680x0 mixer uses d7 register as MixCounter instead of a variable } 

 @LoopHead:
  { * Getting out samplevalue * }
  move.w  (a2,d2),d0
  ext.l   d0

  { * Mix sample with volume * }
  moveq.l #0,d1
  move.w  26(a0),d1     { * MixSmpVol * }
  muls.l  d1,d0

  { * Adding current sample value to buffer position * }
  add.l   d0,(a4)+

  { * Calculating stepping rate step two * }
  { * Getting the real step value from the fixedpoint rate * }
  { * calculated previously * }
  add.l   16(a0),d6   { * MixSmpLowStep * }
  move.l  d6,d0
  lsr.l   #8,d0
  lsr.l   #8,d0
  and.l   #$0000FFFF,d6 
  add.l   12(a0),d0   { * MixSmpHighStep * }
  lsl.l   #1,d0
  
  { * Increasing sample position * }
  add.l   d0,d2

  { * Checking sample limits * }
  cmp.l   32(a0),d2     { * MixSmpEnd * }
  blt     @SmpOk
     move.b   38(a1),d3    { * SType * }
     andi.b   #ISS_SmpPingPongLoop,d3
     beq      @SmpEnd
        move.l   28(a1),d2    { * SLoopStart * }
        bra      @SmpOk
     @SmpEnd:  
        move.l   24(a1),d2    { * SLength * }
        move.l   #$0FFFF,d7  
  @SmpOk:
  addq.l   #1,d7

  cmp.l    4(a3),d7
 blt   @LoopHead

 { * Moving new sample pos into the record * }
 move.l   d2,4(a0)   { * MixSmpPos * }
 move.l   d7,d0
End;

Procedure ISS_MixerUpdateBufferMONO;
Var ChannelCounter : DWord;
    MChIndexPtr    : Pointer;
    MixedLength    : DWord;
Begin
 With ISS_MixerData^ Do Begin

   { * Main mixing loop * }
   { * Going through all the active channels * }
   For ChannelCounter:=0 To ISS_ActiveSSChannels-1 Do Begin
     With ISS_VirtualChannels^[ChannelCounter] Do Begin

       { * If channel is active, then updating... * }
       If (VChControl And ISS_CCActive)>0 Then Begin
         With MixChannels[ChannelCounter] Do Begin

           With MixSmpPtr^ Do Begin
             If (SType And ISS_SmpPingPongLoop)>0 Then Begin
               MixSmpEnd:=SLoopEnd;
              End Else Begin
               MixSmpEnd:=SLength;
              End;

             MChIndexPtr:=@MixChannels[ChannelCounter];

             If (SType And ISS_Smp16BitData)>0 Then Begin
               MixedLength:=MixChannel16BitSampleMONO(MChIndexPtr);
              End Else Begin
               MixedLength:=MixChannel8BitSampleMONO(MChIndexPtr);
              End;

             If MixedLength=$10000 Then Dec(VChControl,ISS_CCActive);

            End;

          End;
        End;
      End;

    End;

  End;
End;



Procedure ISS_MixerUpdateOutput;
Var ChannelCounter : DWord;
Begin

 With ISS_MixerData^ Do Begin

 { * Start Voices Update * }
 For ChannelCounter:=0 To ISS_ActiveSSChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin
     With MixChannels[ChannelCounter] Do Begin

       { * Anything to do on this channel? * }
       If (VChControl>1) And ((VChControl And ISS_CCActive)>0) Then Begin

         { * Stop Voice? * }
         If (VChControl And ISS_CCStop)>0 Then Begin
           Dec(VChControl,ISS_CCStop);
           Dec(VChControl,ISS_CCActive);
          End;

         { * Start a Sample ? * }
         If (VChControl And ISS_CCSample)>0 Then Begin
           Dec(VChControl,ISS_CCSample);
           MixSmpPtr:=VChSmpAddr;
           With MixSmpPtr^ Do Begin
             { * Offset limit checking * }
             If (VChSmpOffs>=SLength) And
                ((SType And ISS_SmpPingPongLoop)>0) Then Begin
                VChSmpOffs:=SLoopStart;
              End;
            End;
           MixSmpPos:=VChSmpOffs;
           If (MixSmpPtr<>Nil) And (MixSmpPtr^.SLength>0) Then
             VChControl:=VChControl Or ISS_CCActive;
          End;

         { * Change Channel Volume ? * }
         { * Change Channel Panning ? * }
         If ((VChControl And ISS_CCVolume)>0) Or
            ((VChControl And ISS_CCPanning)>0) Then Begin

           VChControl:=VChControl And Not ISS_CCVolume;
           VChControl:=VChControl And Not ISS_CCPanning;

           MixSmpVol:=VChFinalVolume;
           MixSmpPan:=VChFinalPanning;

           If Not MixRevStereo Then Begin
             MixSmpVolL:=(MixSmpPan+1);
             MixSmpVolR:=(256-MixSmpPan);
            End Else Begin
             MixSmpVolR:=(MixSmpPan+1);
             MixSmpVolL:=(256-MixSmpPan);
            End;

           {If MixSmpVolL>127 Then MixSmpVolL:=127;}
           {If MixSmpVolR>127 Then MixSmpVolR:=127;}
           MixSmpVolL:=(MixSmpVolL*MixSmpVol) Div 64;
           MixSmpVolR:=(MixSmpVolR*MixSmpVol) Div 64;
          End;

         { * Change Channel Frequency ? * }
         If (VChControl And ISS_CCPeriod)>0 Then Begin
           Dec(VChControl,ISS_CCPeriod);
           MixSmpRate:=VChFreq;
           { * Calculating stepping rates * }
           MixSmpHighStep:=(MixSmpRate Div MixRate);
           { * This is divided into 2 lines, because of an FPC bug (?) :( * }
           MixSmpLowStep:=(MixSmpRate Mod MixRate) Shl 16;
           MixSmpLowStep:=MixSmpLowStep Div MixRate;
{
           If MixSmpLowStep>=$10000 Then WriteLn('EEK LOW! ',ChannelCounter,' ',MixSmpLowStep);

           MixSmpHighStep:=MixSmpHighStep+(MixSmpLowStep Shr 16);
           MixSmpLowStep:=MixSmpLowStep And $0FFFF;
 
           If MixSmpHighStep>=20 Then WriteLn('EEK! ',ChannelCounter,' ',MixSmpHighStep);
}  
          End;

        End;

      End;
    End;
  End;

  { * Clearing mixer buffer * }
  ISS_MixerClearBuffer(MixBufType);

  If (MixBufType And ISS_DevStereo)>0 Then ISS_MixerUpdateBufferSTEREO
                                      Else ISS_MixerUpdateBufferMONO;

  { * Do final buffer clipping, and convert it into the format * }
  { * requested by the output device * }
  ISS_MixerMakeClip(MixBufType);

 End;

End;

Begin
 ISS_MixerOK:=False;
End.
