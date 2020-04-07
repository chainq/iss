{
  Copyright (c) 1998-2003,2014  Karoly Balogh <charlie@amigaspirit.hu>

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

{$INCLUDE ISS_SET.INC}
{$WARNINGS ON}
{$MODE FPC}
{$IFDEF CPUI386}
{$INCLUDE ISS_MX86.PAS}
{$ELSE CPUI386}
{$IFDEF CPUM68K}
{$INCLUDE ISS_M68K.PAS}
{$ELSE CPUM68K}
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

Procedure ISS_MixerClearBuffer(OutputType : Word);
Var tmpBufSize : DWord;
Begin
  tmpBufSize:=ISS_MixerData^.MixBufSize*2;
  If (OutputType And ISS_DevStereo)>0 Then tmpBufSize:=tmpBufSize Shl 1;
  If (OutputType And ISS_Dev16Bit)>0 Then tmpBufSize:=tmpBufSize Shl 1;
  FillChar(ISS_MixerData^.MixBufPtr^,tmpBufSize,#0);
End;



{ * Possible buffer conversions * }
{ * 16bit signed -> 16bit signed (no conversion) * }
{ * 16bit signed -> 8bit signed * }
{ * 16bit signed -> 8bit unsigned * }

Procedure ISS_MixerMakeClip(OutputType : Word);
Type PInteger=^Integer;
Var tmpBufSize : DWord;
    tmpLeft    : LongInt;
    tmpRight   : LongInt;
    counter    : DWord;
    tmpLeftpre : LongInt; // used for noise reduction
    tmpRightpre: LongInt;
Begin
  tmpLeftpre:=0;
  tmpRightpre:=0;
  With ISS_MixerData^ Do Begin
    tmpBufSize:=MixBufSize;
    If (OutputType And ISS_DevStereo)>0 Then tmpBufSize:=tmpBufSize Shl 1;
 
    If (OutputType And ISS_Dev16Bit)>0 Then Begin
      For counter:=0 To (tmpBufSize div 2)-1 Do Begin
        tmpLeft:=PLongInt(MixBufPtr)[(counter*2)];
        tmpLeft+=tmpLeftPre;
        tmpLeftpre:=tmpLeft Mod 1024; 
        tmpLeft:=tmpLeft Div 1024;
        tmpRight:=PLongInt(MixBufPtr)[(counter*2)+1];
        tmpRight+=tmpRightpre;
        tmpRightpre:=tmpRightpre Mod 1024;
        tmpRight:=tmpRight Div 1024;

        If (tmpLeft>32767) Then Begin
          tmpLeft:=32767;
         End Else Begin
          If (tmpLeft<-32768) Then tmpLeft:=-32768;
         End;

        // byte swap
//        tmpLeft:=Lo(Word(tmpLeft))*256+Hi(Word(tmpLeft));
        tmpLeft:=(tmpLeft And $FFFF) Shl 16;

        If (tmpRight>32767) Then Begin
          tmpRight:=32767;
         End Else Begin
          If (tmpRight<-32768) Then tmpRight:=-32768;
         End;

//        tmpRight:=Lo(Word(tmpRight))*256+Hi(Word(tmpRight));
        tmpRight:=(tmpRight And $FFFF);

        PDWord(MixBufOffs)[counter]:=tmpLeft or tmpRight;
       End;
     End Else Begin
     End;
   End;
End;


{ * >>> S T E R E O  M I X I N G  F U N C T I O N S <<< * }

{ * Mixes a 8bit sample into a stereo buffer * }
Function MixChannel8BitSampleSTEREO(MChIndexPtr : Pointer) : DWord;
Var Counter   : DWord;
    tmpSmp    : LongInt;
    tmpSmpL   : LongInt;
    tmpSmpR   : LongInt;
    tmpSmpInc : DWord;
    tmpSmpPre : LongInt;
    tmpSmpStep: LongInt;
Begin
  tmpSmpPre:=0;
  With ISS_MixerData^ Do Begin
    With ISS_TMixChannel(MChIndexPtr^) Do Begin
      MixChannel8BitSampleSTEREO:=MixBufSize;

      tmpSmpPre:=PShortInt(MixSmpPtr^.SDRAMOffs)[MixSmpPos]*256;

      For Counter:=0 To (MixBufSize)-1 Do Begin
 
          { * Mixing Sample Value * }
          tmpSmp:=PShortInt(MixSmpPtr^.SDRAMOffs)[MixSmpPos]*256;
          tmpSmpL:=tmpSmp*MixSmpVolL;
          tmpSmpR:=tmpSmp*MixSmpVolR;
          PLongInt(MixBufPtr)[(Counter*2)]+=tmpSmpL;
          PLongInt(MixBufPtr)[(Counter*2)+1]+=tmpSmpR;

          { * Increasing Sample Position * }
          tmpSmpInc:=MixSmpInc+MixSmpLowStep;
          MixSmpInc:=tmpSmpInc And $0FFFF;
          MixSmpPos+=((tmpSmpInc Shr 16)+MixSmpHighStep);

          { * Checking for Sample Bounds * }
          If MixSmpPos>MixSmpEnd Then Begin
            With MixSmpPtr^ Do Begin
              If (SType And ISS_SmpPingPongLoop)>0 Then Begin
                MixSmpPos:=SLoopStart;
               End Else Begin
                MixSmpPos:=SLength;
                MixChannel8BitSampleSTEREO:=$10000;
                Exit;
               End;
             End;
           End;

       End;
     End;    
   End;
End;

{ * Mixes a 16bit sample into a stereo buffer * }
Function MixChannel16BitSampleSTEREO(MChIndexPtr : Pointer) : DWord;
Type PInteger = ^Integer;
Var Counter   : DWord;
    tmpSmp    : LongInt;
    tmpSmpL   : LongInt;
    tmpSmpR   : LongInt;
    tmpSmpInc : DWord;
Begin
  With ISS_MixerData^ Do Begin
    With ISS_TMixChannel(MChIndexPtr^) Do Begin
      MixChannel16BitSampleSTEREO:=MixBufSize;
      For Counter:=0 To (MixBufSize)-1 Do Begin
 
          { * Mixing Sample Value * }
          tmpSmp:=PInteger(MixSmpPtr^.SDRAMOffs)[MixSmpPos Div 2];
          tmpSmpL:=tmpSmp*MixSmpVolL;
          tmpSmpR:=tmpSmp*MixSmpVolR;
          PLongInt(MixBufPtr)[(Counter*2)]+=tmpSmpL;
          PLongInt(MixBufPtr)[(Counter*2)+1]+=tmpSmpR;

          { * Increasing Sample Position * }
          tmpSmpInc:=MixSmpInc+MixSmpLowStep;
          MixSmpInc:=tmpSmpInc And $0FFFF;
          MixSmpPos+=(((tmpSmpInc Shr 16)+MixSmpHighStep)*2);

          { * Checking for Sample Bounds * }
          If MixSmpPos>MixSmpEnd Then Begin
            With MixSmpPtr^ Do Begin
              If (SType And ISS_SmpPingPongLoop)>0 Then Begin
                MixSmpPos:=SLoopStart;
               End Else Begin
                MixSmpPos:=SLength;
                MixChannel16BitSampleSTEREO:=$10000;
                Exit;
               End;
             End;
           End;

       End;
     End;    
   End;
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
Function MixChannel8BitSampleMONO(MChIndexPtr : Pointer) : DWord;
Var Counter   : DWord;
    tmpSmp    : LongInt;
    tmpSmpInc : DWord;
Begin
  With ISS_MixerData^ Do Begin
    With ISS_TMixChannel(MChIndexPtr^) Do Begin
      MixChannel8BitSampleMONO:=MixBufSize;
      For Counter:=0 To (MixBufSize)-1 Do Begin
 
          { * Mixing Sample Value * }
          tmpSmp:=PShortInt(MixSmpPtr^.SDRAMOffs)[MixSmpPos]*256;
          tmpSmp:=tmpSmp*MixSmpVol;
          PLongInt(MixBufPtr)[Counter]+=tmpSmp;

          { * Increasing Sample Position * }
          tmpSmpInc:=MixSmpInc+MixSmpLowStep;
          MixSmpInc:=tmpSmpInc And $0FFFF;
          MixSmpPos+=((tmpSmpInc Shr 16)+MixSmpHighStep);

          { * Checking for Sample Bounds * }
          If MixSmpPos>MixSmpEnd Then Begin
            With MixSmpPtr^ Do Begin
              If (SType And ISS_SmpPingPongLoop)>0 Then Begin
                MixSmpPos:=SLoopStart;
               End Else Begin
                MixSmpPos:=SLength;
                MixChannel8BitSampleMONO:=$10000;
                Exit;
               End;
             End;
           End;

       End;
     End;    
   End;
End;



{ * Mixes a 16bit sample into a mono buffer * }
Function MixChannel16BitSampleMONO(MChIndexPtr : Pointer) : DWord;
Begin
  MixChannel16BitSampleMONO:=$10000;
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
{$ENDIF}
{$ENDIF}
