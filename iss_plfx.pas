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

{ * ISS_PLFX.PAS - Effect handlers for the module player                   }
{             OS - Platform Independent                                    }

{ * >>> NO UNIT HEADER, BECAUSE THIS IS AN INCLUDE FILE!!! <<< * }

{ * >>> E F F E C T  I D  N U M B E R S <<< * }

Const { * Effect Numbers * }

      FXPortaUp      =  1; { * 01-1xx Portamento Up           (MOD) * }
      FXPortaDown    =  2; { * 02-2xx Portamento Down         (MOD) * }
      FXPortaNote    =  3; { * 03-3xx Portamento to Note      (MOD) * }
      FXVibrato      =  4; { * 04-4xy Vibrato                 (MOD) * }
      FXPortVolSlide =  5; { * 05-5xy Portamento+Volume Slide (MOD) * }
      FXVibVolSlide  =  6; { * 06-6xy Vibrato+Volume Slide    (MOD) * }
      FXSetPanning   =  8; { * 08-8xx Set Panning ($0-$80)    (MOD/DMP) * }
      FXSetOffset    =  9; { * 09-9xx Set Sample Offset       (MOD) * }
      FXVolumeSlide  = 10; { * 10-Axy Volume Slide            (MOD) * }
      FXJump         = 11; { * 11-Bxx Jump to pattern         (MOD) * }
      FXSetVolume    = 12; { * 12-Cxx Set Volume              (MOD) * }
      FXPatBreak     = 13; { * 13-Dxx Pattern Break           (MOD) * }
                           { * 14-Exy Extended Effects (36+x)       * }
      FXSetTempoBPM  = 15; { * 15-Fxx Set Speed/Set BPM       (MOD) * }
      FXSetGVolume   = 16; { * 16-Gxx Set Global Volume       (S3M/XM) * }
      FXGVolumeSlide = 17; { * 17-Hxx Global Volume Slide     (XM) * }

      FXRetrig       = 45; { * 45-E9x Retrig Note             (MOD) * }
      FXFineVolSldUp = 46; { * 46-EAx Fine Volume Slide Up    (MOD) * }
      FXFineVolSldDn = 47; { * 47-EBx Fine Volume Slide Down  (MOD) * }

      FXCutNote      = 48; { * 48-ECx Cut Note                (MOD) * }
      FXNoteDelay    = 49; { * 49-EDx Delay Note              (MOD) * }

      FXKeyOff       = 97; { * Key Off * }

      FXNoEffect     = 255; { * No effect * }

      { * Volume Commands * }

      FXVolSlideDn   =  1; { * Volume Slide Down * }
      FXVolSlideUp   =  2; { * Volume Slide Up * }
      FXVolFineSldDn =  3; { * Fine Volume Slide Down * }
      FXVolFineSldUp =  4; { * Fine Volume Slide Up * }
      FXVolSetVibSpd =  5; { * Set vibrato speed * }
      FXVolVibrato   =  6; { * Vibrato * }
      FXVolSetPan    =  7; { * Set Panning * }
      FXVolPanSlideL =  8; { * Panning Slide Left * }
      FXVolPanSlideR =  9; { * Panning Slide Right * }
      FXVolPortaNote = 10; { * Porta To Note * }

      FXVolNoEffect  = 255; { * No effect * }


{ * >>> V O L U M E  E F F E C T  S U S T A I N <<< * }

{ * Command : Do Volume Slide Down * }
Procedure DoVVolumeSlideDn;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     If ChVolume<ChVVolSlideData Then ChVolume:=0
                                 Else Dec(ChVolume,ChVVolSlideData);
     ISS_SetVolume(CChannel,ChVolume);
    End;
  End;
End;

{ * Command : Do Volume Slide Up * }
Procedure DoVVolumeSlideUp;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     Inc(ChVolume,ChVVolSlideData);
     If ChVolume>64 Then ChVolume:=64;
     ISS_SetVolume(CChannel,ChVolume);
    End;
  End;
End;


{ * Command : Do Panning Slide Left * }
Procedure DoVPanningSlideLeft;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     If ChPanning<ChVPanSlideData Then ChPanning:=0
                                  Else Dec(ChPanning,ChVPanSlideData);
     ISS_SetPanning(CChannel,ChPanning);
    End;
  End;
End;

{ * Command : Do Panning Slide Right * }
Procedure DoVPanningSlideRight;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     If ChPanning+ChVPanSlideData>255 Then ChPanning:=255
                                      Else Inc(ChPanning,ChVPanSlideData);
     ISS_SetPanning(CChannel,ChPanning);
    End;
  End;
End;

{ * >>> V O L U M E  E F F E C T  P R O C E S S I N G <<< * }

{ * Command : Process Volume Slide Up or Down * }
Procedure ProcVVolumeSlide(VolToSlide : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     ChVVolSlideData:=VolToSlide;
    End;
  End;
End;

{ * Command : Process Volume Panning * }
Procedure ProcVSetPanning(PanToSet : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     ChPanning:=(PanToSet Shl 4)+PanToSet;
     ISS_SetPanning(CChannel,ChPanning);
    End;
  End;
End;

{ * Command : Process Volume Panning Slide * }
Procedure ProcVPanningSlide(PanToSlide : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     ChVPanSlideData:=PanToSlide;
    End;
  End;
End;


{ * >>> E F F E C T  S U S T A I N <<< * }

{ * Command : Do Portamento Up   * }
Procedure DoPortaUp;
Var Buf1,Buf2 : DWord;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     Buf1:=ChPortaUpData;
     Buf1:=Buf1 Shl 2;
     Buf2:=ChPeriod;
     Dec(Buf2,Buf1);
     ISS_SetPeriod(CChannel,Buf2);
     ChPeriod:=Buf2;
    End;
  End;
End;

{ * Command : Do Portamento Down * }
Procedure DoPortaDown;
Var Buf1,Buf2 : DWord;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     Buf1:=ChPortaDownData;
     Buf1:=Buf1 Shl 2;
     Buf2:=ChPeriod;
     Inc(Buf2,Buf1);
     ISS_SetPeriod(CChannel,Buf2);
     ChPeriod:=Buf2;
    End;
  End;
End;

{ * Command : Do Porta To Note   * }
Procedure DoPortaNote;
Var Buf1,Buf2,Buf3 : DWord;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     Buf1:=ChPortaToNoteData;
     Buf1:=Buf1 Shl 2;
     Buf2:=ChPeriod;
     Buf3:=ChPortaToNotePeriod;
     If (Buf3<>0) And (Buf3<>Buf2) Then Begin
       If Buf2<=Buf3 Then Begin
         Inc(Buf2,Buf1);
         If Buf2<=Buf3 Then Begin
           ISS_SetPeriod(CChannel,Buf2);
           ChPeriod:=Buf2;
           Exit;
          End;
          Buf2:=Buf3;
        End;
       If Buf2<=Buf1 Then Buf2:=Buf3
                     Else Begin
                      Buf2:=Buf2-Buf1;
                      If Buf2<Buf3 Then Buf2:=Buf3;
                     End;
       ISS_SetPeriod(CChannel,Buf2);
       ChPeriod:=Buf2;
      End;
    End;
  End;
End;

{ * Command : Do Vibrato * }
Procedure DoVibrato;
Var BufPeriod   : DWord;
    BufVibValue : Byte;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     BufPeriod:=ChPeriod;
     BufVibValue:=((ISS_SineTable[ChVibPosition And 127]*ChVibDepth) Div 128);
     If ChVibPosition>127 Then Inc(BufPeriod,BufVibValue)
                          Else Dec(BufPeriod,BufVibValue);

     ISS_SetPeriod(CChannel,BufPeriod);

     Inc(ChVibPosition,ChVibSpeed*4);

    End;
  End;
End;

{ * Command : Do Volume Slide * }
Procedure DoVolumeSlide;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     ChVolume:=ChVolume+ChVolSlideData;

     If ChVolume>64 Then ChVolume:=64;
     If ChVolume<0 Then ChVolume:=0;
     ISS_SetVolume(CChannel,ChVolume);

    End;
  End;
End;

{ * Command : Do Retrig  * }
Procedure DoRetrig;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     If ChFXTick=0 Then Exit;

     If (TickCnt Mod ChFXTick)=0 Then Begin
       ISS_SetSampleOffset(CChannel,0); { * Retrig * }
      End;

    End;
  End;
End;

{ * Command : Do Portamento+VolumeSlide * }
Procedure DoPortVolSlide;
Begin
 DoPortaNote;
 DoVolumeSlide;
End;

{ * Command : Do Vibrato+VolumeSlide * }
Procedure DoVibVolSlide;
Begin
 DoVibrato;
 DoVolumeSlide;
End;

{ * Command : Do Global Volume Slide * }
Procedure DoGVolumeSlide;
Var BufVol : LongInt;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     BufVol:=ISS_GlobalPlVolume+ChGVolSlideData;

     If BufVol<0  Then Begin ISS_GlobalPlVolume:=0;  Exit; End;
     If BufVol>64 Then Begin ISS_GlobalPlVolume:=64; Exit; End;

     ISS_GlobalPlVolume:=BufVol;

    End;
  End;
End;


{ * Command : Cut Note * }
Procedure DoCutNote;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     If TickCnt=ChFXTick Then Begin
       ChVolume:=0;
       ISS_SetVolume(CChannel,ChVolume); { * Cut Note * }
      End;

    End;
  End;
End;

{ * Command : Do Note Delay * }
Procedure DoNoteDelay;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     { * If Tick counter reached specified value, we start the note * }
     If TickCnt=ChFXTick Then Begin
       CNote :=ChRNote;
       CInstr:=ChRInstr;
       If CNote<>0 Then Begin
         ISSPlay_StartNote;
        End;
      End;

    End;
  End;
End;


{ * >>> E F F E C T  P R O C E S S I N G <<< * }

{ * Command : Process Portamento Up * }
Procedure ProcPortaUp(PortaTo : Word);
Begin
 With ISS_Player^ Do Begin
   If PortaTo<>0 Then Channels[CChannel].ChPortaUpData:=PortaTo;
  End;
End;

{ * Command : Process Portamento Down * }
Procedure ProcPortaDown(PortaTo : Word);
Begin
 With ISS_Player^ Do Begin
   If PortaTo<>0 Then Channels[CChannel].ChPortaDownData:=PortaTo;
  End;
End;

{ * Command : Process Porta To Note * }
Procedure ProcPortaNote(PortaTo : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin;
     If PortaTo<>0 Then
       ChPortaToNoteData:=PortaTo;
     If CNote<>0 Then
       ChPortaToNotePeriod:=ISS_ChGetNotePeriod(CChannel,CNote);
    End;
  End;
End;

{ * Command : Process Vibrato * }
Procedure ProcVibrato(VibValues : Word);
Var VibSpeed : Byte;
    VibDepth : Byte;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     VibSpeed:=(VibValues And $0F0) Shr 4;
     VibDepth:=VibValues And $00F;

     { * If a parameter is zero, we're using previous values * }
     If VibSpeed>0 Then ChVibSpeed:=VibSpeed;
     If VibDepth>0 Then ChVibDepth:=VibDepth;

     { * If previous command wasn't vibrato and we're in retrigger * }
     { * mode, then retrig vibrato * }
     If (ChFXType<>FXVibrato) And (ChFXType<>FXVibVolSlide) And
        (ChVibWaveForm<4) Then ChVibPosition:=0;

     { * Vibrato processed in the head tick too * }
     DoVibrato;

    End;
  End;
End;

{ * Command : Process Set Panning * }
Procedure ProcSetPanning(PanToSet : Word);
Begin
 With ISS_Player^ Do Begin
   ISS_SetPanning(CChannel,PanToSet);
  End;
End;

{ * Command : Process Sample Offset * }
Procedure ProcSetSampOffset(OffsetToSet : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     { * Store offset if it it's not zero * }
     If OffsetToSet<>0 Then ChSampleOffset:=OffsetToSet;

     { * Set the sample offset. If the current parameter is zero, we * }
     { * using the previous value * }
     If CNote<>0 Then ISS_SetSampleOffset(CChannel,ChSampleOffset * $100);

    End;
  End;
End;

{ * Command : Process Volume Slide * }
Procedure ProcVolumeSlide(VolToSlide : Word);
Var VolSlideUp : Byte;
    VolSlideDn : Byte;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     VolSlideUp:=(VolToSlide And $0F0) Shr 4;
     VolSlideDn:=VolToSlide And $00F;

     { * Up & Downslide can't performed together, so we do nothing * }
     { * in this case * }
     If (VolSlideUp>0) And (VolSlideDn>0) Then Exit;

     { * Upslide? * }
     If (VolSlideUp>0) Then Begin
       ChVolSlideData:=VolSlideUp;
      End;

     { * Downslide? * }
     If (VolSlideDn>0) Then Begin
       ChVolSlideData:=0-VolSlideDn;
      End;

    End;
  End;
End;

{ * Command : Process Jump To Pattern * }
Procedure ProcJump(OrderToJump : Word);
Begin
 With ISS_Player^ Do Begin
   With ISS_CurrentModule^ Do Begin
     If OrderToJump>MSongLength-1 Then OrderToJump:=MSongLength-1;
    End;
   NextRow:=0;
   NextOrder:=OrderToJump;
  End;
End;

{ * Command : Process Set Volume * }
Procedure ProcSetVolume(VolumeToSet : Word);
Begin
 With ISS_Player^ Do Begin
   Channels[CChannel].ChVolume:=VolumeToSet;
   ISS_SetVolume(CChannel,VolumeToSet);
  End;
End;

{ * Command : Process Pattern Break * }
Procedure ProcPatBreak(RowToBreak : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     NextRow:=(((RowToBreak And $0F0) Shr 4)*10)+(RowToBreak And $00F);
     If NextOrder=-1 Then NextOrder:=Order+1;

    End;
  End;
End;

{ * Command : Process Portamento+VolumeSlide * }
Procedure ProcPortVolSlide(VolToSlide : Word);
Begin
 ProcPortaNote(0);
 ProcVolumeSlide(VolToSlide);
End;

{ * Command : Process Vibrato+VolumeSlide * }
Procedure ProcVibVolSlide(VolToSlide : Word);
Begin
 ProcVibrato(0);
 ProcVolumeSlide(VolToSlide);
End;

{ * Command : Process Set BPM * }
Procedure ProcSetBPM(BPMToSet : Word);
Begin
 If BPMToSet=0 Then Exit;
 With ISS_Player^ Do Begin
   BPM   :=BPMToSet;
   BPMVal:=(1193180*5) Div (BPMToSet Shl 1);
  End;
End;

{ * Command : Process Set Tempo/BPM * }
Procedure ProcSetTempo_BPM(TempoToSet : Word);
Begin
 If TempoToSet>=$20 Then Begin ProcSetBPM(TempoToSet); Exit; End;
 ISS_Player^.Speed:=TempoToSet;
End;

{ * Command : Process Set Global Volume * }
Procedure ProcSetGlobalVolume(VolToSet : Word);
Begin
 If VolToSet>64 Then ISS_GlobalPlVolume:=64
                Else ISS_GlobalPlVolume:=VolToSet;
End;

{ * Command : Process Global Volume Slide * }
Procedure ProcGVolumeSlide(VolToSlide : Word);
Var VolSlideUp : Byte;
    VolSlideDn : Byte;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     VolSlideUp:=(VolToSlide And $0F0) Shr 4;
     VolSlideDn:=VolToSlide And $00F;

     { * Up & Downslide can't performed together, so we do nothing * }
     { * in this case * }
     If (VolSlideUp>0) And (VolSlideDn>0) Then Exit;

     { * Upslide? * }
     If (VolSlideUp>0) Then Begin
       ChGVolSlideData:=VolSlideUp;
      End;

     { * Downslide? * }
     If (VolSlideDn>0) Then Begin
       ChGVolSlideData:=0-VolSlideDn;
      End;

    End;
  End;
End;

{ * Command : Process Fine Volume Slide Up * }
Procedure ProcFineVolSldUp(VolToAdd : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     Inc(ChVolume,VolToAdd);
     If ChVolume>64 Then ChVolume:=64;
     ISS_SetVolume(CChannel,ChVolume);
    End;
  End;
End;

{ * Command : Process Fine Volume Slide Down * }
Procedure ProcFineVolSldDn(VolToSub : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     If ChVolume<VolToSub Then ChVolume:=0
                          Else Dec(ChVolume,VolToSub);
     ISS_SetVolume(CChannel,ChVolume);
    End;
  End;
End;

{ * Command : Cut Note * }
Procedure ProcCutNote(TickUntilCut : Word);
Begin
 With ISS_Player^ Do Begin
   Channels[CChannel].ChFXTick:=TickUntilCut;
  End;
End;

{ * Command : Note Delay * }
Procedure ProcNoteDelay(TickUntilNote : Word);
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin
     If CNote<>0 Then Begin
       If TickUntilNote=0 Then ChFXTick:=Speed+1
                          Else ChFXTick:=TickUntilNote;
      End;
    End;
  End;
 DoNoteDelay;
End;

{ * Process Tick Based Commands (NoteCut, etc) * }
Procedure ProcTick(Parameter : Word);
Begin
 With ISS_Player^ Do Begin
   Channels[CChannel].ChFXTick:=Parameter;
   ISSPlay_DoFX(CFXType);
  End;
End;

{ * >>> T H E  D U M M Y  P R O C E D U R E S <<< * }

Procedure ProcDummy(Parameter : Word);
Begin
End;

Procedure DoDummy;
Begin
End;

{ * >>> E F F E C T  P R O C E D U R E  T A B L E S <<< * }

Type  TFXHandler = Record
        Proc : Procedure(FXParam : Word);
        Sust : Procedure;
       End;

Const { * The processing table * }
      FXProcs : Array[0..51] Of TFXHandler = (

 { * Normal effects * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 00-0xx Arpeggio               !(MOD) * }
 (Proc:@ProcPortaUp;        Sust:@DoPortaUp),         { * 01-1xx Portamento Up           (MOD) * }
 (Proc:@ProcPortaDown;      Sust:@DoPortaDown),       { * 02-2xx Portamento Down         (MOD) * }
 (Proc:@ProcPortaNote;      Sust:@DoPortaNote),       { * 03-3xx Portamento to Note      (MOD) * }
 (Proc:@ProcVibrato;        Sust:@DoVibrato),         { * 04-4xy Vibrato                 (MOD) * }
 (Proc:@ProcPortVolSlide;   Sust:@DoPortVolSlide),    { * 05-5xy Portamento+Volume Slide (MOD) * }
 (Proc:@ProcVibVolSlide;    Sust:@DoVibVolSlide),     { * 06-6xy Vibrato+Volume Slide    (MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 07-7xy Tremolo                !(MOD) * }
 (Proc:@ProcSetPanning;     Sust:@DoDummy),           { * 08-8xx Set Panning ($0-$80)    (MOD/DMP) * }
 (Proc:@ProcSetSampOffset;  Sust:@DoDummy),           { * 09-9xx Set Sample Offset       (MOD) * }
 (Proc:@ProcVolumeSlide;    Sust:@DoVolumeSlide),     { * 10-Axy Volume Slide            (MOD) * }
 (Proc:@ProcJump;           Sust:@DoDummy),           { * 11-Bxx Jump to pattern         (MOD) * }
 (Proc:@ProcSetVolume;      Sust:@DoDummy),           { * 12-Cxx Set Volume              (MOD) * }
 (Proc:@ProcPatBreak;       Sust:@DoDummy),           { * 13-Dxx Pattern Break           (MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 14-Exy Extended Effects (36+x)       * }
 (Proc:@ProcSetTempo_BPM;   Sust:@DoDummy),           { * 15-Fxx Set Speed/Set BPM       (MOD) * }
 (Proc:@ProcSetGlobalVolume;Sust:@DoDummy),           { * 16-Gxx Set Global Volume       (S3M/XM) * }
 (Proc:@ProcGVolumeSlide;   Sust:@DoGVolumeSlide),    { * 17-Hxx Global Volume Slide     (XM)  * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 18-I                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 19-J                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 20-Kxx Key Off (after xx tick)!(XM)  * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 21-Lxx Set Envelope Pos       !(XM)  * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 22-M                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 23-N                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 24-O                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 25-Pxx Panning Slide          !(XM)  * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 26-Q                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 27-Rxy Multi Retrig Note      !(S3M) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 28-S                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 29-Txy Tremor                 !(STM) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 30-U                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 31-V                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 32-W                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 33-Xxy Extra Fine Portamento  !(S3M) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 31-Y                                 * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 32-Z                                 * }
 { * Extended effects * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 36-E0x Set AMiGA Filter       #(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 37-E1x Fine Portamento Up     !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 38-E1x Fine Portamento Down   !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 39-E3x Glissando Control      #(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 40-E4x Set Vibrato Waveform   !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 41-E5x Set Finetune           !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 42-E6x Pattern Loop           !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 43-E7x Set Tremolo WaveForm   !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 44-E8x Unused/Set Panning     !(MOD/S3M) * }
 (Proc:@ProcTick;           Sust:@DoRetrig),          { * 45-E9x Retrig Note             (MOD) * }
 (Proc:@ProcFineVolSldUp;   Sust:@DoDummy),           { * 46-EAx Fine Volume Slide Up    (MOD) * }
 (Proc:@ProcFineVolSldDn;   Sust:@DoDummy),           { * 47-EAx Fine Volume Slide Down  (MOD) * }
 (Proc:@ProcCutNote;        Sust:@DoCutNote),         { * 48-ECx Cut Note                (MOD) * }
 (Proc:@ProcNoteDelay;      Sust:@DoNoteDelay),       { * 49-EDx Delay Note              (MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy),           { * 50-EEx Pattern Delay          !(MOD) * }
 (Proc:@ProcDummy;          Sust:@DoDummy));          { * 51-EFx Invert Loop, Synchro   #(MOD) * }

Const { * The volume effect processing table * }
      FXVolProcs : Array[0..10] Of TFXHandler = (

 (Proc:@ProcDummy;          Sust:@DoDummy),              { * 0 - * }
 (Proc:@ProcVVolumeSlide;   Sust:@DoVVolumeSlideDn),     { * 1 - Volume Slide Down        * }
 (Proc:@ProcVVolumeSlide;   Sust:@DoVVolumeSlideUp),     { * 2 - Volume Slide Up          * }
 (Proc:@ProcFineVolSldDn;   Sust:@DoDummy),              { * 3 - Fine Volume Slide Down   * }
 (Proc:@ProcFineVolSldUp;   Sust:@DoDummy),              { * 4 - Fine Volume Slide Up     * }
 (Proc:@ProcDummy;          Sust:@DoDummy),              { * 5 - Set vibrato speed      ! * }
 (Proc:@ProcDummy;          Sust:@DoDummy),              { * 6 - Vibrato                ! * }
 (Proc:@ProcVSetPanning;    Sust:@DoDummy),              { * 7 - Set Panning              * }
 (Proc:@ProcVPanningSlide;  Sust:@DoVPanningSlideLeft),  { * 8 - Panning Slide Left     ! * }
 (Proc:@ProcVPanningSlide;  Sust:@DoVPanningSlideRight), { * 9 - Panning Slide Right    ! * }
 (Proc:@ProcDummy;          Sust:@DoDummy));             { * A - Porta to Note          ! * }
