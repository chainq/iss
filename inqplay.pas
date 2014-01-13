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

{ * Inquisition's Module Player * }
{ * Example Player for ISS * }
{ * By Charlie/iNQ * }

{$IFDEF GO32V2}
 { DEFINE REFRESHMETER} { * Dirty hack, for debugging only. Do not enable. * }
{$ENDIF}

{$ASMMODE INTEL}
{$MODE FPC}
Program INQPlay;

Uses Crt
     {$IFDEF GO32V2}
      ,GO32
     {$ENDIF}
     {$IFDEF OS2}
      ,DOSCalls
     {$ENDIF}
      ,ISS_Load,ISS_Var,ISS_Sys,ISS_Play,ISS_Mix
     {$IFDEF REFRESHMETER}
      ,ISS_Tim
     {$ENDIF}
     ;

Const NoteStrTable   : Array[0..11] Of String[2] =
      ('C-','C#','D-','D#','E-','F-','F#','G-','G#','A-','A#','B-');
      FXStrTable : Array[0..17] Of String[6] =
      ('appreg','portau','portad','portan','vibrat','pn+vls','vibvls',
       'tremol','setpan','offset','volsld','posjmp','setvol','patbrk',
       '?-??-?','setspd','globvl','gvolsl');

      EFXStrTable : Array[0..14] Of String[6] =
      ('filter','fportu','fportd','glissc','vibrct','finetn','stloop',
       'tremct','?-??-?','retrig','fvolsu','fvolsd','notcut','delayn',
       'pdelay');

      VFXStrTable : Array[1..10] Of String[6] =
      ('vlsldd','vlsldu','fnsldd','fnsldu','vibspd','vibrat','setpan',
       'pnsldl','pnsldr','portan');

      ChVolumeColMap : Array[0..15] Of Byte =
      (10,10,10,10,10,10,10,10,10,10,10,14,14,14,12,12);

      DefaultFileName = '';

      INQPlayerVersionStr    = '0.3.0';
      AllowFileNameParameter = True;

Var ModFileName : String;
    ModFilePtr  : ISS_PModule;

    {$IFDEF REFRESHMETER}
     RefreshMeterProc  : Pointer;
     RefreshMeterValue : DWord;
     UpdFPS        : DWord;
    {$ENDIF}

{$IFDEF GO32V2}
Procedure InitVideoMode; Assembler;
Asm
  mov ax,3h
  int 10h
  mov ax,1112h
  xor dx,dx
  mov bx,800h
  mov cx,256
  int 10h
End;
{$ENDIF}

Function WriteHex(Num : Word) : String[2];
Const
  DigitTab : String[16]='0123456789ABCDEF';
Var
  HexStr  : String[2];
  Counter : Integer;
Begin
  HexStr:='';
  For Counter:=1 DownTo 0 Do Begin
    HexStr:=HexStr+DigitTab[(Num And ($F Shl (Counter*4))) Shr (Counter*4)+1];
   End;
  WriteHex:=HexStr;
End;

{$IFDEF GO32V2}
Procedure Timeslice;
Var Regs : TRealRegs;
Begin
 Regs.AX:=$1680;
 RealIntr($2F,Regs);
End;
{$ENDIF}

Procedure InitPlayer;

 Procedure CheckInternalName;
 Begin
  If DefaultFileName='' Then Begin
    WriteLn('INQ_PLAY: WARNING! No Internal Default Module Name Specified!');
    WriteLn('INQ_PLAY: ERROR! NO MODULE NAME SPECIFIED!');
    Halt(1);
   End Else Begin
    ModFileName:=DefaultFileName;
    WriteLn('INQ_PLAY: Using Default Filename: ',ModFileName);
   End;
 End;

Begin
 WriteLn(#13+#10+'INQ_PLAY: Inquisition''s Module Player version ',INQPlayerVersionStr,' for ',ISS_PlatformStr);
 WriteLn;

 If AllowFileNameParameter Then Begin
   If ParamCount=0 Then Begin
     WriteLn('INQ_PLAY: WARNING! No Module Name Specified In Command Line!');
     CheckInternalName;
    End Else Begin
     ModFileName:=ParamStr(1);
     WriteLn('INQ_PLAY: Using Specified Filename: ',ModFileName);
    End;
  End Else Begin
   WriteLn('INQ_PLAY: NOTE - Command Line Interface Disabled.');
   CheckInternalName;
  End;

 { * ISS Main Init * }
 WriteLn;
 ISS_InitSystem;
 WriteLn;

 If Not ISS_LoadModule(ModFileName,ModFilePtr) Then Begin
   WriteLn('INQ_PLAY: ERROR! FAILED TO LOAD MODULE!');
   Halt(1);
  End Else Begin
   WriteLn('INQ_PLAY: Module Successfully Loaded.');
  End;

 WriteLn('INQ_PLAY: Selecting Sound Device Automatically...');
 ISS_AutoSetup;
 ISS_Init;

End;

Procedure ClearPlayerScreen;
Begin
 TextColor(LightGray); TextBackGround(Black);
 ClrScr;
 TextColor(White); TextBackGround(Blue); ClrEol;
 Write(' * Inquisition''s Module Player version ',INQPlayerVersionStr,' for ',ISS_PlatformStr,' by Charlie/iNQ');

 GotoXY(1,50); ClrEol;
 With ISS_SoundDevice^ Do Begin
   Write(' * Output: ',DevName,' (',DevMixRate,'hz, ');
   If Boolean(DevType And ISS_Dev16Bit) Then Write('16bit') Else Write('8bit');
   Write(', ');
   If Boolean(DevType And ISS_DevStereo) Then Write('stereo') Else Write('mono');
   Write(', ');
   If Boolean(DevType And ISS_DevMixed) Then Write('using the mixer') Else Write('using wavetable');
   Write(')');
  End;

 Window(1,2,80,49);
 TextColor(LightGray); TextBackGround(Black);
 WriteLn;
End;

Procedure ClosePlayerScreen;
Begin
 TextColor(LightGray); TextBackGround(Black);
 {$IFDEF GO32V2}
  TextMode(CO80);
 {$ENDIF}
 {$IFDEF OS2}
  TextMode(_80cols+_25rows);
 {$ENDIF}
 ClrScr;
 WriteLn('* Inquisition''s Module Player version ',INQPlayerVersionStr,' for ',ISS_PlatformStr,' by Charlie/iNQ');
 WriteLn('* SECOND OFFICIAL RELEASE : Codename "Yavin" [',{$I %DATE%},']');
 WriteLn('* Compiled by : ',{$I %CODER%},' at ',{$I %TIME%});
 WriteLn('* FPC Version : ',{$I %FPCVERSION%});
 WriteLn('* ISS Version : ',ISS_VersionStr);
 WriteLn;
End;

Procedure DrawGlobalVolumeBar;
Var Counter : DWord;
Begin
 TextColor(LightGray);
 Write('VOLUME: Player-[',WriteHex(ISS_GlobalPlVolume),'] * ',
               'System-[',WriteHex(ISS_GlobalSSVolume),'][');
 For Counter:=0 To (ISS_GlobalSSVolume Shr 3) Do Write('*');

 If ISS_GlobalSSVolume<>128 Then Begin
   TextColor(DarkGray);
   For Counter:=(ISS_GlobalSSVolume Shr 3) To 15 Do Write('*');
  End;

 TextColor(LightGray);
 WriteLn('] (+/- to change)');
End;

Procedure DrawChannelEffects(ChannelNum : Word);
Var EffectVal : Word;
    Active : Boolean;
Begin
 EffectVal:=ISS_Player^.Channels[ChannelNum].ChFXType;
 Active:=Boolean(ISS_VirtualChannels^[ChannelNum].VChControl And ISS_CCActive);

 If Active Then TextColor(LightGray);
 Write('[');

 If Active And (EffectVal<255) Then TextColor(White);

 Case EffectVal Of
    0..17 : Write(FXStrTable[EffectVal]);
   36..50 : Write(EFXStrTable[EffectVal-36]);
   Else     Write('------');
  End;
 If Active Then TextColor(LightGray);
 Write('][');

 EffectVal:=ISS_Player^.Channels[ChannelNum].ChVFXType;

 If (EffectVal<=10) And (EffectVal>0) Then Begin
   If Active Then TextColor(White);
   Write(VFXStrTable[EffectVal]);
  End Else
   Write('------');

 If Active Then TextColor(LightGray);
 Write(']');

End;


Procedure DrawChannelVolumeBar(ChannelNum : Word);
Var Counter   : Word;
    VolumeVal : Word;
Begin
 VolumeVal:=ISS_VirtualChannels^[ChannelNum].VChFinalVolume;

 TextColor(LightGray);
 Write('[',WriteHex(VolumeVal),'][');
 For Counter:=0 To 15 Do Begin
   If ((VolumeVal Shr 2)>=Counter) And (VolumeVal<>0) Then Begin
     TextColor(ChVolumeColMap[Counter]);
    End Else Begin
     {TextColor(ChVolumeColMap[Counter+1]-8);}
     TextColor(DarkGray);
    End;
   Write('*');
  End;

{ If VolumeVal>0 Then
   For Counter:=0 To (VolumeVal Shr 2) Do Begin
     TextColor(ChVolumeColMap[Counter]); Write('*');
    End;
 If VolumeVal<64 Then Begin
   For Counter:=(VolumeVal Shr 2) To 15 Do Begin

     TextColor(DarkGray);
     Write('*');
    End;
  End;}

 TextColor(LightGray);
 Write(']');
End;

Procedure DrawChannelPanBar(ChannelNum : Word);
Var PanVal : Byte;
Begin
 PanVal:=ISS_VirtualChannels^[ChannelNum].VChFinalPanning;
 PanVal:=PanVal Shr 5;
 If PanVal=0 Then PanVal:=1;
 Dec(PanVal);

 TextColor(LightGray);
 Write('[-------]');
 GotoXY((WhereX-8)+PanVal,WhereY);
 TextColor(Cyan);
 Write('*');

 TextColor(LightGray);
End;

Procedure DrawChannelInstrInfo(ChannelNum : Word);
Var BufStr : String[22];
Begin
 With ISS_VirtualChannels^[ChannelNum] Do Begin
   GotoXY(57,WhereY);
   TextColor(LightGray);
   Write('[');
   TextColor(White);
   If VChInsAddr<>Nil Then Begin
     BufStr:=VChInsAddr^.IName;
    End;
   Write(BufStr:22);
   TextColor(LightGray);
   Write(']');
  End;
End;

Procedure DrawChannelInfos;
Var Counter : Word;
Begin
 For Counter:=0 To ModFilePtr^.MChannels-1 Do Begin

   With ISS_VirtualChannels^[Counter] Do Begin
     With ISS_Player^ Do Begin
       With Channels[Counter] Do Begin

         GotoXY(1,13+Counter);
         If Boolean(VChControl And ISS_CCActive) Then Begin

           TextColor(LightGray); Write('[',Counter:2,'][');
           TextColor(White); Write(NoteStrTable[(ChNote-1) Mod 12],(ChNote Div 12));
           TextColor(LightGray); Write(']');

           DrawChannelEffects(Counter);
           DrawChannelVolumeBar(Counter);
           DrawChannelPanBar(Counter);
           DrawChannelInstrInfo(Counter);

          End Else Begin

           TextColor(DarkGray);
           Write('[',Counter:2,'][---]');
           DrawChannelEffects(Counter);
           Write('[--][****************][-------][',' ':22,']');

          End;

{ * Debug code rules :) * }
{          GotoXY(60,13+Counter);}
{          Write(ChNote:3);}
{          Write(VChControl:4);}
{          Write(VChEnvVolPos:3,VChEnvVolPoint:3,VChControl:4,VChDebug1:2);}
{          Write(ChVibSpeed:3,ChVibDepth:3,ChVibPosition:3);}
{          Write(VChAVibPos:3,VChAVibSwpPos:4,VChAVibPitch:4);}
{          Write(VChPeriod:5,VChFreq:6);}
{            If VChSmpAddr<>Nil Then Begin
             With VChSmpAddr^ Do Begin
               Write(SDRAMOffs:8,' ',(SType And %00000100));
              End;
            End;}

{          GotoXY(60,13+Counter);}
{          With ISS_MixerData^.MixChannels[Counter] Do Begin}
{            Write(MixSmpPos:6,MixSmpEnd:6,MixSmpPtr^.SLength:7);}
{            Write(MixSmpVolL:4,MixSmpVolR:4,MixSmpVol:4,MixSmpPan:4);}
{            If MixSmpPtr=Nil Then Write(' ':8) Else Write(MixSmpPtr^.SLength:8);}
{            If MixSmpPtr=Nil Then Write(' ':4) Else Write(MixSmpPtr^.SType And ISS_Smp16BitData:4);}
{            Write(VChFreq:7);}
{           End;}
{ * Debug code rules :) * }


        End;
      End;
    End;

   WriteLn;
  End;
 TextColor(LightGray);
End;

Procedure DrawPlayerHeader;
Begin
 GotoXY(1,2);
 WriteLn('--[Global]----------------------------------------------------------------------');
 WriteLn('MUSIC : Filename-[',ModFileName,'] * Title-[',ModFilePtr^.MTitle,']');
 GotoXY(1,10);
 WriteLn('--[Channels]--------------------------------------------------------------------');
 WriteLn(' Ch  Not    FX     VlFX         Volume          Panning      Instrument Name');
 GotoXY(1,6);
End;

Procedure SuspendMode;
Var Pressed    : Char;
Begin
 While Keypressed Do ReadKey;
 TextColor(Blue);
 TextBackGround(Black);
 Window(1,1,80,50);
 ClrScr;
 GotoXY(3,3);
 WriteLn(' * INQPLAY ',INQPlayerVersionStr,' MULTITASK MODE. PRESS ESC TO RETURN NORMAL MODE.');
 Repeat
  {$IFDEF GO32V2}
   Timeslice;
  {$ENDIF}
  {$IFDEF OS2}
   DosSleep(32);
  {$ENDIF}
  Pressed:=Readkey;
 Until Pressed=#27;
 While Keypressed Do ReadKey;
 ClearPlayerScreen;
 DrawPlayerHeader;
End;


{$IFDEF REFRESHMETER}
Procedure RefreshMeter;
Begin
 UpdFPS:=RefreshMeterValue;
 RefreshMeterValue:=0;
End;

Procedure InstallRefreshMeter;
Begin
 RefreshMeterProc:=@RefreshMeter;
 ISS_StartTimer(RefreshMeterProc,ISS_TimerSpeed);
End;

Procedure DeInstallRefreshMeter;
Begin
 ISS_StopTimer(RefreshMeterProc);
End;
{$ENDIF}

Procedure MainPlayerScreen;
Var ExitPlayer : Boolean;
    Pressed    : Char;
{    Counter    : DWord;}
Begin
 If Not ISS_InitModulePlay(ModFilePtr) Then Begin
   WriteLn('INQ_PLAY: ERROR WHILE LOADING SAMPLES!');
   ISS_FreeModule(ModFilePtr);
   ISS_Done;
   Halt;
  End;

 ISS_StartModulePlay;

 {$IFDEF REFRESHMETER}
  RefreshMeterValue:=0;
  UpdFPS:=0;
  InstallRefreshMeter;
 {$ENDIF}

 {$IFDEF GO32V2}
 TextMode(CO80+Font8x8);
 InitVideoMode;
 CursorOff;
 {$ENDIF}
 {$IFDEF OS2}
 TextMode(_80cols+_50rows);
 {$ENDIF}

 ClearPlayerScreen;
 DrawPlayerHeader;

{ ISS_Player^.Order:=$4;
 ISS_SetOrder($4);}

{ For Counter:=0 To ISS_ActiveSSChannels-1 Do Begin
   ISS_VirtualChannels^[Counter].VChMute:=True;
  End;}

{ ISS_VirtualChannels^[17].VChMute:=False;}
{ ISS_VirtualChannels^[0].VChMute:=False;
 ISS_VirtualChannels^[1].VChMute:=False;}
{ ISS_VirtualChannels^[2].VChMute:=False;
 ISS_VirtualChannels^[3].VChMute:=False;
 ISS_VirtualChannels^[4].VChMute:=False;
 ISS_VirtualChannels^[5].VChMute:=False;
 ISS_VirtualChannels^[6].VChMute:=False;
 ISS_VirtualChannels^[7].VChMute:=False;
 ISS_VirtualChannels^[8].VChMute:=False;
 ISS_VirtualChannels^[9].VChMute:=False;}
{ ISS_VirtualChannels^[13].VChMute:=False;
 ISS_VirtualChannels^[14].VChMute:=False;}

 Repeat
{  ISS_Player^.Order:=$4;}
  With ISS_Player^ Do Begin

    With ModFilePtr^ Do Begin
      WriteLn('STATUS: Speed/BPM-[',Speed:3,'/',BPM:3,
              '] * Order-[',WriteHex(Order),'/',WriteHex(MSongLength-1),
              '] * Pattern-[',WriteHex(Pattern),'/',WriteHex(MPatternNum-1),
              '] * Row-[',WriteHex(Row),'/',WriteHex(Rows),']'+#13+#10);
     End;

    DrawGlobalVolumeBar;
    GotoXY(1,13);
    DrawChannelInfos;
    {$IFDEF REFRESHMETER}
     GotoXY(2,48);
     Write('Screen Refresh Rate: ',UpdFPS:3,' fps    ');
     Inc(RefreshMeterValue);
    {$ENDIF}
    GotoXY(1,6);

    Pressed:=#255;
    If KeyPressed Then Pressed:=ReadKey;
    Case Pressed Of
      #0 : Begin
            Pressed:=ReadKey;
            Case Pressed Of
              #72,#75 : Begin { * Cursor Up * }
                      If ISS_GetOrder>0 Then ISS_SetOrder(Order-1);
                     End;
              #80,#77 : Begin { * Cursor Down * }
                      If ISS_GetOrder<ModFilePtr^.MSongLength-1 Then ISS_SetOrder(Order+1);
                     End;
             End;

           End;

      #77,#109 : Begin { * Entering Multitask mode * }
                  SuspendMode;
                 End;

      #43: If ISS_GlobalSSVolume<128 Then Inc(ISS_GlobalSSVolume,2);
      #45: If ISS_GlobalSSVolume>0   Then Dec(ISS_GlobalSSVolume,2);
      #27: ExitPlayer:=True;
     End;

   End;

  {$IFDEF OS2}
   DosSleep(32); { * Moolteetaskeen' roolz 8) * }
  {$ENDIF}
 Until ExitPlayer;
 GotoXY(1,45);
 WriteLn;

 ClosePlayerScreen;

 ISS_StopModulePlay;

 ISS_DoneModulePlay;

 ISS_FreeModule(ModFilePtr);

 ISS_Done;

 {$IFDEF REFRESHMETER}
  DeInstallRefreshMeter;
 {$ENDIF}

End;

Begin
 InitPlayer;
 MainPlayerScreen;
End.
