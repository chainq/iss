{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ ISS_GUS2.PAS - Device Driver for GF1/Interwave based cards under OS/2  ³}
{³                  via the Manley Drivers native API.                      ³}
{³                  Work started     : 2001.03.25.                          ³}
{³                  Last modification: 2001.06.29.                          ³}
{³             OS - OS/2 only.                                              ³}
{³                                                                          ³}
{³            ISS - Inquisition Sound Server for Free Pascal                ³}
{³                  Code by Karoly Balogh (a.k.a. Charlie/iNQ)              ³}
{³                  Copyright (C) 1998-2001 Inquisition                     ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ This unit is heavily based on the work of Sander van Leeuwen author of ³}
{³   the latest OS/2 GUS2 drivers and Timo Maier, who ported the native API ³}
{³   C include file to Virtual Pascal/2. Thank you.                         ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{$INCLUDE ISS_SET.INC}
{$ASMMODE INTEL}
{$MODE FPC}

{$HINTS OFF} { þ Enable this if you modify the source! þ }
{$NOTES OFF} { þ Enable this if you modify the source! þ }

{$IFDEF _ISS_GUSNATIVE_OLDVOLUMETABLE_}
 {$INFO GUS2 driver will use the old volume table!}
{$ENDIF}

Unit ISS_GUS2;

Interface

Uses ISS_Var, { þ Uses the system variables and types þ }
     OS2Def,  { þ Uses OS/2 system interface þ }
     DOSCalls;{ þ Uses OS/2 DOSCALLS.DLL þ }

Const ISS_GUS2VersionStr = '0.1.0';
      ISS_GUS2Name     = 'OS/2 GF1/Interwave Driver';
      ISS_GUS2LongDesc = 'OS/2 GF1/Interwave Based Device Driver';

Var ISS_GUS2Device : ISS_TSoundDevice; { þ GUS2 Device Structure þ }
    ISS_GUS2Driver : ISS_TSoundDriver; { þ GUS2 Device Driver þ }

Procedure ISS_GUS2DevInit; { þ Inits the GUS2 driver structures þ }

Implementation

Type UltraAllocStruct = Packed Record
       SmpSize     : LongInt; { þ Memory size to allocate þ }
       SmpLocation : LongInt; { þ Location (return value) þ }
       SmpType     : Byte;    { þ 0 = 8bit sample, 4 = 16bit sample þ }
      End;
     UltraFreeStruct  = Packed Record
       SmpSize     : LongInt; { þ Memory size to free þ }
       SmpLocation : LongInt; { þ Location to free þ }
      End;
     UltraXferStruct = Packed Record
       XferControl : Byte;
       XferDRAMLoc : LongInt;
      End;
     UltraTimerStruct = Packed Record
       Timer : Integer; { þ GUS timer interrupt number þ }
       Time  : Byte;    { þ Time between interrupts þ }
      End;
     UltraVolumeStruct = Packed Record
       Voice   : Integer;
       End_idx : word;
       Rate    : byte;
       Mode    : byte;
      End;
     UltraBalanceStruct = Packed Record
       Voice   : Integer;
       Data    : Byte;
      End;
     UltraFreqStruct = Packed Record
       Voice     : Integer;
       Speed_Khz : LongInt;
      End;
     UltraVoiceStruct = Packed Record
       VoiceNum       : Integer; { voice to start }
       VoiceStart     : LongInt;
       VoiceLoopStart : LongInt; { start loop location in ultra DRAM }
       VoiceLoopEnd   : LongInt; { end location in ultra DRAM }
       VoiceMode      : Byte;    { mode to run the voice (loop etc) }
      End;

Const GUS2_Commands = $0F;

      UltraDevStartEffectVoice = $4B;
      UltraDevAddEffectToVoice = $4C;
      UltraDevRemoveEffectFromVoice = $4D;
      UltraDevGetUltraType = $4E;

      UltraDevSetNVoices = $50;
      UltraDevEnableOutput = $51;
      UltraDevDisableOutput = $52;
      UltraDevPokeData = $53;
      UltraDevPeekData = $54;
      UltraDevMemAlloc = $55;
      UltraDevMemFree = $56;
      UltraDevMemInit = $57;
      UltraDevStartTimer = $58;
      UltraDevStopTimer = $59;
      UltraDevBlockTimerHandler1 = $5A;
      UltraDevBlockTimerHandler2 = $5B;
      UltraDevStartVoice = $5C;
      UltraDevStopVoice = $5D;
      UltraDevSetBalance = $5E;
      UltraDevSetFrequency = $5F;
      UltraDevVectorLinearVolume = $60;
      UltraDevPrepare4DMAXfer = $61;
      UltraDevUnblockAll = $62;
      UltraDevSetAll = $63;

      UltraDevGetAccess = $6A;
      UltraDevReleaseAccess = $6B;
      UltraDevSizeDRAM = $6D;
      UltraDevGetRegStatus = $6C;
      UltraDevGetDriverVersion = $6E;
      UltraDevStopAll = $6F;

      UltraDevSetLoopMode = $73;
      UltraDevVoiceStopped = $74;

      UltraDevSetLoopStart = $78;

      { þ Here are the dma to/from DRAM control bit definitions: þ }
      UltraDramRead  = $02;
      UltraDram16Bit = $40;
      UltraDram8Bit  = $80;

Var ISS_GUS2DRAMOffset : DWord;

    ISS_GUS2ActiveChannels : Word;  { þ Number of active hardware channels þ }

    ISS_GUS2Handle         : DWord;
    ISS_GUS2UpdThreadID    : LongInt;
    ISS_GUS2Thread_Exit    : Boolean;
    ISS_GUS2PeriodicCall   : Procedure;

{ þ >>> O S / 2  S Y S T E M  C O N S T A N T S <<< þ }
{ þ The constants here should be added later to the FPC OS/2 RTL (?) þ }

Const { þ DosOpen/DosQFHandState/DosQueryFileInfo et al file attributes; also þ }
      { þ known as Dos File Mode bits... þ }
      FILE_NORMAL    = $0000;
      FILE_READONLY  = $0001;
      FILE_HIDDEN    = $0002;
      FILE_SYSTEM    = $0004;
      FILE_DIRECTORY = $0010;
      FILE_ARCHIVED  = $0020;

      { þ DosOpen() actions þ }
      FILE_EXISTED   = $0001;
      FILE_CREATED   = $0002;
      FILE_TRUNCATED = $0003;

      { þ DosOpen() open flags þ }
      FILE_OPEN     = $0001;
      FILE_TRUNCATE = $0002;
      FILE_CREATE   = $0010;

      { þ DosOpen/DosSetFHandState flags þ }
      OPEN_ACCESS_READONLY        = $0000; { þ ---- ---- ---- -000 þ }
      OPEN_ACCESS_WRITEONLY       = $0001; { þ ---- ---- ---- -001 þ }
      OPEN_ACCESS_READWRITE       = $0002; { þ ---- ---- ---- -010 þ }
      OPEN_SHARE_DENYREADWRITE    = $0010; { þ ---- ---- -001 ---- þ }
      OPEN_SHARE_DENYWRITE        = $0020; { þ ---- ---- -010 ---- þ }
      OPEN_SHARE_DENYREAD         = $0030; { þ ---- ---- -011 ---- þ }
      OPEN_SHARE_DENYNONE         = $0040; { þ ---- ---- -100 ---- þ }
      OPEN_FLAGS_NOINHERIT        = $0080; { þ ---- ---- 1--- ---- þ }
      OPEN_FLAGS_NO_LOCALITY      = $0000; { þ ---- -000 ---- ---- þ }
      OPEN_FLAGS_SEQUENTIAL       = $0100; { þ ---- -001 ---- ---- þ }
      OPEN_FLAGS_RANDOM           = $0200; { þ ---- -010 ---- ---- þ }
      OPEN_FLAGS_RANDOMSEQUENTIAL = $0300; { þ ---- -011 ---- ---- þ }
      OPEN_FLAGS_NO_CACHE         = $1000; { þ ---1 ---- ---- ---- þ }
      OPEN_FLAGS_FAIL_ON_ERROR    = $2000; { þ --1- ---- ---- ---- þ }
      OPEN_FLAGS_WRITE_THROUGH    = $4000; { þ -1-- ---- ---- ---- þ }
      OPEN_FLAGS_DASD             = $8000; { þ 1--- ---- ---- ---- þ }
      OPEN_FLAGS_NONSPOOLED       = $00040000;
      OPEN_FLAGS_PROTECTED_HANDLE = $40000000;

      { þ definitions for DosError - combine with Or þ }
      FERR_DISABLEHARDERR   = $00000000; { þ disable hard error popups þ }
      FERR_ENABLEHARDERR    = $00000001; { þ enable hard error popups  þ }
      FERR_ENABLEEXCEPTION  = $00000000; { þ enable exception popups   þ }
      FERR_DISABLEEXCEPTION = $00000002; { þ disable exception popups  þ }

{ þ Workaround for DosDevIOCtl definition in DOSCALLS.DLL using pointers. þ }
Function DosDevIOCtl(Handle, Category, Func : LongInt;
                     Params: Pointer; ParamLen: LongInt; Var ParamSize: LongInt;
                     Data: Pointer; DataLen: LongInt; Var DataSize: LongInt): LongInt; CDecl;
External 'DOSCALLS' Index 284;


{ þ >>> D E B U G  F U N C T I O N S <<< þ }

{$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
 Function WriteHex(Num : Word) : String[4];
 Const DigitTab : String[16]='0123456789ABCDEF';
 Var HexStr  : String[4];
     Counter : Integer;
 Begin
  HexStr:='';
  For Counter:=3 DownTo 0 Do Begin
    HexStr:=HexStr+DigitTab[(Num And ($F Shl (Counter*4))) Shr
            (Counter*4)+1];
   End;
  WriteHex:=HexStr;
 End;
{$ENDIF}

{ þ >>> I N T E R N A L  F U N C T I O N S <<< þ }


{ þ HW initializating and basic settings þ }

Function UltraGetAccess : LongInt;
var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  UltraGetAccess:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                              UltraDevGetAccess,
                              NIL,0,ParmLength,NIL,0,DataLength);
End;

Function UltraReleaseAccess : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  UltraReleaseAccess:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                  UltraDevReleaseAccess,
                                  NIL,0,ParmLength,NIL,0,DataLength);
End;

Function UltraEnableOutput : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  UltraEnableOutput:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                 UltraDevEnableOutput,
                                 NIL,0,ParmLength,NIL,0,DataLength);
End;

Function UltraDisableOutput : LongInt;
var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  UltraDisableOutput:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                  UltraDevDisableOutput,
                                  NIL,0,ParmLength,NIL,0,DataLength);
End;

Function UltraSetNumVoices(NumVoices : DWord) : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(NumVoices);
  { þ Make sure, voices is in the 14-32 range. þ }
  If NumVoices < 14 Then NumVoices := 14 Else
  If NumVoices > 32 Then NumVoices := 32;
  UltraSetNumVoices:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                 UltraDevSetNVoices,
                                 NIL,0,ParmLength,
                                 @NumVoices,DataLength,DataLength);
End;

Function UltraSizeDram : LongInt;
Var ParmLength,DataLength : LongInt;
    DRAMSize              : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(DRAMSize);
  DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,UltraDevSizeDRAM,NIL,0,
              ParmLength,@DRAMSize,DataLength,DataLength);
  UltraSizeDram:=DRAMSize;
End;

Function UltraGetDriverVersion : Word;
var ParmLength,DataLength : LongInt;
    DrvVersion               : Word;
Begin
  ParmLength := 0;
  DataLength := SizeOf(DrvVersion);
  { High byte Contains major version,low byte minor version }
  DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,UltraDevGetDriverVersion,NIL,0,
              ParmLength,@DrvVersion,DataLength,DataLength);
  UltraGetDriverVersion:=DrvVersion;
End;

Function UltraGetDriverVersionStr : String;
{ þ This procedure was GetManleyVersion in the original VP/2 version þ }
Var DrvVersion    : Word;
    DrvVersionStr : String[5];
Begin
  DrvVersion:=UltraGetDriverVersion;
  Str(DrvVersion,DrvVersionStr);
  While Length(DrvVersionStr) < 4 Do
    DrvVersionStr:=' '+DrvVersionStr;
  UltraGetDriverVersionStr:=
    Copy(DrvVersionStr,1,2)+'.'+Copy(DrvVersionStr,3,2);
End;

Function UltraGetUltraType : LongInt;
Var ParmLength,DataLength : LongInt;
    GUSType               : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(GUSType);
  DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,UltraDevGetUltraType,NIL,0,
              ParmLength,@GUSType,DataLength,DataLength);
  UltraGetUltraType:=GUSType;
End;

Function UltraGetUltraTypeStr : String;
Const TypeStr : Array[1..11] Of String[10] = (
      ('PLAIN'),('ICS2101FLR'),('ICS2101'),('CS4231'),('ACE'),('CD3'),
      ('ICS2102'),('MAX23'),('CS4231_DB'),('?'),('PNP'));
Var GUSType : LongInt;
Begin
  GUSType:=UltraGetUltraType;
  If (GUSType >=1) And (GUSType <= 11) Then Begin
    UltraGetUltraTypeStr := TypeStr[GUSType]
   End Else Begin
    UltraGetUltraTypeStr := 'UNKNOWN';
   End;
End;


{ þ Memory handling þ }

Function UltraMemAlloc(Size : LongInt; Var Location : LongInt; Smp16Bit : Boolean) : LongInt;
Var ParmLength,DataLength : LongInt;
    AllocBuffer           : UltraAllocStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraAllocStruct);
  If (Size MOD 32) <> 0 Then
    Inc(Size,32 - (Size MOD 32)); { þ Bring Size up to a 32 byte boundary þ }
  With AllocBuffer Do Begin
    SmpSize     := Size;
    SmpLocation := Location;
    If Smp16Bit Then SmpType:=$04 Else SmpType:=$00;
   End;
  UltraMemAlloc:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                             UltraDevMemAlloc,
                             NIL,0,ParmLength,
                             @AllocBuffer,DataLength,DataLength);
  Location := AllocBuffer.SmpLocation; { þ location in GUS DRAM þ }
End;

Function UltraMemFree(Size,Location : LongInt) : LongInt;
Var ParmLength,DataLength : LongInt;
    FreeBuffer            : UltraFreeStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraFreeStruct);
  With FreeBuffer Do Begin
    SmpSize     := Size;
    SmpLocation := Location;
   End;
  DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
              UltraDevMemFree,
              NIL,0,ParmLength,
              @FreeBuffer,DataLength,DataLength);
  UltraMemFree := FreeBuffer.SmpSize;
End;

Function UltraDownload(DataPtr : Pointer; Control : Byte; DRAMLoc, Size : LongInt) : LongInt;
{ þ Requires Driver Version >= 1.10 þ }
Const Max = 64*1000;
Var ParmLength,DataLength : LongInt;
    RC                    : LongInt;
    Written               : LongInt;
    XferBuffer            : UltraXferStruct;
    Buffer64k             : Pointer;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraXferStruct);
  With XferBuffer Do Begin
    XferControl := Control;
    XferDRAMLoc := DRAMLoc;
   End;

  RC := 0;
  { þ NEED to allocate a buffer to transfer samples > 64 kb !!! þ }
  RC := DosAllocMem(Buffer64k,64*1024,PAG_COMMIT or PAG_WRITE);
  If RC = 0 Then Begin
    { þ 16 bit segments in a 32 bit world :( þ }
    { þ Another good example for the weakness of x86 architecture... þ }
    While (Size > Max) And (RC = 0) Do Begin
      Move(DataPtr^,Buffer64k^,Max);
      RC := DosDevIOCtl(ISS_GUS2Handle,GUS2_COMMANDS,
                        UltraDevPrepare4DMAXfer,
                        NIL,0,ParmLength,
                        @XferBuffer,DataLength,DataLength);
      If RC = 0 Then Begin
        RC := DosWrite(ISS_GUS2Handle,Buffer64k^,Max,Written);
        If RC = 0 Then Begin
          Inc(DRAMLoc,Max);
          With XferBuffer Do Begin
            XferDRAMLoc := DRAMLoc;
            XferControl := Control;
           End;
          Dec(Size,Max);
          Inc(DWord(DataPtr),Max);
         End;
       End;
     End;

    If (Size > 0) And (RC = 0) Then Begin
      Move(DataPtr^,Buffer64k^,Size);
      RC := DosDevIOCtl(ISS_GUS2Handle,GUS2_COMMANDS,
                        UltraDevPrepare4DMAXfer,
                        NIL,0,ParmLength,
                        @XferBuffer,DataLength,DataLength);
      If RC = 0 Then Begin { þ Last transfer þ }
        RC := DosWrite(ISS_GUS2Handle,Buffer64k^,Size,Written);
       End;
     End;

    DosFreeMem(Buffer64k);
   End;
  UltraDownLoad := RC;
End;


{ þ Timer handling þ }

Function UltraBlockTimerHandler1 : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  { þ block until GUS timer1 interrupt þ }
  UltraBlockTimerHandler1:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                       UltraDevBlockTimerHandler1,
                                       NIL,0,ParmLength,
                                       NIL,DataLength,DataLength);
End;

Function UltraBlockTimerHandler2 : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  { þ block until GUS timer2 interrupt þ }
  UltraBlockTimerHandler2:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                       UltraDevBlockTimerHandler2,
                                       NIL,0,ParmLength,
                                       NIL,DataLength,DataLength);
End;

Function UltraStartTimer(Timer,Time : LongInt) : LongInt;
Var ParmLength,DataLength : LongInt;
    TimerBuffer           : UltraTimerStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraTimerStruct);
  TimerBuffer.Timer:=Timer;
  TimerBuffer.Time :=Time;
  UltraStartTimer:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                               UltraDevStartTimer,
                               NIL,0,ParmLength,
                               @TimerBuffer,DataLength,DataLength);
End;

Function UltraStopTimer(Timer : LongInt) : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(Timer);
  UltraStopTimer:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                              UltraDevStopTimer,
                              NIL,0,ParmLength,
                              @Timer,DataLength,DataLength);
End;


{ þ Voice management þ }

Function UltraStopVoice(Voice : Integer) : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(Voice);
  UltraStopVoice:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                              UltraDevStopVoice,
                              NIL,0,ParmLength,
                              @Voice,DataLength,DataLength);
End;

Function UltraVoiceStopped(Voice : Integer) : Boolean;
{ þ Returns false while playing Voice þ }
Var ParmLength,DataLength : LongInt;
Begin
  ParmLength := 0;
  DataLength := SizeOf(Voice);
  DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
              UltraDevVoiceStopped,
              NIL,0,ParmLength,
              @Voice,DataLength,DataLength);
  UltraVoiceStopped := (Voice > 0);
End;

Function UltraVectorLinearVolume(Voice,End_Idx : Word; Rate,
                                 Mode : Byte): LongInt;
{ þ End_Idx;   Voice end level                     þ }
{ þ Rate;      0 to 63                             þ }
{ þ Mode;      mode to run the volume ramp in ...  þ }
Var ParmLength,DataLength : LongInt;
    VolBuffer             : UltraVolumeStruct;
Begin
  VolBuffer.Voice := Voice;
  VolBuffer.End_Idx := End_Idx;
  VolBuffer.Rate := Rate;
  VolBuffer.Mode := Mode;
  ParmLength := 0;
  DataLength := SizeOf(UltraVolumeStruct);
  UltraVectorLinearVolume := DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                         UltraDevVectorLinearVolume,
                                         NIL,0,ParmLength,
                                         @VolBuffer,DataLength,DataLength);
End;

Function UltraSetBalance(Voice : Integer; Data : Byte) : LongInt;
Var ParmLength,DataLength : LongInt;
    BalanceBuffer         : UltraBalanceStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraBalanceStruct);
  BalanceBuffer.Voice := Voice;
  BalanceBuffer.Data  := Data;
  UltraSetBalance:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                               UltraDevSetBalance,
                               NIL,0,ParmLength,
                               @BalanceBuffer,DataLength,DataLength);
End;

Function UltraSetFrequency(Voice : Integer; Freq : LongInt) : LongInt;
Var ParmLength,DataLength : LongInt;
    FreqBuffer            : UltraFreqStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraFreqStruct);
  FreqBuffer.Voice     := Voice;
  FreqBuffer.Speed_Khz := Freq;
  UltraSetFrequency:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                 UltraDevSetFrequency,
                                 NIL,0,ParmLength,
                                 @FreqBuffer,DataLength,DataLength);
End;

Function UltraStartVoice(GusVoice : Integer; begin_,start,end_ : LongInt;
                         Mode : Byte) : LongInt;
{ þ gusvoice  voice to start                    þ }
{ þ begin_    start location in ultra DRAM      þ }
{ þ start     start loop location in ultra DRAM þ }
{ þ end_      end location in ultra DRAM        þ }
{ þ mode      mode to run the voice (loop etc)  þ }
Var ParmLength,DataLength : LongInt;
    Voice                 : UltraVoiceStruct;
Begin
  ParmLength := 0;
  With Voice Do Begin
    VoiceNUm       := GusVoice;
    VoiceStart     := Begin_;  { start in DRAM  }
    VoiceLoopStart := Start;   { start loop }
    VoiceLoopEnd   := End_;
    VoiceMode      := Mode;
   End;
  DataLength   := SizeOf(UltraVoiceStruct);
  UltraStartVoice:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                               UltraDevStartVoice,
                               NIL,0,ParmLength,
                               @Voice,DataLength,DataLength);
End;


{ þ HW open/close þ }

Function OpenUltraSound : Boolean; { þ True if successful þ }
Var Status : LongInt;
    Action : LongInt;
Begin
  DOSCalls.DosError(fErr_DisableHardErr);
  Status := DosOpen('ULTRA1$',ISS_GUS2Handle,Action,0,FILE_NORMAL,
                     FILE_OPEN,OPEN_ACCESS_READWRITE or
                     OPEN_SHARE_DENYNONE or OPEN_FLAGS_WRITE_THROUGH,
                     NIL );
  If Status <> 0 Then Begin
    OpenUltraSound := FALSE
   End Else Begin
    If UltraGetAccess > 0 Then Begin
      DosClose(ISS_GUS2Handle);
      OpenUltraSound:=False;  { þ Not only owner þ }
     End Else Begin
      OpenUltraSound:=True;   { þ Ultrasound access ok þ }
     End;
   End;
  DOSCalls.DosError(fErr_EnableHardErr);
End;

Procedure CloseUltraSound;
Begin
  UltraDisableOutPut;
  UltraReleaseAccess;
  DosClose(ISS_GUS2Handle);
End;


{ þ >>> U L T R A S O U N D  U P D A T E  T H R E A D <<< þ }

Function ISS_GUS2HandlerThread(Param : Pointer) : LongInt; CDecl;
Var DGUSTimer : DWord;
Begin
 DGUSTimer:=12800000 Div (140*128*32);
 ISS_TimerDiff:=1193180 Div 140;
 UltraStartTimer(2,DGUSTimer);

 Repeat
  ISS_GUS2PeriodicCall;
  UltraBlockTimerHandler2;
 Until ISS_GUS2Thread_Exit;

 UltraStopTimer(2);
 ISS_GUS2HandlerThread:=0;
End;


{ þ >>> E X T E R N A L  D E V I C E - D R I V E R  F U N C T I O N S <<< þ }

Function ISS_GUS2Detect : Boolean;
Begin
 ISS_GUS2Detect:=ISS_GUS2Device.DevAvail;
End;

Function ISS_GUS2Init : Boolean;
Begin
 ISS_GUS2Init:=False;
 If Not ISS_GUS2Device.DevAvail Then Begin
   { þ ERROR CODE! þ }
   Exit;
  End;
 If Not OpenUltraSound Then Exit;
 ISS_GUS2DRAMOffset:=0;
 ISS_GUS2Init:=True;
End;

Function ISS_GUS2Done : Boolean;
Begin
 ISS_GUS2DRAMOffset:=0;
 CloseUltraSound;
 ISS_GUS2Done:=True;
End;

Function ISS_GUS2LoadSample(SStruc : ISS_PSample) : Boolean;
Type PInteger  = ^Integer;
     PShortInt = ^ShortInt;
Var SmpConvBuf  : Pointer;
    Counter     : DWord;
    RealSLength : DWord;
    SmpDRAMPos  : DWord;
    Status      : Boolean;
Begin
 ISS_GUS2LoadSample:=False;
 Status:=False;

 { þ Uploading sample þ }
 With SStruc^ Do Begin

   { þ If sample is 16bit, divide size with 2 þ }
   If (SType And ISS_Smp16BitData)>0 Then Begin
     RealSLength:=SLength Div 2;
    End Else Begin
     RealSLength:=SLength;
    End;

   { þ Is the sample fits into GUSRAM? þ }
   If RealSLength+ISS_GUS2DRAMOffset+1>ISS_GUS2Device.DevDRAMSize Then Begin
     { þ ERROR CODE! þ }
     Exit;
    End;

   GetMem(SmpConvBuf,RealSLength);

   { þ GUS has some limitations when using 16bit samples, noteably a 16bit þ }
   { þ sample can't cross a 256k boundary. To simplify the driver, and þ }
   { þ because the lack of time, i decided to convert the samples to 8bit þ }
   { þ before loading into the GUS RAM. Later, a GUS heap manager should þ }
   { þ be implemented to handle 16bit samples correctly, but the mixer þ }
   { þ and other tasks has higher priority now. þ }
   { þ UPDATE: instead of a heap handler, an intelligent sample manager þ }
   { þ         should be implemented, to allow playing XM's bigger than þ }
   { þ         the available wavetable memory on wavetable soundcards.  þ }
   If (SType And ISS_Smp16BitData)>0 Then Begin
     For Counter:=0 To RealSLength-1 Do Begin
       PShortInt(SmpConvBuf)[Counter]:=PInteger(SData)[Counter] Shr 8;
      End;
    End Else Begin
     Move(SData^,SmpConvBuf^,RealSLength);
    End;

   { þ Modifying sample beginning to avoid some clickings þ }
   PShortInt(SmpConvBuf)[0]:=0;

   If UltraMemAlloc(RealSLength,SmpDRAMPos,FALSE)=0 Then Begin
     Status:=(UltraDownLoad(SmpConvBuf,UltraDram16bit,SmpDRAMPos,RealSLength)=0);
    End;

   FreeMem(SmpConvBuf,RealSLength);

   SStruc^.SDRAMOffs:=SmpDRAMPos;
   If Status Then Inc(ISS_GUS2DRAMOffset,RealSLength);
  End;

 ISS_GUS2LoadSample:=Status;
End;


Function ISS_GUS2FreeSample(SStruc : ISS_PSample) : Boolean;
Begin
 ISS_GUS2FreeSample:=True;
End;

Function ISS_GUS2SetVolume(Volume : DWord) : Boolean;
Begin
 { þ Dummy. Old GUSes have no mixer. :( Later, the support for the þ }
 { þ newer-series GF1's ICS mixer should be added. I'm looking for þ }
 { þ some programing docs about the ICS mixer! If you have the docs, þ }
 { þ please contact me, so i can add support for it. þ }
 ISS_GUS2SetVolume:=True;
End;

Function ISS_GUS2StartOutput(PeriodicCall  : Pointer) : Boolean;
Var GUS2HandlerThread : TThreadEntry;
Begin
 ISS_GUS2StartOutput:=False;

 If ISS_ActiveSSChannels>32 Then ISS_GUS2ActiveChannels:=32
                            Else ISS_GUS2ActiveChannels:=ISS_ActiveSSChannels;
 If ISS_GUS2ActiveChannels<14 Then ISS_GUS2ActiveChannels:=14;

 { þ Boostin' up "The Queen Of The SoundCards" :) þ }
 UltraSetNumVoices(ISS_GUS2ActiveChannels);
 UltraEnableOutput;
 { þ Phew, this was easy, wasn't it? Compare this with a Sound Blaster þ }
 { þ initialization code... :) þ }

 { þ Now initializing the handler thread þ }
 ISS_GUS2Thread_Exit:=False;
 GUS2HandlerThread:=@ISS_GUS2HandlerThread;
 Pointer(ISS_GUS2PeriodicCall):=PeriodicCall;
 If DosCreateThread(ISS_GUS2UpdThreadID,GUS2HandlerThread,NIL,0,8192)=0 Then Begin
   DosSetPriority(dpThread,dpTimeCritical,31,ISS_GUS2UpdThreadID);
   ISS_GUS2StartOutput:=True;
   {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
     WriteLn('DEV_INIT: Starting ',ISS_GUS2Name,' output...');
   {$ENDIF}
  End Else Begin
   {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
     WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_GUS2Name,' output!');
   {$ENDIF}
  End;
End;

Function ISS_GUS2StopOutput(PeriodicCall : Pointer) : Boolean;
Begin
 UltraDisableOutput;

 { þ Terminating GUS update thread þ }
 ISS_GUS2Thread_Exit:=True;
 DosWaitThread(ISS_GUS2UpdThreadID,dtWait);

 ISS_GUS2StopOutput:=True{ISS_StopTimer(PeriodicCall)}; { þ Stops timer call þ }
 {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
   WriteLn('DEV_INIT: ',ISS_GUS2Name,' output stopped.');
 {$ENDIF}
End;

Procedure ISS_GUS2UpdateOutput; { þ Updates the sound output þ }
{ þ This is the main driver, which runs on the timer at 140hz frequency þ }
{ þ so it should be as fast as possible. Because it's a low-level driver, þ }
{ þ hardware and platform dependent, it's a good idea to implement this þ }
{ þ later again - in full-assembly. þ }
Var ChannelCounter : Word;
    SampleBegin    : DWord;
    SampleEnd      : DWord;
    SampleFreq     : DWord;
    LoopBegin      : DWord;
    LoopEnd        : DWord;
    RealLength     : DWord;
    RealLoopStart  : DWord;
    RealLoopEnd    : DWord;
Begin

 { þ Stop Voices If Needed þ }
 For ChannelCounter:=0 To ISS_GUS2ActiveChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin
     If ((VChControl And ISS_CCActive)>0) And
        ((VChControl And ISS_CCStop)>0) Then Begin
       Dec(VChControl,ISS_CCStop);
       UltraStopVoice(ChannelCounter);
      End;
    End;
  End;

 { þ Start Voices Update þ }
 For ChannelCounter:=0 To ISS_GUS2ActiveChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin

     { þ Anything to do on this channel? þ }
     If (VChControl>1) And ((VChControl And ISS_CCActive)>0) Then Begin

       { þ Start a Sample ? þ }
       If (VChControl And ISS_CCSample)>0 Then Begin
         Dec(VChControl,ISS_CCSample);

         With VChSmpAddr^ Do Begin

           { þ 16bit sample values conversion þ }
           If (SType And ISS_Smp16BitData)>0 Then Begin
             RealLength   :=SLength    Div 2;
             RealLoopStart:=SLoopStart Div 2;
             RealLoopEnd  :=SLoopEnd   Div 2;
            End Else Begin
             RealLength   :=SLength;
             RealLoopStart:=SLoopStart;
             RealLoopEnd  :=SLoopEnd;
            End;

           { þ Sample & Loop End Address Calc. þ }
           { þ -1 is needed, because SampleEnd value _must_ contain the þ }
           { þ _last_ sample position and not the last+1!!! þ }
           SampleEnd:=SDRAMOffs+RealLength-1;

           If (SType And ISS_SmpPingPongLoop)>0 Then Begin
             { þ Offset limit checking þ }
             If (VChSmpOffs>RealLength-1) Then VChSmpOffs:=RealLoopStart;
             { þ Sample end value checking þ }
             LoopEnd:=SDRAMOffs+RealLoopEnd-1; { þ Same here as above... þ }
             If LoopEnd>SampleEnd Then LoopEnd:=SampleEnd;
            End Else Begin
             LoopEnd:=SampleEnd;
            End;

           { þ Sample & Loop Start Address Calc. þ }
           SampleBegin:=SDRAMOffs+VChSmpOffs;
           LoopBegin  :=SDRAMOffs+RealLoopStart;

           { þ Now we're going to tell to the GUS these values þ }
           UltraStartVoice(ChannelCounter,SampleBegin,
                           LoopBegin,LoopEnd,SType And %00011000);
          End;
        End;

       { þ Change Channel Panning ? þ }
       If (VChControl And ISS_CCPanning)>0 Then Begin
         Dec(VChControl,ISS_CCPanning);
         UltraSetBalance(ChannelCounter,VChFinalPanning Shr 4);
        End;

       { þ Change Channel Volume ? þ }
       If (VChControl And ISS_CCVolume)>0 Then Begin
         Dec(VChControl,ISS_CCVolume);
         UltraVectorLinearVolume(ChannelCounter,VChFinalVolume Shl 2,$2F,0);
        End;

       { þ Change Channel Frequency ? þ }
       If (VChControl And ISS_CCPeriod)>0 Then Begin
         Dec(VChControl,ISS_CCPeriod);
         UltraSetFrequency(ChannelCounter,VChFreq);
        End;

       { þ Is Channel Still Active ? þ }
       If UltraVoiceStopped(ChannelCounter) Then Begin
         VChControl:=VChControl And Not ISS_CCActive;
        End;

      End;
    End;
  End;
End;


{ þ >>> P U B L I C  F U N C T I O N S <<< þ }

{ þ Inits the GUS2 driver structures þ }
Procedure ISS_GUS2DevInit;
Begin
 With ISS_GUS2Driver Do Begin
   Detect    :=@ISS_GUS2Detect;
   Init      :=@ISS_GUS2Init;
   Done      :=@ISS_GUS2Done;
   LoadSample:=@ISS_GUS2LoadSample;
   FreeSample:=@ISS_GUS2FreeSample;
   SetVolume :=@ISS_GUS2SetVolume;
   StartOut  :=@ISS_GUS2StartOutput;
   StopOut   :=@ISS_GUS2StopOutput;
   UpdateOut :=@ISS_GUS2UpdateOutput;
  End;
 ISS_GUS2Device.DevDriver:=@ISS_GUS2Driver;

 {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
  WriteLn('DEV_INIT: Device - ',ISS_GUS2LongDesc,' ',ISS_GUS2VersionStr);
 {$ENDIF}

 { þ Reading ULTRASND Environment Settings þ }
 If OpenUltraSound Then Begin
   { þ If ULTRASND found, assigning hardware parameters þ }
   With ISS_GUS2Device Do Begin
     DevName    :=ISS_GUS2Name; { þ Name of the device þ }
     DevType    :=ISS_Dev16Bit+ISS_DevStereo+ISS_DevWaveTable;{ þ Device Type þ }
     DevBaseport:=0;
     DevIRQ     :=0;
     DevDMA1    :=0;
     DevDMA2    :=0;
     DevFreq    :=44100;
     DevMaxChan :=32;
     DevDRAMSize:=UltraSizeDRAM*1024;
     If DevDRAMSize>0 Then DevAvail:=True;
     DevHWVer   :=UltraGetUltraTypeStr;
     DevSWVer   :=UltraGetDriverVersionStr;
    End;
   CloseUltraSound;
  End;

 If ISS_GUS2Device.DevAvail Then Begin
   With ISS_GUS2Device Do Begin
     {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
      WriteLn('DEV_INIT:        - Hardware Revision: ',DevHWVer,' - DRAM Size: ',DevDRAMSize Div 1024,'KB');
      WriteLn('DEV_INIT:        - Using Manley Driver Version: ',DevSWVer);
     {$ENDIF}
    End;
  End Else Begin
   {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
    WriteLn('DEV_INIT:        - GF1/INTERWAVE DETECTION FAILED!');
   {$ENDIF}
  End;
End;

Begin
End.
{ þ ISS_GUS22.PAS - (C) 2001 Charlie/Inquisition þ }
