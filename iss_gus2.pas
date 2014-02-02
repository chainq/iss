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

{ * ISS_GUS2.PAS - Device Driver for GF1/Interwave based cards under OS/2 }
{                  via the Manley Drivers native API.                     }
{             OS - OS/2 only.                                             }

{ * This unit is heavily based on the work of Sander van Leeuwen author of }
{   the latest OS/2 GUS2 drivers and Timo Maier, who ported the native API }
{   C include file to Virtual Pascal/2. Thank you.                         }

{$INCLUDE ISS_SET.INC}
{$ASMMODE INTEL}
{$MODE FPC}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }

{$IFDEF _ISS_GUSNATIVE_OLDVOLUMETABLE_}
 {$INFO GUS2 driver will use the old volume table!}
{$ENDIF}

Unit ISS_GUS2;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     OS2Def,  { * Uses OS/2 system interface * }
     DOSCalls;{ * Uses OS/2 DOSCALLS.DLL * }

Const ISS_GUS2VersionStr = '0.1.0';
      ISS_GUS2Name     = 'OS/2 GF1/Interwave Driver';
      ISS_GUS2LongDesc = 'OS/2 GF1/Interwave Based Device Driver';

Var ISS_GUS2Device : ISS_TSoundDevice; { * GUS2 Device Structure * }
    ISS_GUS2Driver : ISS_TSoundDriver; { * GUS2 Device Driver * }

Procedure ISS_GUS2DevInit; { * Inits the GUS2 driver structures * }

Implementation

Type UltraAllocStruct = Packed Record
       SmpSize     : LongInt; { * Memory size to allocate * }
       SmpLocation : LongInt; { * Location (return value) * }
       SmpType     : Byte;    { * 0 = 8bit sample, 4 = 16bit sample * }
      End;
     UltraFreeStruct  = Packed Record
       SmpSize     : LongInt; { * Memory size to free * }
       SmpLocation : LongInt; { * Location to free * }
      End;
     UltraXferStruct = Packed Record
       XferControl : Byte;
       XferDRAMLoc : LongInt;
      End;
     UltraTimerStruct = Packed Record
       Timer : Integer; { * GUS timer interrupt number * }
       Time  : Byte;    { * Time between interrupts * }
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

      { * Here are the dma to/from DRAM control bit definitions: * }
      UltraDramRead  = $02;
      UltraDram16Bit = $40;
      UltraDram8Bit  = $80;

Var ISS_GUS2DRAMOffset : DWord;

    ISS_GUS2ActiveChannels : Word;  { * Number of active hardware channels * }

    ISS_GUS2Handle         : DWord;
    ISS_GUS2UpdThreadID    : LongInt;
    ISS_GUS2Thread_Exit    : Boolean;
    ISS_GUS2PeriodicCall   : Procedure;

{ * >>> O S / 2  S Y S T E M  C O N S T A N T S <<< * }
{ * The constants here should be added later to the FPC OS/2 RTL (?) * }

Const { * DosOpen/DosQFHandState/DosQueryFileInfo et al file attributes; also * }
      { * known as Dos File Mode bits... * }
      FILE_NORMAL    = $0000;
      FILE_READONLY  = $0001;
      FILE_HIDDEN    = $0002;
      FILE_SYSTEM    = $0004;
      FILE_DIRECTORY = $0010;
      FILE_ARCHIVED  = $0020;

      { * DosOpen() actions * }
      FILE_EXISTED   = $0001;
      FILE_CREATED   = $0002;
      FILE_TRUNCATED = $0003;

      { * DosOpen() open flags * }
      FILE_OPEN     = $0001;
      FILE_TRUNCATE = $0002;
      FILE_CREATE   = $0010;

      { * DosOpen/DosSetFHandState flags * }
      OPEN_ACCESS_READONLY        = $0000; { * ---- ---- ---- -000 * }
      OPEN_ACCESS_WRITEONLY       = $0001; { * ---- ---- ---- -001 * }
      OPEN_ACCESS_READWRITE       = $0002; { * ---- ---- ---- -010 * }
      OPEN_SHARE_DENYREADWRITE    = $0010; { * ---- ---- -001 ---- * }
      OPEN_SHARE_DENYWRITE        = $0020; { * ---- ---- -010 ---- * }
      OPEN_SHARE_DENYREAD         = $0030; { * ---- ---- -011 ---- * }
      OPEN_SHARE_DENYNONE         = $0040; { * ---- ---- -100 ---- * }
      OPEN_FLAGS_NOINHERIT        = $0080; { * ---- ---- 1--- ---- * }
      OPEN_FLAGS_NO_LOCALITY      = $0000; { * ---- -000 ---- ---- * }
      OPEN_FLAGS_SEQUENTIAL       = $0100; { * ---- -001 ---- ---- * }
      OPEN_FLAGS_RANDOM           = $0200; { * ---- -010 ---- ---- * }
      OPEN_FLAGS_RANDOMSEQUENTIAL = $0300; { * ---- -011 ---- ---- * }
      OPEN_FLAGS_NO_CACHE         = $1000; { * ---1 ---- ---- ---- * }
      OPEN_FLAGS_FAIL_ON_ERROR    = $2000; { * --1- ---- ---- ---- * }
      OPEN_FLAGS_WRITE_THROUGH    = $4000; { * -1-- ---- ---- ---- * }
      OPEN_FLAGS_DASD             = $8000; { * 1--- ---- ---- ---- * }
      OPEN_FLAGS_NONSPOOLED       = $00040000;
      OPEN_FLAGS_PROTECTED_HANDLE = $40000000;

      { * definitions for DosError - combine with Or * }
      FERR_DISABLEHARDERR   = $00000000; { * disable hard error popups * }
      FERR_ENABLEHARDERR    = $00000001; { * enable hard error popups  * }
      FERR_ENABLEEXCEPTION  = $00000000; { * enable exception popups   * }
      FERR_DISABLEEXCEPTION = $00000002; { * disable exception popups  * }

{ * Workaround for DosDevIOCtl definition in DOSCALLS.DLL using pointers. * }
Function DosDevIOCtl(Handle, Category, Func : LongInt;
                     Params: Pointer; ParamLen: LongInt; Var ParamSize: LongInt;
                     Data: Pointer; DataLen: LongInt; Var DataSize: LongInt): LongInt; CDecl;
External 'DOSCALLS' Index 284;


{ * >>> D E B U G  F U N C T I O N S <<< * }

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

{ * >>> I N T E R N A L  F U N C T I O N S <<< * }


{ * HW initializating and basic settings * }

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
  { * Make sure, voices is in the 14-32 range. * }
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
{ * This procedure was GetManleyVersion in the original VP/2 version * }
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


{ * Memory handling * }

Function UltraMemAlloc(Size : LongInt; Var Location : LongInt; Smp16Bit : Boolean) : LongInt;
Var ParmLength,DataLength : LongInt;
    AllocBuffer           : UltraAllocStruct;
Begin
  ParmLength := 0;
  DataLength := SizeOf(UltraAllocStruct);
  If (Size MOD 32) <> 0 Then
    Inc(Size,32 - (Size MOD 32)); { * Bring Size up to a 32 byte boundary * }
  With AllocBuffer Do Begin
    SmpSize     := Size;
    SmpLocation := Location;
    If Smp16Bit Then SmpType:=$04 Else SmpType:=$00;
   End;
  UltraMemAlloc:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                             UltraDevMemAlloc,
                             NIL,0,ParmLength,
                             @AllocBuffer,DataLength,DataLength);
  Location := AllocBuffer.SmpLocation; { * location in GUS DRAM * }
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
{ * Requires Driver Version >= 1.10 * }
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
  { * NEED to allocate a buffer to transfer samples > 64 kb !!! * }
  RC := DosAllocMem(Buffer64k,64*1024,PAG_COMMIT or PAG_WRITE);
  If RC = 0 Then Begin
    { * 16 bit segments in a 32 bit world :( * }
    { * Another good example for the weakness of x86 architecture... * }
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
      If RC = 0 Then Begin { * Last transfer * }
        RC := DosWrite(ISS_GUS2Handle,Buffer64k^,Size,Written);
       End;
     End;

    DosFreeMem(Buffer64k);
   End;
  UltraDownLoad := RC;
End;


{ * Timer handling * }

Function UltraBlockTimerHandler1 : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  { * block until GUS timer1 interrupt * }
  UltraBlockTimerHandler1:=DosDevIOCtl(ISS_GUS2Handle,GUS2_Commands,
                                       UltraDevBlockTimerHandler1,
                                       NIL,0,ParmLength,
                                       NIL,DataLength,DataLength);
End;

Function UltraBlockTimerHandler2 : LongInt;
Var ParmLength,DataLength : LongInt;
Begin
  DataLength := 0; ParmLength := 0;
  { * block until GUS timer2 interrupt * }
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


{ * Voice management * }

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
{ * Returns false while playing Voice * }
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
{ * End_Idx;   Voice end level                     * }
{ * Rate;      0 to 63                             * }
{ * Mode;      mode to run the volume ramp in ...  * }
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
{ * gusvoice  voice to start                    * }
{ * begin_    start location in ultra DRAM      * }
{ * start     start loop location in ultra DRAM * }
{ * end_      end location in ultra DRAM        * }
{ * mode      mode to run the voice (loop etc)  * }
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


{ * HW open/close * }

Function OpenUltraSound : Boolean; { * True if successful * }
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
      OpenUltraSound:=False;  { * Not only owner * }
     End Else Begin
      OpenUltraSound:=True;   { * Ultrasound access ok * }
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


{ * >>> U L T R A S O U N D  U P D A T E  T H R E A D <<< * }

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


{ * >>> E X T E R N A L  D E V I C E - D R I V E R  F U N C T I O N S <<< * }

Function ISS_GUS2Detect : Boolean;
Begin
 ISS_GUS2Detect:=ISS_GUS2Device.DevAvail;
End;

Function ISS_GUS2Init : Boolean;
Begin
 ISS_GUS2Init:=False;
 If Not ISS_GUS2Device.DevAvail Then Begin
   { * ERROR CODE! * }
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

 { * Uploading sample * }
 With SStruc^ Do Begin

   { * If sample is 16bit, divide size with 2 * }
   If (SType And ISS_Smp16BitData)>0 Then Begin
     RealSLength:=SLength Div 2;
    End Else Begin
     RealSLength:=SLength;
    End;

   { * Is the sample fits into GUSRAM? * }
   If RealSLength+ISS_GUS2DRAMOffset+1>ISS_GUS2Device.DevDRAMSize Then Begin
     { * ERROR CODE! * }
     Exit;
    End;

   GetMem(SmpConvBuf,RealSLength);

   { * GUS has some limitations when using 16bit samples, noteably a 16bit * }
   { * sample can't cross a 256k boundary. To simplify the driver, and * }
   { * because the lack of time, i decided to convert the samples to 8bit * }
   { * before loading into the GUS RAM. Later, a GUS heap manager should * }
   { * be implemented to handle 16bit samples correctly, but the mixer * }
   { * and other tasks has higher priority now. * }
   { * UPDATE: instead of a heap handler, an intelligent sample manager * }
   { *         should be implemented, to allow playing XM's bigger than * }
   { *         the available wavetable memory on wavetable soundcards.  * }
   If (SType And ISS_Smp16BitData)>0 Then Begin
     For Counter:=0 To RealSLength-1 Do Begin
       PShortInt(SmpConvBuf)[Counter]:=PInteger(SData)[Counter] Shr 8;
      End;
    End Else Begin
     Move(SData^,SmpConvBuf^,RealSLength);
    End;

   { * Modifying sample beginning to avoid some clickings * }
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
 { * Dummy. Old GUSes have no mixer. :( Later, the support for the * }
 { * newer-series GF1's ICS mixer should be added. I'm looking for * }
 { * some programing docs about the ICS mixer! If you have the docs, * }
 { * please contact me, so i can add support for it. * }
 ISS_GUS2SetVolume:=True;
End;

Function ISS_GUS2StartOutput(PeriodicCall  : Pointer) : Boolean;
Var GUS2HandlerThread : TThreadEntry;
Begin
 ISS_GUS2StartOutput:=False;

 If ISS_ActiveSSChannels>32 Then ISS_GUS2ActiveChannels:=32
                            Else ISS_GUS2ActiveChannels:=ISS_ActiveSSChannels;
 If ISS_GUS2ActiveChannels<14 Then ISS_GUS2ActiveChannels:=14;

 { * Boostin' up "The Queen Of The SoundCards" :) * }
 UltraSetNumVoices(ISS_GUS2ActiveChannels);
 UltraEnableOutput;
 { * Phew, this was easy, wasn't it? Compare this with a Sound Blaster * }
 { * initialization code... :) * }

 { * Now initializing the handler thread * }
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

 { * Terminating GUS update thread * }
 ISS_GUS2Thread_Exit:=True;
 DosWaitThread(ISS_GUS2UpdThreadID,dtWait);

 ISS_GUS2StopOutput:=True{ISS_StopTimer(PeriodicCall)}; { * Stops timer call * }
 {$IFDEF _ISS_GUSNATIVE_DEBUGMODE_}
   WriteLn('DEV_INIT: ',ISS_GUS2Name,' output stopped.');
 {$ENDIF}
End;

Procedure ISS_GUS2UpdateOutput; { * Updates the sound output * }
{ * This is the main driver, which runs on the timer at 140hz frequency * }
{ * so it should be as fast as possible. Because it's a low-level driver, * }
{ * hardware and platform dependent, it's a good idea to implement this * }
{ * later again - in full-assembly. * }
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

 { * Stop Voices If Needed * }
 For ChannelCounter:=0 To ISS_GUS2ActiveChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin
     If ((VChControl And ISS_CCActive)>0) And
        ((VChControl And ISS_CCStop)>0) Then Begin
       Dec(VChControl,ISS_CCStop);
       UltraStopVoice(ChannelCounter);
      End;
    End;
  End;

 { * Start Voices Update * }
 For ChannelCounter:=0 To ISS_GUS2ActiveChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin

     { * Anything to do on this channel? * }
     If (VChControl>1) And ((VChControl And ISS_CCActive)>0) Then Begin

       { * Start a Sample ? * }
       If (VChControl And ISS_CCSample)>0 Then Begin
         Dec(VChControl,ISS_CCSample);

         With VChSmpAddr^ Do Begin

           { * 16bit sample values conversion * }
           If (SType And ISS_Smp16BitData)>0 Then Begin
             RealLength   :=SLength    Div 2;
             RealLoopStart:=SLoopStart Div 2;
             RealLoopEnd  :=SLoopEnd   Div 2;
            End Else Begin
             RealLength   :=SLength;
             RealLoopStart:=SLoopStart;
             RealLoopEnd  :=SLoopEnd;
            End;

           { * Sample & Loop End Address Calc. * }
           { * -1 is needed, because SampleEnd value _must_ contain the * }
           { * _last_ sample position and not the last+1!!! * }
           SampleEnd:=SDRAMOffs+RealLength-1;

           If (SType And ISS_SmpPingPongLoop)>0 Then Begin
             { * Offset limit checking * }
             If (VChSmpOffs>RealLength-1) Then VChSmpOffs:=RealLoopStart;
             { * Sample end value checking * }
             LoopEnd:=SDRAMOffs+RealLoopEnd-1; { * Same here as above... * }
             If LoopEnd>SampleEnd Then LoopEnd:=SampleEnd;
            End Else Begin
             LoopEnd:=SampleEnd;
            End;

           { * Sample & Loop Start Address Calc. * }
           SampleBegin:=SDRAMOffs+VChSmpOffs;
           LoopBegin  :=SDRAMOffs+RealLoopStart;

           { * Now we're going to tell to the GUS these values * }
           UltraStartVoice(ChannelCounter,SampleBegin,
                           LoopBegin,LoopEnd,SType And %00011000);
          End;
        End;

       { * Change Channel Panning ? * }
       If (VChControl And ISS_CCPanning)>0 Then Begin
         Dec(VChControl,ISS_CCPanning);
         UltraSetBalance(ChannelCounter,VChFinalPanning Shr 4);
        End;

       { * Change Channel Volume ? * }
       If (VChControl And ISS_CCVolume)>0 Then Begin
         Dec(VChControl,ISS_CCVolume);
         UltraVectorLinearVolume(ChannelCounter,VChFinalVolume Shl 2,$2F,0);
        End;

       { * Change Channel Frequency ? * }
       If (VChControl And ISS_CCPeriod)>0 Then Begin
         Dec(VChControl,ISS_CCPeriod);
         UltraSetFrequency(ChannelCounter,VChFreq);
        End;

       { * Is Channel Still Active ? * }
       If UltraVoiceStopped(ChannelCounter) Then Begin
         VChControl:=VChControl And Not ISS_CCActive;
        End;

      End;
    End;
  End;
End;


{ * >>> P U B L I C  F U N C T I O N S <<< * }

{ * Inits the GUS2 driver structures * }
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

 { * Reading ULTRASND Environment Settings * }
 If OpenUltraSound Then Begin
   { * If ULTRASND found, assigning hardware parameters * }
   With ISS_GUS2Device Do Begin
     DevName    :=ISS_GUS2Name; { * Name of the device * }
     DevType    :=ISS_Dev16Bit+ISS_DevStereo+ISS_DevWaveTable;{ * Device Type * }
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
