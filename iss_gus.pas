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

{ * ISS_GUS .PAS - Device Driver for GF1 based Gravis Ultrasound cards    }
{             OS - GO32V2 only.                                           }

{$INCLUDE ISS_SET.INC}
{$ASMMODE INTEL}
{$MODE FPC}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }

Unit ISS_GUS;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     ISS_Tim, { * Uses the timer services * }
     GO32,    { * Uses GO32 unit, because DOS-only driver * }
     DOS;     { * Uses DOS unit, for environment variable access * }

Const ISS_GUSVersionStr = '0.6.6';
      ISS_GUSName     = 'Gravis Ultrasound';
      ISS_GUSLongDesc = 'Gravis Ultrasound GF1 Based Wavetable Device Driver';

Var ISS_GUSDevice : ISS_TSoundDevice; { * GUS Device Structure * }
    ISS_GUSDriver : ISS_TSoundDriver; { * GUS Device Driver * }

Procedure ISS_GUSDevInit; { * Inits the GUS driver structures * }

Implementation

{ * UltraSound Ports * }
Const StatusPort      = $6; { * $2x6 * }
      TimerCtrlPort   = $8; { * $2x8 * }
      TimerDataPort   = $9; { * $2x9 * }

      MidiCtrlPort    = $100;
      MidiDataPort    = $101;

      ActiveVoicePort = $102;
      CommandPort     = $103; { * $3x3 (Reg Select) * }
      DataLowPort     = $104; { * $3x4 * }
      DataHighPort    = $105; { * $3x5 * }
      DRAMIOPort      = $107; { * $3x7 * }

      JoystickTimer   = $201;

      BoardVersion    = $506; { * Read * }
      MixerControl    = $506; { * Write (ICS Mixer Control) * }

      { * UltraSound Commands ( or/and register ) * }

      WriteVoiceMode  = $00;
      SetVoiceFreq    = $01; { * Value=Freq/Divisor * }
      LoopStartLo     = $02;
      LoopStartHi     = $03;
      SampleEndLo     = $04;
      SampleEndHi     = $05;
      VolRampRate     = $06;
      VolRampStart    = $07;
      VolRampEnd      = $08;
      SetVolume       = $09;
      SampleStartLo   = $0A;
      SampleStartHi   = $0B;
      VoiceBalance    = $0C;
      VolumeCtrl      = $0D;
      VoicesActive    = $0E;
      DMACtrl         = $41;
      DMAStartAddress = $42;
      DRAMAddrLo      = $43;
      DRAMAddrHi      = $44;
      TimerCtrl       = $45;
      TimerCount1     = $46;
      TimerCount2     = $47;
      SampleFreq      = $48;
      SampleCtrl      = $49;
      Initialize      = $4C;
      Read            = $80;
      ReadVolume      = Read+SetVolume;     { * $89 * }
      VoicePosLo      = Read+SampleStartLo; { * $8A * }
      VoicePosHi      = Read+SampleStartHi; { * $8B * }
      ReadVolCtrl     = Read+VolumeCtrl;    { * $8D * }
      IRQStatus       = $8F;

      GUSFreq : Array[0..31] Of Word =( { * Ultrasound Frequency Table * }
                      44100,44100,44100,44100,44100,44100,44100,
                      44100,44100,44100,44100,44100,44100,44100,
                      41160,38587,36317,34300,32494,30870,29400,
                      28063,26843,25725,24696,23746,22866,22050,
                      21289,20580,19916,19293);

      GUS_StopLineIn = 0; { * 0 = Line In Enabled, 1 = Line In Disabled * }

Type ISS_TGUSHWSetup = Record { * Used for GUS detection * }
       Base_Port  : Word;
       DRAM_DMA   : Word;
       ADC_DMA    : Word;
       GF1_IRQ    : Word;
       MIDI_IRQ   : Word;
       DRAM_Size  : Word;
       GUS_Ver    : Byte;
      End;

Var ISS_GUSHWSetup    : ISS_TGUSHWSetup; { * GUS Hardware Parameters * }
    ISS_GUSDRAMOffset : DWord;

    ISS_GUSActiveChannels : Word;  { * Number of active hardware channels * }
    ISS_GUSDivisor        : DWord; { * Frequency divisor (See GUS_Freq) * }

    ISS_GUSVol : Array[0..512] Of Word; { * GUS Volume table * }

{ * >>> D E B U G  F U N C T I O N S <<< * }

{$IFDEF _ISS_GUS_DEBUGMODE_}
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

{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }
Function ParseToHex(Var FromStr : String; Var ToWord : Word) : Boolean;
{ * Take the first number found.  Disregard ',' and ' ' chars * }
Begin
 ParseToHex := False;
 While ((FromStr[1]=' ') Or (FromStr[1]=',') Or
        (FromStr[1]='$')) And (Length(FromStr)<>0) Do Delete(FromStr,1,1);
 If (Ord(FromStr[1])>47) And (Ord(FromStr[1])<58) And
    (FromStr <> '') Then Begin
   ParseToHex:=True;  { * a number was found * }
   ToWord:=0;
   While (Ord(FromStr[1])>47) And (Ord(FromStr[1])<58) And
         (FromStr <> '') Do Begin
     ToWord:=(ToWord*$10)+(Ord(FromStr[1])-Ord('0'));
     Delete(FromStr,1,1);
    End;
  End;
End;
{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }

{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }
Function ParseToNum(Var FromStr : String; Var ToWord : Word) : Boolean;
{ * Take the first number found.  Disregard ',' And ' ' chars * }
Var StripStr : String[10];
    Code     : Integer;
Begin
 ParseToNum := False;
 While ((FromStr[1]=' ') Or (FromStr[1]=',')) And
        (Length(FromStr)<>0) Do Delete(FromStr,1,1);
 If (Ord(FromStr[1])>47) And (Ord(FromStr[1])<58) And (FromStr<>'') Then Begin
   ParseToNum:=True;  { * a number was found * }
   StripStr:='';
   While (Ord(FromStr[1])>47) And (Ord(FromStr[1])<58) And
         (FromStr<>'') Do Begin
     StripStr:=StripStr+FromStr[1];
     Delete(FromStr,1,1);
    End;
   Val(StripStr,ToWord,Code);
  End;
End;
{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }

{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }
Function UltraGetConfig(Var Config : ISS_TGUSHWSetup) : Boolean;
Var EnvStr : String;
Begin
 UltraGetConfig:=False;
 EnvStr:=GetEnv('ULTRASND');
 If EnvStr<>'' Then Begin
   With Config Do Begin
     If ParseToHex(EnvStr,Base_Port) And ParseToNum(EnvStr,DRAM_DMA) And
        ParseToNum(EnvStr,ADC_DMA) And ParseToNum(EnvStr,GF1_IRQ) And
        ParseToNum(EnvStr,MIDI_IRQ) Then Begin
       UltraGetConfig:=True;
      End;
    End;
  End;
End;
{ *  !UGLY  SHIT  FROM  GUS  SDK!  * }

Procedure GUSGenerateVolTable;
Var Counter  : DWord;
    Counter2 : DWord;
    TempVal  : DWord;
Begin
 ISS_GUSVol[0]:=0;
 ISS_GUSVol[512]:=$0FFF;
 For Counter:=1 To 511 Do Begin
   Counter2:=$0600;
   TempVal :=Counter;
   While TempVal>0 Do Begin
     Inc(Counter2,$0100);
     TempVal:=TempVal Shr 1;
    End;
   ISS_GUSVol[Counter]:=Counter2 Or ((Counter Shl (8-((Counter2-$0700) Shr 8))) And $0FF);
  End;
End;

Procedure GUSDelay; Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 IN  AL,DX
 IN  AL,DX
 IN  AL,DX
 IN  AL,DX
 IN  AL,DX
 IN  AL,DX
 IN  AL,DX
End;

Procedure GUSWrite(Register, Value : Byte); Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,Register
 OUT DX,AL { * Selects the register to write * }
 ADD DX,2  { * Data High port * }
 MOV AL,Value
 OUT DX,AL { * Writes value out to the GUS * }
End;

Procedure GUSWriteDelay(Register, Value : Byte); Assembler;
Asm
 MOV  DX,CommandPort
 ADD  DX,ISS_GUSDevice.DevBaseport
 MOV  AL,Register
 OUT  DX,AL { * Selects the register to write * }
 ADD  DX,2  { * Data High port * }
 MOV  AL,Value
 OUT  DX,AL { * Writes value out to the GUS * }
 CALL GUSDelay
 OUT  DX,AL { * Writes value out to the GUS * }
End;

Procedure GUSWriteW(Register : Byte; Value : Word); Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,Register
 OUT DX,AL { * Selects the register to write * }
 INC DX    { * Data Low port * }
 MOV AX,Value
 OUT DX,AX { * Writes value out to the GUS * }
End;

Function GUSRead(Register : Byte) : Byte; Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,Register
 OUT DX,AL { * Selects the register to read * }
 ADD DX,2  { * Data High Port * }
 IN  AL,DX { * Reads value out from the GUS * }
End;

Function GUSReadW(Register : Byte) : Word; Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,Register
 OUT DX,AL { * Selects the register to read * }
 INC DX    { * Data Low Port * }
 IN  AX,DX { * Reads value out from the GUS * }
End;

{ * Set the current active voice * }
Procedure GUSSetVoice(Voice : Byte); Assembler;
Asm
 MOV AL,Voice
 MOV DX,ActiveVoicePort
 ADD DX,ISS_GUSDevice.DevBaseport
 OUT DX,AL
End;

{ * Reads a byte from GUS DRAM * }
Function GUSPeek(Address : DWord) : Byte; Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,DRAMAddrLo
 OUT DX,AL { * Select Register (3x3) * }
 INC DX    { * DataLowPort     (3x4) * }
 MOV EAX,Address
 OUT DX,AX
 DEC DX    { * CommandPort     (3x3) * }
 MOV AL,DRAMAddrHi
 OUT DX,AL
 ADD DX,2  { * DataHighPort    (3x5) * }
 SHR EAX,16
 OUT DX,AL
 ADD DX,2  { * DRAMIOPort      (3x7) * }
 IN  AL,DX
End;

{ * Writes a byte to GUS DRAM * }
Procedure GUSPoke(Address : DWord; Value : Byte); Assembler;
Asm
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,DRAMAddrLo
 OUT DX,AL { * Select Register (3x3) * }
 INC DX    { * DataLowPort     (3x4) * }
 MOV EAX,Address
 OUT DX,AX
 DEC DX    { * CommandPort     (3x3) * }
 MOV AL,DRAMAddrHi
 OUT DX,AL
 ADD DX,2  { * DataHighPort    (3x5) * }
 SHR EAX,16
 OUT DX,AL
 ADD DX,2  { * DRAMIOPort      (3x7) * }
 MOV AL,Value
 OUT DX,AL
End;

Function GUSHardDetect : Boolean;
Begin
 GUSHardDetect:=False;
 With ISS_GUSDevice Do Begin
   If DevBaseport=0 Then Exit;

   Asm
    MOV DX,CommandPort
    ADD DX,ISS_GUSDevice.DevBaseport
    MOV AL,Initialize { * Take the GUS out of a reset state * }
    OUT DX,AL
    ADD DX,2          { * DataHighPort * }
    MOV AL,00000111B
    OUT DX,AL
   End;

   { * Detecting amount of GUS memory * }
   DevDRAMSize:=0;
   GUSPoke(DevDRAMSize,$AA);
   While (GUSPeek(DevDRAMSize)=$AA) And (DevDRAMSize<1024*1024) Do Begin
     Inc(DevDRAMSize,256*1024);
     GUSPoke(DevDRAMSize,$AA);
    End;

   { * If no RAM available then GUS cannot be used, so exit. * }
   If DevDRAMSize=0 Then Exit;

   { * Querying hardware revision ID. * }
   Asm
    MOV DX,BoardVersion
    ADD DX,ISS_GUSDevice.DevBaseport
    IN  AL,DX
    MOV ISS_GUSHWSetup.GUS_Ver,AL
   End;

  End;
 GUSHardDetect:=True;
End;

Procedure GUS_OutputInit;
Var Counter : Byte;
Begin
 Asm
  CLI
 End;
 GUSWrite(Initialize,0); { * GF1 Master Reset * }
 GUSDelay;
 GUSDelay;
 GUSWrite(Initialize,1); { * GF1 Enable * }
 GUSDelay;
 GUSDelay;
 GUSWrite(VoicesActive,31 Or $0C0);
 For Counter:=0 To 31 Do Begin
   Asm
    MOV DX,ActiveVoicePort
    ADD DX,ISS_GUSDevice.DevBaseport
    MOV AL,Counter
    OUT DX,AL { * Now we select channel * }
   End;
   GUSWriteDelay(WriteVoiceMode,3); { * Voice Off * }
   GUSWriteDelay(VolumeCtrl,3);     { * Volume Ramp Off * }
   GUSWrite(VolRampRate,%00111111); { * Max RampRate * }
   GUSWriteW(SetVolume,$5000); { * Volume <- 0 * }
   GUSDelay;
   GUSDelay;
  End;
 GUSWrite(Initialize,3);
 GUSDelay;
 Asm
  MOV DX,ISS_GUSDevice.DevBaseport
  MOV AL,8+GUS_StopLineIn;
  OUT DX,AL
  STI
 End;
End;

Procedure GUS_RAM2DRAM(SData : Pointer; GUSOffset : DWord; Count : DWord); Assembler;
Asm
 CLI
 MOV EBX,GUSOffset
 MOV EDI,EBX
 SHR EBX,16
 MOV DX,CommandPort
 ADD DX,ISS_GUSDevice.DevBaseport
 MOV AL,DRAMAddrHi
 OUT DX,AL { * 3x3 Select Register * }
 ADD DX,2
 MOV AX,BX
 OUT DX,AL { * 3x5 Data High * }
 SUB DX,2
 MOV AL,DRAMAddrLo
 OUT DX,AL { * 3x3 Select Register * }
 INC DX
 MOV ECX,Count
 MOV ESI,SData { * Data Source * }
 @CycleHead:
  MOV AX,DI
  OUT DX,AX { * 3x4 Data Low * }
  ADD DX,3
  MOV AL,[ESI]
  INC ESI
  OUT DX,AL { * 3x7 DRAM I/O * }
  SUB DX,3
  ADD DI,1
  JNC @DoLoop
   DEC DX
   INC BX
   MOV AL,DRAMAddrHi
   OUT DX,AL { * 3x3 Select Register * }
   ADD DX,2
   MOV AX,BX
   OUT DX,AL { * 3x5 Data High * }
   SUB DX,2
   MOV AL,DRAMAddrLo
   OUT DX,AL { * 3x3 Select Register * }
   INC DX
  @DoLoop:
  DEC ECX
 JNZ @CycleHead
 STI
End;

{ * Start a volume ramp on the current channel from the actual volume to * }
{ * the specified volume * }
Procedure GUS_FadeVol(Vol2 : Word);
Var Vol1    : Word;
    Buffer  : Word;
    Swapped : Byte;
Begin

 { * Read current volume from the GUS * }
 Vol1:=GUSReadW(ReadVolume) Shr 4;

 { * Setup ramp direction * }
 If Vol1>Vol2 Then Begin
   Buffer:=Vol1; Vol1:=Vol2; Vol2:=Buffer; Swapped:=$40;
  End Else Begin
   Swapped:=$0;
  End;

 Dec(Vol2,Vol1); If Vol2=0 Then Exit;

 { * Just simply set the volume if ramp would be too small * }
 If Vol2<64 Then Begin
   If Swapped=0 Then Inc(Vol1,Vol2);
   GUSWriteW(SetVolume,Vol1 Shl 4);
   Exit;
  End;

 { * Limit checks * }
 Inc(Vol2,Vol1);

 if (Vol1<64)   then Vol1:=64;
 if (Vol2>4032) then Vol2:=4032;

 { * Setup new ramp * }
 GUSWrite(VolRampStart,Vol1 Shr 4);
 GUSWrite(VolRampEnd,Vol2 Shr 4);
 { * Start new ramp * }
 GUSWriteDelay(VolumeCtrl,Swapped);
End;

Procedure GUS_FadeVolDown;
Begin
 GUSWrite(VolRampStart,$04);
 GUSWrite(VolRampEnd,$FC);
 GUSWriteDelay(VolumeCtrl,$40);
End;

Procedure GUS_SetAddress(Register : Byte; Address : DWord);
Begin
 GUSWriteW(Register,Address Shr 7);
 GUSWriteW(Register+1,Address Shl 9);
End;

{ * >>> E X T E R N A L  D E V I C E - D R I V E R  F U N C T I O N S <<< * }

Function ISS_GUSDetect : Boolean;
Begin
 ISS_GUSDetect:=GUSHardDetect;
End;

Function ISS_GUSInit : Boolean;
Begin
 ISS_GUSInit:=False;
 If Not GUSHardDetect Then Begin
   { * ERROR CODE! * }
   Exit;
  End;
 ISS_GUSDRAMOffset:=0;
 GUSGenerateVolTable;
 GUS_OutputInit;
 ISS_GUSInit:=True;
End;

Function ISS_GUSDone : Boolean;
Begin
 ISS_GUSDone:=True;
End;

Function ISS_GUSLoadSample(SStruc : ISS_PSample) : Boolean;
Type PInteger  = ^Integer;
     PShortInt = ^ShortInt;
Var SmpConvBuf  : Pointer;
    Counter     : DWord;
    RealSLength : DWord;
Begin
 ISS_GUSLoadSample:=False;

 { * Uploading sample * }
 With SStruc^ Do Begin

   { * If sample is 16bit, divide size with 2 * }
   If (SType And ISS_Smp16BitData)>0 Then Begin
     RealSLength:=SLength Div 2;
    End Else Begin
     RealSLength:=SLength;
    End;

   { * Is the sample fits into GUSRAM? * }
   If RealSLength+ISS_GUSDRAMOffset+1>ISS_GUSDevice.DevDRAMSize Then Begin
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

   GUS_RAM2DRAM(SmpConvBuf,ISS_GUSDRAMOffset,RealSLength);

   FreeMem(SmpConvBuf,RealSLength);

   SStruc^.SDRAMOffs:=ISS_GUSDRAMOffset;
   Inc(ISS_GUSDRAMOffset,RealSLength);
  End;

 ISS_GUSLoadSample:=True;
End;

Function ISS_GUSFreeSample(SStruc : ISS_PSample) : Boolean;
Begin
 ISS_GUSFreeSample:=True;
End;

Function ISS_GUSSetVolume(Volume : DWord) : Boolean;
Begin
 { * Dummy. Old GUSes have no mixer. :( Later, the support for the * }
 { * newer-series GF1's ICS mixer should be added. I'm looking for * }
 { * some programing docs about the ICS mixer! If you have the docs, * }
 { * please contact me, so i can add support for it. * }
 ISS_GUSSetVolume:=True;
End;

Function ISS_GUSStartOutput(PeriodicCall  : Pointer) : Boolean;
Begin
 ISS_GUSStartOutput:=False;

 If ISS_ActiveSSChannels>32 Then ISS_GUSActiveChannels:=32
                            Else ISS_GUSActiveChannels:=ISS_ActiveSSChannels;
 If ISS_GUSActiveChannels<14 Then ISS_GUSActiveChannels:=14;

 { * Boostin' up "The Queen Of The SoundCards" :) * }
 GUSWrite(VoicesActive,Byte((ISS_GUSActiveChannels-1) Or $C0));
 { * Phew, this was easy, wasn't it? Compare this with a Sound Blaster * }
 { * initialization code... :) * }

 ISS_GUSDivisor:=GUSFreq[ISS_GUSActiveChannels-1];

 { * Now initializing the timer call * }
 If ISS_StartTimer(PeriodicCall,(ISS_TimerSpeed Div 140)) Then Begin
   ISS_GUSStartOutput:=True;
   ISS_TimerDiff:=ISS_TimerSpeed Div 140;
   {$IFDEF _ISS_GUS_DEBUGMODE_}
     WriteLn('DEV_INIT: Starting ',ISS_GUSName,' output...');
   {$ENDIF}
  End Else Begin
   {$IFDEF _ISS_GUS_DEBUGMODE_}
     WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_GUSName,' output!');
   {$ENDIF}
  End;
End;

Function ISS_GUSStopOutput(PeriodicCall : Pointer) : Boolean;
Begin
 ISS_GUSStopOutput:=ISS_StopTimer(PeriodicCall); { * Stops timer call * }
 ISS_GUSInit; { * Reinitalizes soundcard * }
 {$IFDEF _ISS_GUS_DEBUGMODE_}
   WriteLn('DEV_INIT: ',ISS_GUSName,' output stopped.');
 {$ENDIF}
End;

Procedure ISS_GUSUpdateOutput; { * Updates the sound output * }
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
 For ChannelCounter:=0 To ISS_GUSActiveChannels-1 Do Begin
   With ISS_VirtualChannels^[ChannelCounter] Do Begin
     If ((VChControl And ISS_CCActive)>0) And
        ((VChControl And ISS_CCStop)>0) Then Begin
       Dec(VChControl,ISS_CCStop);
       GUSSetVoice(ChannelCounter);
       GUSWriteDelay(WriteVoiceMode,3);
       GUS_FadeVolDown;
      End;
    End;
  End;

 { * Wait until volume slides ends * }
 For ChannelCounter:=0 To ISS_GUSActiveChannels-1 Do Begin
   GUSSetVoice(ChannelCounter);
   Repeat Until (GUSRead(ReadVolCtrl) And 1)>0;
  End;

 { * Start Voices Update * }
 For ChannelCounter:=0 To ISS_GUSActiveChannels-1 Do Begin
   GUSSetVoice(ChannelCounter);
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
           GUS_SetAddress(SampleEndLo,LoopEnd);       { * Sample End * }
           GUS_SetAddress(LoopStartLo,LoopBegin);     { * Loop Start * }

           { * Setting sample position * }
           GUS_SetAddress(SampleStartLo,SampleBegin); { * Sample Start * }
           { * Starting the sample... * }
           GUSWriteDelay(WriteVoiceMode,SType And %00011000); { * Sample Mode * }

          End;

        End;

       { * Change Channel Panning ? * }
       If (VChControl And ISS_CCPanning)>0 Then Begin
         Dec(VChControl,ISS_CCPanning);
         GUSWrite(VoiceBalance,(VChFinalPanning Shr 4));
        End;

       { * Change Channel Volume ? * }
       If (VChControl And ISS_CCVolume)>0 Then Begin
         Dec(VChControl,ISS_CCVolume);
         GUS_FadeVol(ISS_GUSVol[VChFinalVolume Shl 2]);
        End;

       { * Change Channel Frequency ? * }
       If (VChControl And ISS_CCPeriod)>0 Then Begin
         Dec(VChControl,ISS_CCPeriod);
         SampleFreq:=((VChFreq Shl 9)+(ISS_GUSDivisor Shr 1)) Div ISS_GUSDivisor;
         SampleFreq:=SampleFreq Shl 1;
         GUSWriteW(SetVoiceFreq,SampleFreq);
        End;

       { * Is Channel Still Active ? * }
       If (GUSRead(Read) And 1)>0 Then Begin
         VChControl:=VChControl And Not ISS_CCActive;
        End;

      End;

    End;

  End;

End;

{ * >>> P U B L I C  F U N C T I O N S <<< * }

{ * Inits the GUS driver structures * }
Procedure ISS_GUSDevInit;
Begin
 With ISS_GUSDriver Do Begin
   Detect    :=@ISS_GUSDetect;
   Init      :=@ISS_GUSInit;
   Done      :=@ISS_GUSDone;
   LoadSample:=@ISS_GUSLoadSample;
   FreeSample:=@ISS_GUSFreeSample;
   SetVolume :=@ISS_GUSSetVolume;
   StartOut  :=@ISS_GUSStartOutput;
   StopOut   :=@ISS_GUSStopOutput;
   UpdateOut :=@ISS_GUSUpdateOutput;
  End;
 ISS_GUSDevice.DevDriver:=@ISS_GUSDriver;

 {$IFDEF _ISS_GUS_DEBUGMODE_}
  WriteLn('DEV_INIT: Device - ',ISS_GUSLongDesc,' ',ISS_GUSVersionStr);
 {$ENDIF}

 { * Reading ULTRASND Environment Settings * }
 If UltraGetConfig(ISS_GUSHWSetup) Then Begin
   { * If ULTRASND found, assigning hardware parameters * }
   With ISS_GUSDevice Do Begin
     DevName    :=ISS_GUSName; { * Name of the device * }
     DevType    :=ISS_Dev16Bit+ISS_DevStereo+ISS_DevSigned
                  +ISS_DevWaveTable;{ * Device Type * }
     With ISS_GUSHWSetup Do Begin
       DevBaseport:=Base_Port;
       DevIRQ     :=GF1_IRQ;
       DevDMA1    :=DRAM_DMA;
       DevDMA2    :=ADC_DMA;
       DevFreq    :=44100;
       DevMaxChan :=32;
       DevDRAMSize:=0; { * HardDetect will give a proper value * }
       DevAvail   :=GUSHardDetect;
       Case GUS_Ver Of
         $FF  : Begin
                  DevHWVer:='Classic';
                 End;
         5..9 : Begin
                  DevHWVer:='3.7+ (with ICS Mixer)';
                 End;
         $0A..$0F :Begin
                  DevHWVer:='MAX/PnP (with CS4231 Codec)';
                 End;
         Else   Begin
                  DevHWVer:='Unknown';
                 End;
        End;
      End;
    End;
  End;

 If ISS_GUSDevice.DevAvail Then Begin
   With ISS_GUSDevice Do Begin
     {$IFDEF _ISS_GUS_DEBUGMODE_}
      WriteLn('DEV_INIT:        - Hardware Revision: ',DevHWVer);
      WriteLn('DEV_INIT:        - Baseport: $',WriteHex(DevBaseport),
             ' IRQ:',DevIRQ,' DMA1:',DevDMA1,' DMA2:',DevDMA2,
             ' DRAM:',DevDRAMSize Div 1024,'KB');
     {$ENDIF}
    End;
  End Else Begin
   {$IFDEF _ISS_GUS_DEBUGMODE_}
    WriteLn('DEV_INIT:        - ULTRASOUND DETECTION FAILED!');
   {$ENDIF}
  End;
End;

Begin
End.
