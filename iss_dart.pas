{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ ISS_DART.PAS - OS/2 Warp DART (Direct Audio RouTines) Device Driver    ³}
{³                  Work started     : 2001.03.02.                          ³}
{³                  Last modification: 2001.06.29.                          ³}
{³             OS - EMX (OS/2) only.                                        ³}
{³                                                                          ³}
{³            ISS - Inquisition Sound Server for Free Pascal                ³}
{³                  Code by Karoly Balogh (a.k.a. Charlie/iNQ)              ³}
{³                  Copyright (C) 1998-2001 Inquisition                     ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$ASMMODE INTEL}

{$HINTS OFF} { þ Enable this if you modify the source! þ }
{$NOTES OFF} { þ Enable this if you modify the source! þ }

Unit ISS_DART;

Interface

Uses ISS_Var,  { þ Uses the system variables and types þ }
     ISS_Mix,  { þ Uses the mixer þ }
     OS2Def,   { þ Uses OS/2 system interface þ }
     DOSCalls, { þ Uses OS/2 DOSCALLS.DLL þ }
     ISS_OS2M; { þ Uses OS/2 Multimedia interface þ }

Const ISS_DARTVersionStr = '0.0.2ALPHA';

      ISS_DARTName     = 'DART Device Driver';
      ISS_DARTLongDesc = 'OS/2 Warp Direct Audio (DART) Device Driver';

Var ISS_DARTDevice : ISS_TSoundDevice; { þ No sound Device Structure þ }
    ISS_DARTDriver : ISS_TSoundDriver; { þ No sound Device Driver þ }

Procedure ISS_DARTDevInit;

Implementation

Const { þ MCI DART System Constants þ }
      MCI_BUFFER      = 62;
      MCI_MIXSETUP    = 63;
      MCI_MAX_COMMAND = 64;
      MAX_BUFFERS     = 256;

      { þ Constants for DART Callbacks þ }
      MIX_STREAM_ERROR    = $00000080;
      MIX_READ_COMPLETE   = $00000001;
      MIX_WRITE_COMPLETE  = $00000002;

      { þ DART constants þ }
      MCI_MIXSETUP_INIT      = $00010000;
      MCI_MIXSETUP_DEINIT    = $00020000;
      MCI_MIXSETUP_QUERYMODE = $00040000;

      MCI_ALLOCATE_MEMORY    = $00040000;
      MCI_DEALLOCATE_MEMORY  = $00080000;

      MIX_BUFFER_EOS         = $00000001; { þ End-Of-Stream Flag þ }

      { þ Additional consts þ }
      STRING_LENGTH   = 128;

      DARTBufSize     = 4096;
      DARTBufNum      = 4;
      DARTStopPlay    : Boolean = False;

Type { þ DART structures þ }
     MCI_MIX_BUFFER = Record
       ulStructLength : DWord;     { þ Structure length            þ }
       pBuffer        : Pointer;   { þ Pointer to a buffer         þ }
       ulBufferLength : DWord;     { þ Length of the buffer        þ }
       ulFlags        : DWord;     { þ Flags                       þ }
       ulUserParm     : DWord;     { þ User parameter              þ }
       ulTime         : DWord;     { þ Device time in ms           þ }
       ulReserved1    : DWord;
       ulReserved2    : DWord;
      End;
     PMCI_MIX_BUFFER = ^MCI_MIX_BUFFER;

     { þ DART Callbacks þ }
     PMIXERPROC  = Function(ulHandle : DWord; pBuffer : PMCI_MIX_BUFFER; ulFlags : DWord) : Longint; cdecl;
     PMIXEREVENT = Function(ulStatus : DWord; pBuffer : PMCI_MIX_BUFFER; ulFlags : DWord) : Longint; cdecl;

     MCI_MIXSETUP_PARMS = Record
       hwndCallback    : Cardinal;   { þ Window handle                 þ }
       ulBitsPerSample : DWord;      { þ Bits per sample               þ }
       ulFormatTag     : DWord;      { þ Format tag                    þ }
       ulSamplesPerSec : DWord;      { þ Sampling rate                 þ }
       ulChannels      : DWord;      { þ Number of channels            þ }
       ulFormatMode    : DWord;      { þ Play or record                þ }
       ulDeviceType    : DWord;      { þ Device type                   þ }
       ulMixHandle     : DWord;      { þ Mixer handle                  þ }
       pmixWrite       : pMixerProc; { þ Entry point                   þ }
       pmixRead        : pMixerProc; { þ Entry point                   þ }
       pmixEvent       : pMixerEvent;{ þ Entry point                   þ }
       pExtendedInfo   : Pointer;    { þ Extended information          þ }
       ulBufferSize    : DWord;      { þ Recommended buffer size       þ }
       ulNumBuffers    : DWord;      { þ Recommended number of buffers þ }
      End;
     PMCI_MIXSETUP_PARMS = ^MCI_MIXSETUP_PARMS;

     MCI_BUFFER_PARMS = Record
       hwndCallback   : Cardinal;
       ulStructLength : DWord;
       ulNumBuffers   : DWord;
       ulBufferSize   : DWord;
       ulMinToStart   : DWord;
       ulSrcStart     : DWord;
       ulTgtStart     : DWord;
       pBufList       : Pointer;
      End;

Var ISS_DARTPlayFreq     : DWord;    { þ Current playing freq. þ }
    ISS_DARTMixBufSize   : DWord;    { þ Current mixing buffer size þ }
    ISS_DARTPeriodicCall : Pointer;  { þ Pointer to the tracker code þ }

    ISS_DARTDevTypeMul      : DWord; { þ Device type multiplier, for easy buffersize calculations þ }
    ISS_DARTMixFinalBufSize : DWord; { þ Final buffer length þ }

    ISS_DARTDeviceID        : Integer;              (* Amp Mixer device id     *)
    ISS_DARTBufferCount     : DWord;                (* Current file buffer     *)
    DARTNumBuffers        : DWord;                (* Number of file buffers  *)
    MixBuffers            : Array[0..MAX_BUFFERS-1] of MCI_MIX_BUFFER;
                                                  (* Device buffers          *)
    MixSetupParms         : MCI_MIXSETUP_PARMS;   (* Mixer parameters        *)
    BufferParms           : MCI_BUFFER_PARMS;     (* Device buffer parms     *)
    GenericParms          : MCI_GENERIC_PARMS;

Procedure DART_ISSGetBuffer(BufferNum : Integer); Forward;


{ þ >>> M C I  R E L E A T E D  F U N C T I O N S <<< þ }

Procedure MCIError(ErrorCode : DWord);
Var StrBuffer : Array[0..STRING_LENGTH-1] Of Char;
Begin
  MCIGetErrorString(ErrorCode,StrBuffer,STRING_LENGTH);
{ Writeln(StrBuffer);}
End;

Function MCICommand(DeviceID: Integer; Message: Integer; Param1: DWord; Var Param2) : Boolean;
Var RC : DWord;
Begin
  RC:=MCISendCommand(DeviceID,Message,Param1,Param2,0);
  MciCommand:=(RC=MCIERR_SUCCESS);
  If RC <> MCIERR_SUCCESS Then MCIError(RC);
End;


{ þ >>> D A R T  R E L E A T E D  F U N C T I O N S <<< þ }

{ þ The address to this procedure is passed to the mixer device in the    þ }
{ þ MIX_SETUP_PARMS structure. The mixer device then calls this procedure þ }
{ þ when it has expended a buffer.                                        þ }
{ þ NOTE: This is a high priority thread. Too much code here will bog the þ }
{ þ system down.                                                          þ }
Function DART_SoundEvent(Status: DWord; Buffer: PMCI_MIX_BUFFER;
                         Flags: DWord) : LongInt; CDecl;
Begin
  Case Flags Of
    MIX_STREAM_ERROR Or MIX_READ_COMPLETE ,  { þ Error occur in device þ }
    MIX_STREAM_ERROR Or MIX_WRITE_COMPLETE:  { þ Error occur in device þ }
      If Status = ERROR_DEVICE_UNDERRUN Then Begin
         MixSetupParms.pmixWrite(MixSetupParms.ulMixHandle,@MixBuffers[0],
                        DARTNumBuffers );
      End;
    MIX_WRITE_COMPLETE: Begin { þ Normal playback þ }
      If (DARTStopPlay=True) Then Begin
        MCISendCommand(ISS_DARTDeviceID,MCI_STOP,MCI_WAIT,GenericParms,0);
       End Else Begin
        DART_ISSGetBuffer(ISS_DARTBufferCount);
        MixSetupParms.pmixWrite(MixSetupParms.ulMixHandle,
                                @MixBuffers[ISS_DARTBufferCount],1);
        Inc(ISS_DARTBufferCount);
        If ISS_DARTBufferCount>DARTNumBuffers-1 Then ISS_DARTBufferCount:=0;
       End;
     End;
   End;
  DART_SoundEvent:=LongInt(TRUE);
End;

{ þ Opens Amp-Mixer Device þ }
Function DART_AmpMixOpen : Boolean;
Var AmpOpenParms : MCI_AMP_OPEN_PARMS;
    Device       : Word;
Begin
  Device:=2;
  { þ Open the mixer device þ }
  FillChar(AmpOpenParms,SizeOf(MCI_AMP_OPEN_PARMS),0);
  AmpOpenParms.usDeviceID:=0;
  AmpOpenParms.pszDeviceType:=Pointer(DWord(MCI_DEVTYPE_AUDIO_AMPMIX+(Device Shl 16)));

  DART_AmpMixOpen:=MCICommand(0,MCI_OPEN,
                              MCI_WAIT Or MCI_OPEN_TYPE_ID Or MCI_OPEN_SHAREABLE,
                              AmpOpenParms);
  ISS_DARTDeviceID:=AmpOpenParms.usDeviceID;
End;

{ þ Deallocate memory and close the Amp-Mixer Device. þ }
Function DART_AmpMixClose : Boolean;
Begin
  DART_AmpMixClose:=False;
  If Not MCICommand(ISS_DARTDeviceID,MCI_BUFFER,MCI_WAIT Or MCI_DEALLOCATE_MEMORY,
                BufferParms) Then Exit;
  DART_AmpMixClose:=MCICommand(ISS_DARTDeviceID,MCI_CLOSE,MCI_WAIT,GenericParms);
End;

{ þ Starts playback with sending buffers to the Amp-Mixer þ }
Procedure DART_StartPlayBack;
Begin
  If DARTNumBuffers > 1 then begin
    ISS_DARTBufferCount := 2;
    { þ Write two buffers to kick off the amp mixer. þ }
    MixSetupParms.pmixWrite( MixSetupParms.ulMixHandle, @MixBuffers, 2 );
  End Else Begin
    ISS_DARTBufferCount := 1;
    { þ Write one buffer. þ }
    MixSetupParms.pmixWrite( MixSetupParms.ulMixHandle, @MixBuffers, 1 );
  End;
End;


{ þ >>> I N T E R N A L  F U N C T I O N S <<< þ }

Procedure DART_ISSGetBuffer(BufferNum : Integer);
Type Proc = Procedure;
Var DARTBufSize : DWord;
    DARTBuf     : Pointer;
Begin
  With ISS_MixerData^ Do Begin
    MixBufOffs:=DWord(MixBuffers[BufferNum].PBuffer);
    DARTBufSize:=DWord(MixBuffers[BufferNum].ulBufferLength);
    While ISS_DARTMixFinalBufSize<=DARTBufSize Do Begin
      Proc(ISS_DARTPeriodicCall);
      Inc(DWord(MixBufOffs),ISS_DARTMixFinalBufSize);
      Dec(DARTBufSize,ISS_DARTMixFinalBufSize);
     End;
   End;
End;


Function DART_AmpMixerSetup(Device : ISS_TSoundDevice) : Boolean;
Begin
  DART_AmpMixerSetup:=False;
  If ISS_DARTDeviceID=0 Then Exit;
  { þ Set the DART_MixSetupParms data structure to match the loaded file. þ }
  { þ This is a global that is used to setup the mixer. þ }

  FillChar(MixSetupParms,SizeOf(MCI_MIXSETUP_PARMS),0);
  With Device Do Begin
    With MixSetupParms Do Begin
      If (DevType And ISS_Dev16Bit)>0 Then ulBitsPerSample:=16
                                      Else ulBitsPerSample:=8;
      ulFormatTag:=MCI_WAVE_FORMAT_PCM;
      ulSamplesPerSec:=DevFreq;
      If (DevType And ISS_DevStereo)>0 Then ulChannels:=2
                                       Else ulChannels:=1;

      { þ Setup the mixer for playback of wave data þ }
      ulFormatMode:=MCI_PLAY;
      ulDeviceType:=MCI_DEVTYPE_WAVEFORM_AUDIO;
      pmixEvent   :=@DART_SoundEvent;
     End;
   End;
  DART_AmpMixerSetup := mciCommand(ISS_DARTDeviceID, MCI_MIXSETUP,
                     MCI_WAIT or MCI_MIXSETUP_INIT,
                     MixSetupParms);
End;

Function DART_InitStream(Device : ISS_TSoundDevice) : Boolean;
Var Counter    : DWord;
    BufferSize : DWord;
Begin
  DART_InitStream:=False;
  If Not DART_AmpMixerSetup(Device) Then Exit;

  DARTNumBuffers := DARTBufNum;

  { þ Calculating Buffer Length þ }
  BufferSize:=ISS_DARTMixFinalBufSize;
  { þ Multiply the buffer size to be sure this will be big enough for þ }
  { þ any kind of device... Still needs to be balanced for optimal use. þ }
  BufferSize:=BufferSize*24;

  { þ Set up the BufferParms data structure and allocate þ }
  { þ device buffers from the Amp-Mixer þ }
  MixSetupParms.ulBufferSize:=BufferSize;
  With BufferParms Do Begin
    ulNumBuffers := DARTNumBuffers;
    ulBufferSize := MixSetupParms.ulBufferSize;
    pBufList     := @MixBuffers;
   End;

  If Not MCICommand(ISS_DARTDeviceID, MCI_BUFFER, MCI_WAIT or MCI_ALLOCATE_MEMORY,
                    BufferParms) Then Exit;

  { þ Clean up buffers and set the buffer size þ }
  For Counter:=0 To DARTNumBuffers-1 Do Begin
    With MixBuffers[Counter] Do Begin
      FillChar(pBuffer^,BufferParms.ulBufferSize,0);
      ulBufferLength:=BufferParms.ulBufferSize;
     End;
   End;
End;


{ þ >>> E X T E R N A L  D E V I C E - D R I V E R  F U N C T I O N S <<< þ }

Function ISS_DARTDetect : Boolean;
Begin
 ISS_DARTDetect:=True;
End;

Function ISS_DARTInit : Boolean;
Begin
 ISS_DARTInit:=True;
End;

Function ISS_DARTDone : Boolean;
Begin
 ISS_DARTDone:=True;
End;

Function ISS_DARTSetVolume(Volume : DWord) : Boolean;
Begin
 ISS_DARTSetVolume:=True;
End;

Function ISS_DARTStartOutput(PeriodicCall : Pointer) : Boolean;
Begin
 ISS_DARTStartOutput:=False;

 With ISS_DARTDevice Do Begin
   ISS_DARTPlayFreq:=DevFreq;
   ISS_TimerDiff:=1193180 Div 180;
   ISS_DARTMixBufSize:=ISS_DARTPlayFreq Div 180;

   { þ Calculating buffer size multiplier and final buffer size þ }
   ISS_DARTDevTypeMul:=1;
   If (DevType And ISS_DevStereo)>0 Then ISS_DARTDevTypeMul:=ISS_DARTDevTypeMul*2;
   If (DevType And ISS_Dev16Bit)>0  Then ISS_DARTDevTypeMul:=ISS_DARTDevTypeMul*2;
   ISS_DARTMixFinalBufSize:=ISS_DARTMixBufSize*ISS_DARTDevTypeMul;

   If DART_AmpMixOpen Then Begin
     DART_InitStream(ISS_DARTDevice);
     ISS_DARTPeriodicCall:=PeriodicCall;

     If (DevType And ISS_Dev16Bit)>0 Then Begin
       ISS_MixerInit(ISS_DARTPlayFreq,ISS_DARTMixBufSize,0,0,ISS_DARTDevice.DevType+ISS_DevSigned);
      End Else Begin
       ISS_MixerInit(ISS_DARTPlayFreq,ISS_DARTMixBufSize,0,0,ISS_DARTDevice.DevType+ISS_DevUnsigned);
      End;

     DART_StartPlayBack;

     ISS_DARTStartOutput:=True;
     {$IFDEF _ISS_DART_DEBUGMODE_}
      WriteLn('DEV_INIT: Starting ',ISS_DARTName,' output...');
     {$ENDIF}
    End Else Begin
     {$IFDEF _ISS_DART_DEBUGMODE_}
      WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_DARTName,' output!');
     {$ENDIF}
    End;
  End;
End;

Function ISS_DARTStopOutput(PeriodicCall : Pointer) : Boolean;
var WaitTime:word;
Begin
 ISS_DARTStopOutput:=True;{ISS_StopTimer(PeriodicCall);}

 DART_AmpMixClose;

 ISS_MixerDone;
 {$IFDEF _ISS_DART_DEBUGMODE_}
   WriteLn('DEV_INIT: ',ISS_DARTName,' output stopped.');
 {$ENDIF}
End;

{ þ This procedure assigns the device driver procedures þ }
Procedure ISS_DARTDevInit;
Begin
 With ISS_DARTDriver Do Begin
   Detect    :=@ISS_DARTDetect;
   Init      :=@ISS_DARTInit;
   Done      :=@ISS_DARTDone;
   SetVolume :=@ISS_DARTSetVolume;
   StartOut  :=@ISS_DARTStartOutput;
   StopOut   :=@ISS_DARTStopOutput;
   LoadSample:=@ISS_MixerLoadSample;
   FreeSample:=@ISS_MixerFreeSample;
   UpdateOut :=@ISS_MixerUpdateOutput;
  End;
 ISS_DARTDevice.DevDriver:=@ISS_DARTDriver;

 {$IFDEF _ISS_DART_DEBUGMODE_}
  WriteLn('DEV_INIT: Device - ',ISS_DARTLongDesc,' ',ISS_DARTVersionStr);
 {$ENDIF}

 With ISS_DARTDevice Do Begin
   DevAvail   :=True;         { þ Device is available (for detection) þ }
   DevName    :=ISS_DARTName; { þ Name of the device þ }
   DevType    :=ISS_DevStereo+ISS_DevSigned+ISS_Dev16Bit+ISS_DevMixed; { þ Device Type þ }
   DevBaseport:=0;
   DevIRQ     :=0;
   DevDMA1    :=0;
   DevDMA2    :=0;
   DevFreq    :=44100;
   DevMaxChan :=256;
   DevDRAMSize:=0;
  End;
End;

Begin
End.
{ þ ISS_DART.PAS - (C) 2001 Charlie/Inquisition þ }
