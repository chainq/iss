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

{ * ISS_DART.PAS - OS/2 Warp DART (Direct Audio RouTines) Device Driver   }
{             OS - EMX (OS/2) only.                                       }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$ASMMODE INTEL}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }

Unit ISS_DART;

Interface

Uses ISS_Var,  { * Uses the system variables and types * }
     ISS_Mix,  { * Uses the mixer * }
     OS2Def,   { * Uses OS/2 system interface * }
     DOSCalls, { * Uses OS/2 DOSCALLS.DLL * }
     ISS_OS2M; { * Uses OS/2 Multimedia interface * }

Const ISS_DARTVersionStr = '0.0.2ALPHA';

      ISS_DARTName     = 'DART Device Driver';
      ISS_DARTLongDesc = 'OS/2 Warp Direct Audio (DART) Device Driver';

Var ISS_DARTDevice : ISS_TSoundDevice; { * No sound Device Structure * }
    ISS_DARTDriver : ISS_TSoundDriver; { * No sound Device Driver * }

Procedure ISS_DARTDevInit;

Implementation

Const { * MCI DART System Constants * }
      MCI_BUFFER      = 62;
      MCI_MIXSETUP    = 63;
      MCI_MAX_COMMAND = 64;
      MAX_BUFFERS     = 256;

      { * Constants for DART Callbacks * }
      MIX_STREAM_ERROR    = $00000080;
      MIX_READ_COMPLETE   = $00000001;
      MIX_WRITE_COMPLETE  = $00000002;

      { * DART constants * }
      MCI_MIXSETUP_INIT      = $00010000;
      MCI_MIXSETUP_DEINIT    = $00020000;
      MCI_MIXSETUP_QUERYMODE = $00040000;

      MCI_ALLOCATE_MEMORY    = $00040000;
      MCI_DEALLOCATE_MEMORY  = $00080000;

      MIX_BUFFER_EOS         = $00000001; { * End-Of-Stream Flag * }

      { * Additional consts * }
      STRING_LENGTH   = 128;

      DARTBufSize     = 4096;
      DARTBufNum      = 4;
      DARTStopPlay    : Boolean = False;

Type { * DART structures * }
     MCI_MIX_BUFFER = Record
       ulStructLength : DWord;     { * Structure length            * }
       pBuffer        : Pointer;   { * Pointer to a buffer         * }
       ulBufferLength : DWord;     { * Length of the buffer        * }
       ulFlags        : DWord;     { * Flags                       * }
       ulUserParm     : DWord;     { * User parameter              * }
       ulTime         : DWord;     { * Device time in ms           * }
       ulReserved1    : DWord;
       ulReserved2    : DWord;
      End;
     PMCI_MIX_BUFFER = ^MCI_MIX_BUFFER;

     { * DART Callbacks * }
     PMIXERPROC  = Function(ulHandle : DWord; pBuffer : PMCI_MIX_BUFFER; ulFlags : DWord) : Longint; cdecl;
     PMIXEREVENT = Function(ulStatus : DWord; pBuffer : PMCI_MIX_BUFFER; ulFlags : DWord) : Longint; cdecl;

     MCI_MIXSETUP_PARMS = Record
       hwndCallback    : Cardinal;   { * Window handle                 * }
       ulBitsPerSample : DWord;      { * Bits per sample               * }
       ulFormatTag     : DWord;      { * Format tag                    * }
       ulSamplesPerSec : DWord;      { * Sampling rate                 * }
       ulChannels      : DWord;      { * Number of channels            * }
       ulFormatMode    : DWord;      { * Play or record                * }
       ulDeviceType    : DWord;      { * Device type                   * }
       ulMixHandle     : DWord;      { * Mixer handle                  * }
       pmixWrite       : pMixerProc; { * Entry point                   * }
       pmixRead        : pMixerProc; { * Entry point                   * }
       pmixEvent       : pMixerEvent;{ * Entry point                   * }
       pExtendedInfo   : Pointer;    { * Extended information          * }
       ulBufferSize    : DWord;      { * Recommended buffer size       * }
       ulNumBuffers    : DWord;      { * Recommended number of buffers * }
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

Var ISS_DARTPlayFreq     : DWord;    { * Current playing freq. * }
    ISS_DARTMixBufSize   : DWord;    { * Current mixing buffer size * }
    ISS_DARTPeriodicCall : Pointer;  { * Pointer to the tracker code * }

    ISS_DARTDevTypeMul      : DWord; { * Device type multiplier, for easy buffersize calculations * }
    ISS_DARTMixFinalBufSize : DWord; { * Final buffer length * }

    ISS_DARTDeviceID        : Integer;              (* Amp Mixer device id     *)
    ISS_DARTBufferCount     : DWord;                (* Current file buffer     *)
    DARTNumBuffers        : DWord;                (* Number of file buffers  *)
    MixBuffers            : Array[0..MAX_BUFFERS-1] of MCI_MIX_BUFFER;
                                                  (* Device buffers          *)
    MixSetupParms         : MCI_MIXSETUP_PARMS;   (* Mixer parameters        *)
    BufferParms           : MCI_BUFFER_PARMS;     (* Device buffer parms     *)
    GenericParms          : MCI_GENERIC_PARMS;

Procedure DART_ISSGetBuffer(BufferNum : Integer); Forward;


{ * >>> M C I  R E L E A T E D  F U N C T I O N S <<< * }

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


{ * >>> D A R T  R E L E A T E D  F U N C T I O N S <<< * }

{ * The address to this procedure is passed to the mixer device in the    * }
{ * MIX_SETUP_PARMS structure. The mixer device then calls this procedure * }
{ * when it has expended a buffer.                                        * }
{ * NOTE: This is a high priority thread. Too much code here will bog the * }
{ * system down.                                                          * }
Function DART_SoundEvent(Status: DWord; Buffer: PMCI_MIX_BUFFER;
                         Flags: DWord) : LongInt; CDecl;
Begin
  Case Flags Of
    MIX_STREAM_ERROR Or MIX_READ_COMPLETE ,  { * Error occur in device * }
    MIX_STREAM_ERROR Or MIX_WRITE_COMPLETE:  { * Error occur in device * }
      If Status = ERROR_DEVICE_UNDERRUN Then Begin
         MixSetupParms.pmixWrite(MixSetupParms.ulMixHandle,@MixBuffers[0],
                        DARTNumBuffers );
      End;
    MIX_WRITE_COMPLETE: Begin { * Normal playback * }
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

{ * Opens Amp-Mixer Device * }
Function DART_AmpMixOpen : Boolean;
Var AmpOpenParms : MCI_AMP_OPEN_PARMS;
    Device       : Word;
Begin
  Device:=2;
  { * Open the mixer device * }
  FillChar(AmpOpenParms,SizeOf(MCI_AMP_OPEN_PARMS),0);
  AmpOpenParms.usDeviceID:=0;
  AmpOpenParms.pszDeviceType:=Pointer(DWord(MCI_DEVTYPE_AUDIO_AMPMIX+(Device Shl 16)));

  DART_AmpMixOpen:=MCICommand(0,MCI_OPEN,
                              MCI_WAIT Or MCI_OPEN_TYPE_ID Or MCI_OPEN_SHAREABLE,
                              AmpOpenParms);
  ISS_DARTDeviceID:=AmpOpenParms.usDeviceID;
End;

{ * Deallocate memory and close the Amp-Mixer Device. * }
Function DART_AmpMixClose : Boolean;
Begin
  DART_AmpMixClose:=False;
  If Not MCICommand(ISS_DARTDeviceID,MCI_BUFFER,MCI_WAIT Or MCI_DEALLOCATE_MEMORY,
                BufferParms) Then Exit;
  DART_AmpMixClose:=MCICommand(ISS_DARTDeviceID,MCI_CLOSE,MCI_WAIT,GenericParms);
End;

{ * Starts playback with sending buffers to the Amp-Mixer * }
Procedure DART_StartPlayBack;
Begin
  If DARTNumBuffers > 1 then begin
    ISS_DARTBufferCount := 2;
    { * Write two buffers to kick off the amp mixer. * }
    MixSetupParms.pmixWrite( MixSetupParms.ulMixHandle, @MixBuffers, 2 );
  End Else Begin
    ISS_DARTBufferCount := 1;
    { * Write one buffer. * }
    MixSetupParms.pmixWrite( MixSetupParms.ulMixHandle, @MixBuffers, 1 );
  End;
End;


{ * >>> I N T E R N A L  F U N C T I O N S <<< * }

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
  { * Set the DART_MixSetupParms data structure to match the loaded file. * }
  { * This is a global that is used to setup the mixer. * }

  FillChar(MixSetupParms,SizeOf(MCI_MIXSETUP_PARMS),0);
  With Device Do Begin
    With MixSetupParms Do Begin
      If (DevType And ISS_Dev16Bit)>0 Then ulBitsPerSample:=16
                                      Else ulBitsPerSample:=8;
      ulFormatTag:=MCI_WAVE_FORMAT_PCM;
      ulSamplesPerSec:=DevFreq;
      If (DevType And ISS_DevStereo)>0 Then ulChannels:=2
                                       Else ulChannels:=1;

      { * Setup the mixer for playback of wave data * }
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

  { * Calculating Buffer Length * }
  BufferSize:=ISS_DARTMixFinalBufSize;
  { * Multiply the buffer size to be sure this will be big enough for * }
  { * any kind of device... Still needs to be balanced for optimal use. * }
  BufferSize:=BufferSize*24;

  { * Set up the BufferParms data structure and allocate * }
  { * device buffers from the Amp-Mixer * }
  MixSetupParms.ulBufferSize:=BufferSize;
  With BufferParms Do Begin
    ulNumBuffers := DARTNumBuffers;
    ulBufferSize := MixSetupParms.ulBufferSize;
    pBufList     := @MixBuffers;
   End;

  If Not MCICommand(ISS_DARTDeviceID, MCI_BUFFER, MCI_WAIT or MCI_ALLOCATE_MEMORY,
                    BufferParms) Then Exit;

  { * Clean up buffers and set the buffer size * }
  For Counter:=0 To DARTNumBuffers-1 Do Begin
    With MixBuffers[Counter] Do Begin
      FillChar(pBuffer^,BufferParms.ulBufferSize,0);
      ulBufferLength:=BufferParms.ulBufferSize;
     End;
   End;
End;


{ * >>> E X T E R N A L  D E V I C E - D R I V E R  F U N C T I O N S <<< * }

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

   { * Calculating buffer size multiplier and final buffer size * }
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

{ * This procedure assigns the device driver procedures * }
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
   DevAvail   :=True;         { * Device is available (for detection) * }
   DevName    :=ISS_DARTName; { * Name of the device * }
   DevType    :=ISS_DevStereo+ISS_DevSigned+ISS_Dev16Bit+ISS_DevMixed; { * Device Type * }
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
