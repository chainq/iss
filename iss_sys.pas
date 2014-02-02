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

{ * ISS_SYS .PAS - "Heart of the Tiger" :-)                               }
{             OS - GO32V2,EMX                                             }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
Unit ISS_Sys;

Interface

Uses ISS_Var,  { * Uses the system variables and types * }
     ISS_Load  { * Uses the file-loader functions * }
     {$IFDEF _ISS_GUS_INCLUDE_}
      ,ISS_GUS { * Include GUS * }
     {$ENDIF}
     {$IFDEF _ISS_SB_INCLUDE_}
      ,ISS_SB { * Include SB * }
     {$ENDIF}
     {$IFDEF _ISS_NSND_INCLUDE_}
      ,ISS_NSND { * Include Nosound * }
     {$ENDIF}
     {$IFDEF _ISS_GUSNATIVE_INCLUDE_}
      ,ISS_GUS2 { * Include OS/2 native GF1/Interwave driver * }
     {$ENDIF}
     {$IFDEF _ISS_DART_INCLUDE_}
      ,ISS_DART { * Include OS/2 Warp DART API driver * }
     {$ENDIF}
     {$IFDEF _ISS_DSOUND_INCLUDE_}
      ,ISS_DSND { * Include Win32 DirectSound API driver * }
     {$ENDIF}
     {$IFDEF _ISS_OSS_INCLUDE_}
      ,ISS_OSS  { * Include Linux OSS API driver * }
     {$ENDIF}
     ;

{ * System Initalization * }
Function ISS_InitSystem : Boolean;
Function ISS_InitDevices : Boolean;

Function ISS_Setup : Boolean;
Function ISS_AutoSetup : Boolean;
Function ISS_Init : Boolean;
Function ISS_Done : Boolean;

Function ISS_SetActiveChannels(Channels : DWord) : Boolean;

Function ISS_LoadSample(Sample : ISS_PSample) : Boolean;
Function ISS_FreeSample(Sample : ISS_PSample) : Boolean;
Function ISS_StartOutput(PeriodicProc : Pointer) : Boolean;
Function ISS_StopOutput : Boolean;

{ * Period Control * }
Function ISS_GetPeriod(Channel : Word) : DWord;
Function ISS_SetPeriod(Channel : Word; Period : Word) : Boolean;
Function ISS_ChGetNotePeriod(Channel : DWord; Note : DWord) : DWord;
Function ISS_SmpGetNotePeriod(SampleAddress : ISS_PSample;
                              Note          : DWord) : DWord;

{ * Volume & Panning Control * }
Function ISS_GetGlobalVolume : DWord;
Function ISS_SetGlobalVolume(Volume : Byte) : Boolean;
Function ISS_SetVolume(Channel : Word; Volume : Byte) : Boolean;
Function ISS_GetVolume(Channel : Word) : DWord;
Function ISS_SetPanning(Channel : Word; Panning : Byte) : Boolean;
Function ISS_GetPanning(Channel : Word) : DWord;

{ * Instrument Control * }
Procedure ISS_InitInstrument(Channel : Word);
Function  ISS_StartInstrument(Channel : Word; Instr : Word; Note : Word) : Boolean;
Procedure ISS_UpdateInstruments;
Procedure ISS_KeyOff(Channel : Word);

{ * Misc. * }
Procedure ISS_UpdateOutput; { * Updates the output device.    * }
                            { *  - Volume calc.               * }
                            { *  - Calls the low-level driver * }

Procedure ISS_SetSampleOffset(Channel : DWord; SampOffset : DWord);

Var ISS_SystemOK       : Boolean; { * True if the system is initiated and * }
                                  { * everything (seems:) OK. * }

    ISS_DevicesOK      : Boolean; { * True if there is an useable device * }
    ISS_DevicesNum     : DWord;   { * Number of devices * }
    ISS_SoundDeviceNum : DWord;   { * Number of selected device * }

    ISS_SoundDevice  : ISS_PSoundDevice;
    ISS_SoundDriver  : ISS_PSoundDriver;
    ISS_SoundDevices : Array[1..ISS_MaxDevices] Of ISS_PSoundDevice;

    {$IFDEF _ISS_MAIN_DEBUGMODE_}
     ISS_CompileCoder : String;
    {$ENDIF}

Implementation

{ * Including Linear Frequency Table * }
{$INCLUDE ISS_FREQ.INC}

Var ISS_SysStarted   : Boolean;
    ISS_PeriodicCall : Pointer;

{ * >>> I N T E R N A L  F U N C T I O N S <<< * }

Function ISS_DetectBestDev : DWord; { * Returns Best Device Number * }
Var Counter   : DWord;
Begin
 Counter:=0;
 Repeat
  Inc(Counter);
  If Counter>ISS_DevicesNum Then Begin
    { * Musn't happen, because Nosound device should be always linked * }
    ISS_DetectBestDev:=$FFFFFFFF;
    ISS_SoundDeviceNum:=$FFFFFFFF;
    ISS_DevicesOK:=False;
    Exit;
   End;
 Until ISS_SoundDevices[Counter]^.DevDriver^.Detect();
 ISS_DevicesOK:=True;
 ISS_SoundDeviceNum:=Counter;
 ISS_DetectBestDev:=Counter;
End;

Function ISS_InitDev : Boolean; { * Initializes the selected Device * }
Begin
 ISS_SoundDevice:=ISS_SoundDevices[ISS_SoundDeviceNum];
 ISS_SoundDriver:=ISS_SoundDevice^.DevDriver;
 ISS_InitDev:=ISS_SoundDriver^.Init(); { * Initializes the Device * }
End;

Function ISS_DoneDev : Boolean; { * Closes the selected Device * }
Begin
 ISS_DoneDev:=False;
 If ISS_SoundDriver^.Done() Then Begin
   ISS_SoundDevice:=Nil;
   ISS_SoundDriver:=Nil;
   ISS_DoneDev:=True;
  End;
End;

{ * >>> P U B L I C  F U N C T I O N S <<< * }

Function ISS_Setup : Boolean;
Begin
 ISS_Setup:=ISS_AutoSetup;
End;

Function ISS_AutoSetup : Boolean;
Begin
 ISS_AutoSetup:=False;
 ISS_DetectBestDev; { * Detecting Best Device * }
 If ISS_DevicesOK Then Begin { * If OK, setting up the device! * }
   ISS_SoundDevice:=ISS_SoundDevices[ISS_SoundDeviceNum];
   ISS_SoundDriver:=ISS_SoundDevice^.DevDriver;
   With ISS_SoundDevice^ Do Begin
     DevMixRate:=DevFreq;
     DevMode:=DevType;
     {$IFDEF _ISS_MAIN_DEBUGMODE_}
      WriteLn('ISS_INIT: Selected Device - ',DevName);
     {$ENDIF}
    End;
   ISS_AutoSetup:=True;
  End Else Begin
   {$IFDEF _ISS_MAIN_DEBUGMODE_}
    WriteLn('ISS_FAIL: Failed To Auto Select Device!');
   {$ENDIF}
  End;
End;

Function ISS_Init : Boolean;
Begin
 ISS_Init:=False;
 If (Not ISS_SysStarted) And (Not ISS_SystemOK) Then Begin
   If Not ISS_InitDev Then Exit; { * Currently selected device is in error! * }
   If ISS_VirtualChannels=Nil Then New(ISS_VirtualChannels);
   FillChar(ISS_VirtualChannels^,SizeOf(ISS_TVirtualChannels),#0);
   ISS_Init:=True;
   ISS_SystemOK:=True;
  End;
 ISS_GlobalSSVolume:=128;
End;

Function ISS_Done : Boolean;
Begin
 ISS_Done:=False;
 If (Not ISS_SysStarted) And (ISS_SystemOK) Then Begin
   If ISS_VirtualChannels<>Nil Then Dispose(ISS_VirtualChannels);
   ISS_SystemOK:=False;
   ISS_Done:=ISS_DoneDev;
  End;
End;

{ * Sets the active channels number * }
Function ISS_SetActiveChannels(Channels : DWord) : Boolean;
Begin
 ISS_SetActiveChannels:=False;
 If (Channels<=ISS_MaxSSChannels) And
    (Channels<=ISS_SoundDevice^.DevMaxChan) Then Begin
   ISS_ActiveSSChannels:=Channels;
   ISS_SetActiveChannels:=True;
   Exit;
  End Else Begin
   { * ERROR CODE! * }
   { * Too many channels * }
  End;
End;

Function ISS_LoadSample(Sample : ISS_PSample) : Boolean;
Begin
 ISS_LoadSample:=False;
 If ISS_DevicesOK Then Begin
   ISS_LoadSample:=ISS_SoundDriver^.LoadSample(Sample);
  End;
End;

Function ISS_FreeSample(Sample : ISS_PSample) : Boolean;
Begin
 ISS_FreeSample:=False;
 If ISS_DevicesOK Then Begin
   ISS_FreeSample:=ISS_SoundDriver^.FreeSample(Sample);
  End;
End;

Function ISS_StartOutput(PeriodicProc : Pointer) : Boolean;
Begin
 ISS_StartOutput:=False;
 If ISS_DevicesOK Then Begin
   If ISS_SysStarted Then Exit; { * Exit if output device is active * }

   { * Starting Device * }
   ISS_PeriodicCall:=PeriodicProc;
   ISS_SysStarted:=ISS_SoundDriver^.StartOut(ISS_PeriodicCall);

   ISS_StartOutput:=ISS_SysStarted;
  End;
End;

Function ISS_StopOutput : Boolean;
Begin
 ISS_StopOutput:=False;
 If ISS_DevicesOK Then Begin
   If Not ISS_SysStarted Then Exit; { * Exit if output device is inactive * }

   { * Stopping Device * }
   ISS_SysStarted:=Not ISS_SoundDriver^.StopOut(ISS_PeriodicCall);

   ISS_StopOutput:=Not ISS_SysStarted;
  End;
End;


{ * Returns Actual Period for the specified Channel * }
Function ISS_GetPeriod(Channel : Word) : DWord;
Begin
 If ISS_DevicesOK And ISS_SysStarted Then Begin
   ISS_GetPeriod:=ISS_VirtualChannels^[Channel].VChPeriod;
  End;
End;

{ * Sets Period for the specified Channel * }
Function ISS_SetPeriod(Channel : Word; Period : Word) : Boolean;
Begin
 ISS_SetPeriod:=False;
 If ISS_DevicesOK And ISS_SysStarted Then Begin

   With ISS_VirtualChannels^[Channel] Do Begin
     { * Don't set the period, if there is no sample address * }
     If VChSmpAddr=Nil Then Exit;

     VChPeriod :=Period;
     VChControl:=VChControl Or ISS_CCPeriod;
    End;
  End;
End;

{ * Get the period from the note and the sample definition * }
Function ISS_SmpGetNotePeriod(SampleAddress : ISS_PSample;
                              Note          : DWord) : DWord;
Var BufPeriod : DWord;
    RealNote  : LongInt;
Begin
 ISS_SmpGetNotePeriod:=0;

 If (SampleAddress<>Nil) And (Note<=95) Then Begin
   With SampleAddress^ Do Begin

     RealNote:=(Note-1)+SRelNote;

     { * Check Sample Realtive Note * }
     If (RealNote>0) Or (RealNote<=118) Then Begin
       { * Linear Period * }
       { * Amiga periods should be implemented later. * }
       BufPeriod:=7680-(RealNote*64)-(SFineTune Div 2);
       ISS_SmpGetNotePeriod:=BufPeriod;
      End;

    End;
  End;

End;

{ * Get the period from the note and channel number. * }
Function ISS_ChGetNotePeriod(Channel : DWord; Note : DWord) : DWord;
Begin
 ISS_ChGetNotePeriod:=
   ISS_SmpGetNotePeriod(ISS_VirtualChannels^[Channel].VChSmpAddr,Note);
End;

{ * Calculates the frequency from the specified period, sets it to * }
{ * the specified channel, and returns it to the caller. * }
Function ISS_SetFrequency(Channel : DWord; Period : DWord) : DWord;

Var BufFreq : DWord;
Begin
 ISS_SetFrequency:=0;
 With ISS_VirtualChannels^[Channel] Do Begin

   { * Linear Frequency * }
   { * Amiga frequency should be implemented later. * }
   BufFreq:=ISS_LinearFreqTable[Period Mod 768];
   BufFreq:=BufFreq Shr (Period Div 768);

   VChFreq:=BufFreq;
   ISS_SetFrequency:=VChFreq;

  End;
End;


{ * Returns Global System Volume * }
Function ISS_GetGlobalVolume : DWord;
Begin
 ISS_GetGlobalVolume:=ISS_GlobalSSVolume;
End;

{ * Sets Global System Volume - Volume Range : 0-64 * }
Function ISS_SetGlobalVolume(Volume : Byte) : Boolean;
Var Counter : DWord;
Begin
 ISS_SetGlobalVolume:=False;
 If Volume>$80 Then Volume:=$80;

 ISS_GlobalSSVolume:=Volume; { * Setting Global Volume * }

 For Counter:=0 To ISS_ActiveSSChannels-1 Do Begin
   With ISS_VirtualChannels^[Counter] Do Begin
     VChControl:=VChControl Or ISS_CCVolume;
    End;
  End;

 ISS_SetGlobalVolume:=True;
End;

{ * Sets volume on the specified channel - Volume range : 0-64 * }
Function ISS_SetVolume(Channel : Word; Volume : Byte) : Boolean;
Begin
 ISS_SetVolume:=False;
 If Volume>$40 Then Volume:=$40;

 If Channel>ISS_MaxSSChannels Then Exit;

 With ISS_VirtualChannels^[Channel] Do Begin
   VChVolume:=Volume;
   VChControl:=VChControl Or ISS_CCVolume;
  End;

 ISS_SetVolume:=True;
End;

{ * Gets volume from the specified channel * }
Function ISS_GetVolume(Channel : Word) : DWord;
Begin
 If Channel>ISS_MaxSSChannels Then Begin
   ISS_GetVolume:=0; Exit;
  End;
 ISS_GetVolume:=ISS_VirtualChannels^[Channel].VChVolume;
End;

{ * Sets panning on the specified channel - Panning range : 0-255 * }
Function ISS_SetPanning(Channel : Word; Panning : Byte) : Boolean;
Begin
 ISS_SetPanning:=False;
 If Channel>ISS_MaxSSChannels Then Exit;

 With ISS_VirtualChannels^[Channel] Do Begin
   VChPanning:=Panning;
   VChControl:=VChControl Or ISS_CCPanning;
  End;
End;

{ * Gets panning from the specified channel * }
Function ISS_GetPanning(Channel : Word) : DWord;
Begin
 If Channel>ISS_MaxSSChannels Then Begin
   ISS_GetPanning:=0; Exit;
  End;
 ISS_GetPanning:=ISS_VirtualChannels^[Channel].VChPanning;
End;


{ * Sets instrument envelope default values on the specified channel * }
Procedure ISS_InitInstrument(Channel : Word);
Begin
 If Channel>ISS_MaxSSChannels Then Exit;

 With ISS_VirtualChannels^[Channel] Do Begin

   VChFadeOutVolume:=$08000; { * Default FadeOut Volume * }
   VChEnvVolume    :=64;     { * Default Envelope Volume * }
   VChEnvVolPoint  :=0;      { * Default Volume Envelope Positions * }
   VChEnvVolPos    :=0;

   VChEnvPanning   :=32;     { * Default Envelope Panning * }
   VChEnvPanPoint  :=0;      { * Default Panning Envelope Positions * }
   VChEnvPanPos    :=0;

   VChAVibPitch    :=0;      { * Default Autovibrato Pitch * }
   VChAVibPos      :=0;      { * Default Autovibrato Position * }
   VChAVibSwpPos   :=0;      { * Default Autovibrato Sweep Position * }

   VChControl:=VChControl And Not ISS_CCVolFadeOut; { *  Disable FadeOut * }
   VChControl:=VChControl Or ISS_CCVolume; { * Volume Change * }

  End;
End;

{ * Starts a specified Instrument on the specified channel * }
Function ISS_StartInstrument(Channel : Word;
                             Instr   : Word;
                             Note    : Word) : Boolean;
Var SampleToStart : ISS_PSample;
    InstrToStart  : ISS_PInstrument;
    BufPeriod     : DWord;
Begin
 ISS_StartInstrument:=False;
 If ISS_DevicesOK And ISS_SysStarted Then Begin

   With ISS_CurrentModule^ Do Begin

     { * Values Range Checking * }
     If Channel>ISS_MaxSSChannels Then Exit; { * Channel number valid? * }
     If Instr>MInstrNum Then Exit; { * Exit if Instrument Number invalid * }
     If Note>95 Then Exit; { * Exit if Note is invalid * }

     { * Device Control * }
     With ISS_VirtualChannels^[Channel] Do Begin

       { * Specifying a new note will always stop the current. * }
       VChControl:=VChControl Or ISS_CCStop;

       { * Selecting Sample to be started * }
       SampleToStart:=Nil;
       InstrToStart:=MInstruments[Instr];
       With InstrToStart^ Do Begin
         If ISampleNum=0 Then Exit;
         SampleToStart:=ISamples[INoteTable[Note]];
        End;

       If SampleToStart=Nil Then Exit; { * Exit if sample is invalid * }

       { * Checking if sample size is valid * }
       With SampleToStart^ Do Begin
         If SLength=0 Then Exit;
        End;

       VChSmpAddr:=SampleToStart; { * Set sample address * }
       VChInsAddr:=InstrToStart;  { * Set instrument address * }
       VChSmpOffs:=0;             { * Set sample offset to zero * }

       { * Period Setup * }
       BufPeriod:=ISS_SmpGetNotePeriod(SampleToStart,Note);
       If BufPeriod<>0 Then ISS_SetPeriod(Channel,BufPeriod)
                       Else Exit;

       { * Setting Channel Control (See Flags in ISS_VAR.PAS) * }
       VChControl:=VChControl Or (ISS_CCSample+ISS_CCPeriod+
                                  ISS_CCVolume+ISS_CCActive+ISS_CCPanning);

      End;

    End;

   ISS_StartInstrument:=True;

  End;
End;

{ * Update Instruments in every tick * }
Procedure ISS_UpdateInstruments;
Var Counter     : DWord;
    DPosition   : LongInt; { * Delta EnvPosition * }
    DVolume     : LongInt; { * Delta EnvVolume * }
    DPanning    : LongInt; { * Delta Panning * }
    BufVibValue : Byte;
Begin
 For Counter:=0 To ISS_ActiveSSChannels-1 Do Begin
   With ISS_VirtualChannels^[Counter] Do Begin

     { * Check if instrument is active on the current channel * }
     If (VChInsAddr<>Nil) And ((VChControl And ISS_CCActive)>0) Then Begin
       With VChInsAddr^ Do Begin

         { * Do FadeOut * }
         If (VChControl And ISS_CCVolFadeOut)>0 Then Begin
           { * Fadeout reached 0? * }
           If IVolFadeOut<VChFadeOutVolume Then Begin
             { * No, still fade out volume * }
             Dec(VChFadeOutVolume,IVolFadeOut);
             VChControl:=VChControl Or ISS_CCVolume;
            End Else Begin
             { * Yes, stop voice. * }
             VChFadeOutVolume:=0;
             VChControl:=VChControl Or ISS_CCStop;
            End;
           End;

         { * Do Volume Envelope * }
         With IVolumeEnv Do Begin
           If (EnvType And ISS_EnvEnabled)>0 Then Begin

             { * Processing Envelope Loops * }
             If VChEnvVolPos=0 Then Begin

               { * Envelope Looped? * }
               If (EnvType And ISS_EnvLoop)>0 Then Begin
                 { * Do envelope loop, if no sustain or fadeout in progess * }
                 If (VChEnvVolPoint<>EnvSustain) Or
                    ((EnvType And ISS_EnvSustain)=0) Or
                    ((VChControl And ISS_CCVolFadeOut)>0) Then Begin

                   { * Loop, if envelope reached loop endpoint, and the * }
                   { * endpoint is not the sustain point. * }
                   If (VChEnvVolPoint=EnvLoopEnd) And
                      (VChEnvVolPoint<>EnvSustain) Then Begin
                     VChEnvVolPoint:=EnvLoopStart;
                    End;

                  End;
                End;

              End;

             { * Envelope Reached Last Point? * }
             If VChEnvVolPoint+1<>EnvPointsNum Then Begin

               { * Delta Initial Values * }
               With EnvPoints[VChEnvVolPoint+1] Do Begin
                 DPosition:=EPPosition;
                 DVolume:=EPValue;
                End;

               With EnvPoints[VChEnvVolPoint] Do Begin
                 { * Calculating Delta Values * }
                 DPosition:=Abs(DPosition-EPPosition);
                 DVolume:=DVolume-EPValue;

                 { * Calculating Final Volume * }
                 VChEnvVolume:=EPValue+((DVolume*VChEnvVolPos) Div DPosition);
                 VChControl:=VChControl Or ISS_CCVolume;
                End;

               { * Always increase envelope position if fadeout active, * }
               { * sustain disabled or envelope position is between points * }
               If ((EnvType And ISS_EnvSustain)=0) Or
                  ((VChControl And ISS_CCVolFadeOut)>0) Or
                  (VChEnvVolPos<>0) Or
                  (VChEnvVolPoint<>EnvSustain) Then Begin

                 Inc(VChEnvVolPos); { * Increase envelope * }
                 { * Position reached next point? * }
                 If VChEnvVolPos>=DPosition Then Begin
                   VChEnvVolPos:=0;
                   Inc(VChEnvVolPoint);
                  End;

                End;

              End Else Begin
               { * Envelope reached end * }
               VChEnvVolume:=EnvPoints[VChEnvVolPoint].EPValue;
               VChControl:=VChControl Or ISS_CCVolume;
              End;

            End;

          End;

         { * Do Panning Envelope * }
         With IPanningEnv Do Begin
           If (EnvType And ISS_EnvEnabled)>0 Then Begin

             { * Processing Envelope Loops * }
             If VChEnvPanPos=0 Then Begin

               { * Envelope Looped? * }
               If (EnvType And ISS_EnvLoop)>0 Then Begin
                 { * Do envelope loop, if no sustain in progess * }
                 If (VChEnvPanPoint<>EnvSustain) Or
                    ((EnvType And ISS_EnvSustain)=0) Then Begin

                   { * Loop, if envelope reached loop endpoint * }
                   If (VChEnvPanPoint=EnvLoopEnd) And
                      (VChEnvPanPoint<>EnvSustain) Then Begin
                     VChEnvPanPoint:=EnvLoopStart;
                    End;

                  End;
                End;

              End;

             { * Envelope Reached Last Point? * }
             If VChEnvPanPoint+1<>EnvPointsNum Then Begin

               { * Delta Initial Values * }
               With EnvPoints[VChEnvPanPoint+1] Do Begin
                 DPosition:=EPPosition;
                 DPanning:=EPValue;
                End;

               With EnvPoints[VChEnvPanPoint] Do Begin
                 { * Calculating Delta Values * }
                 DPosition:=Abs(DPosition-EPPosition);
                 DPanning:=DPanning-EPValue;

                 { * Calculating Final Panning * }
                 VChEnvPanning:=EPValue+((DPanning*VChEnvPanPos) Div DPosition);
                 VChControl:=VChControl Or ISS_CCPanning;
                End;

               { * Always increase envelope position, if sustain disabled * }
               { * or envelope position is between points * }
               If ((EnvType And ISS_EnvSustain)=0) Or
                  (VChEnvPanPos<>0) Or
                  (VChEnvPanPoint<>EnvSustain) Then Begin

                 Inc(VChEnvPanPos); { * Increase envelope * }
                 { * Position reached next point? * }
                 If VChEnvPanPos>=DPosition Then Begin
                   VChEnvPanPos:=0;
                   Inc(VChEnvPanPoint);
                  End;

                End;

              End Else Begin
               { * Envelope reached end * }
               VChEnvPanning:=EnvPoints[VChEnvPanPoint].EPValue;
               VChControl:=VChControl Or ISS_CCPanning;
              End;

            End;
          End;

         { * Do Auto Vibrato * }
         If IVibDepth>0 Then Begin

           BufVibValue:=(ISS_SineTable[VChAVibPos And 127]*IVibDepth) Div 256;
           If VChAVibPos>127 Then VChAVibPitch:=BufVibValue
                             Else VChAVibPitch:=-BufVibValue;
           Inc(VChAVibPos,IVibRate);

           { * Calculating Vibrato Sweep * }
           If VChAVibSwpPos<IVibSweep Then Begin
             VChAVibPitch:=(VChAVibPitch*VChAVibSwpPos) Div IVibSweep;
             Inc(VChAVibSwpPos,1);
            End;

           { * Enabling Frequency Control * }
           VChControl:=VChControl Or ISS_CCPeriod;

          End;

        End;
      End;

    End;
  End;
End;

{ * Instrument Key Off * }
Procedure ISS_KeyOff(Channel : Word);
Begin
 If Channel>ISS_MaxSSChannels Then Exit;

 With ISS_VirtualChannels^[Channel] Do Begin

   If VChInsAddr=Nil Then Exit;

   If (VChInsAddr^.IVolumeEnv.EnvType And ISS_EnvEnabled)>0 Then Begin
     VChControl:=VChControl Or ISS_CCVolFadeOut;
    End Else Begin
     VChControl:=VChControl Or ISS_CCStop;
    End;

  End;

End;


Procedure ISS_UpdateOutput; { * Updates the output device * }
Var Counter   : Word;
    BufPeriod : DWord;
    BufVolume : DWord;
Begin
 For Counter:=0 To ISS_ActiveSSChannels-1 Do Begin
   With ISS_VirtualChannels^[Counter] Do Begin

     { * Process Final Frequency Calculations * }
     If (VChControl And ISS_CCPeriod)>0 Then Begin
       BufPeriod:=VChPeriod;
       Inc(BufPeriod,VChAVibPitch); { * Adding AutoVibrato Pitch * }
       ISS_SetFrequency(Counter,BufPeriod);
      End;

     { * Process Final Volume Calculations * }
     If (VChControl And ISS_CCVolume)>0 Then Begin
       BufVolume:=VChVolume; { * Sample Volume (0-64) * }

       { * Global System Volume (0-128) * }
       BufVolume:=BufVolume*ISS_GlobalSSVolume;
       BufVolume:=BufVolume Shr 7;

       { * FadeOut Volume ($0-$8000) * }
       BufVolume:=BufVolume*VChFadeOutVolume;
       BufVolume:=BufVolume Shr 15;

       { * Envelope Volume (0-64) * }
       BufVolume:=BufVolume*VChEnvVolume;
       BufVolume:=BufVolume Shr 6;

       { * Global Player Volume (0-64) * }
       BufVolume:=BufVolume*ISS_GlobalPlVolume;
       BufVolume:=BufVolume Shr 6;

       VChFinalVolume:=BufVolume;
       If VChMute Then VChFinalVolume:=0; { * Force Mute Channel * }

      End;

     { * Process Final Panning Calculations * }
     If (VChControl And ISS_CCPanning)>0 Then Begin
       VChFinalPanning:=VChPanning+((VChEnvPanning-32)*
                        (128-Abs(VChPanning-128)) Div 32);
      End;

    End;
  End;

 ISS_SoundDriver^.UpdateOut; { * Low-level driver call * }
End;


{ * Set sample offset on the specified channel * }
Procedure ISS_SetSampleOffset(Channel : DWord; SampOffset : DWord);
Begin
 If Channel>ISS_MaxSSChannels Then Exit; { * Channel number valid? * }

 With ISS_VirtualChannels^[Channel] Do Begin
    VChSmpOffs:=SampOffset;
    VChControl:=VChControl Or (ISS_CCStop+ISS_CCSample+ISS_CCVolume);
  End;
End;

{ * Device drivers initalization * }
Function ISS_InitDevices : Boolean;
Begin
 ISS_DevicesNum:=0;
 {$IFDEF _ISS_GUS_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_GUSDevInit; { * Init GUS * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_GUSDevice; { * Include GUS * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_SB_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_SBDevInit; { * Init SB * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_SBDevice; { * Include SB * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_NSND_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_NSNDDevInit; { * Init Nosound * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_NSNDDevice; { * Include Nosound * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_GUSNATIVE_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_GUS2DevInit; { * Init OS/2 native GF1/Interwave * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_GUS2Device; { * Include OS/2 GUS * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_DART_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_DARTDevInit; { * Init DART * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_DARTDevice; { * Include DART * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_DSOUND_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_DSNDDevInit; { * Init DirectSound * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_DSNDDevice; { * Include DirectSound * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}
 {$IFDEF _ISS_OSS_INCLUDE_}
  Inc(ISS_DevicesNum);
  ISS_OSSDevInit; { * Init OSS * }
  ISS_SoundDevices[ISS_DevicesNum]:=@ISS_OSSDevice; { * Include OSS * }
  If Not ISS_SoundDevices[ISS_DevicesNum]^.DevAvail Then Dec(ISS_DevicesNum);
 {$ENDIF}

 {$IFDEF _ISS_MAIN_DEBUGMODE_}
  If ISS_DevicesNum<>0 Then
    WriteLn('ISS_INIT: ',ISS_DevicesNum,' Device Driver(s) Initialized')
   Else
    WriteLn('ISS_INIT: WARNING! No Device Drivers Initialized!');
  WriteLn;
 {$ENDIF}

 ISS_InitDevices:=(ISS_DevicesNum>0);
End;

{ * System Initialization * }
Function ISS_InitSystem : Boolean;
Var DevInit : Boolean;
    LdrInit : Boolean;
Begin
 {$IFDEF _ISS_MAIN_DEBUGMODE_}
  {$WARNINGS OFF} ISS_CompileCoder:={$I %CODER%}; {$WARNINGS ON}
  If ISS_CompileCoder='' Then ISS_CompileCoder:='an unknown guy';
  WriteLn('ISS_INIT: Inquisition Sound Server version ',ISS_VersionStr,' [',
           {$I %DATE%},'] for ',ISS_PlatformStr);
  WriteLn('ISS_INIT: Copyright (C) 1998-2001 by Charlie/Inquisition');
  WriteLn('ISS_INIT: Compiled by : ',ISS_CompileCoder,' at ',{$I %TIME%});
  WriteLn('ISS_INIT: FPC Version : ',{$I %FPCVERSION%});
  WriteLn;
  WriteLn('ISS_INIT: Device Drivers Initialization...');
 {$ENDIF}

 DevInit:=ISS_InitDevices;

 {$IFDEF _ISS_MAIN_DEBUGMODE_}
  WriteLn('ISS_INIT: Module Loaders Initialization...');
 {$ENDIF}

 LdrInit:=ISS_InitLoaders;

 {$IFDEF _ISS_MAIN_DEBUGMODE_}
  Write('ISS_INIT: System initialization ');
  If LdrInit And DevInit Then WriteLn('completted.') Else WriteLn('FAILED!');
 {$ENDIF}

 ISS_InitSystem:=(DevInit And LdrInit);
End;

Begin
End.
