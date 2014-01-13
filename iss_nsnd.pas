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

{ * ISS_NSND.PAS - No Sound Device Driver                                 }
{             OS - GO32V2 only.                                           }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$ASMMODE INTEL}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }

Unit ISS_NSND;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     ISS_Tim  { * Uses the timer services * }
     {$IFDEF _ISS_NSND_SLOWMODE_}
      ,ISS_Mix { * Uses the mixer in slow mode * }
     {$ENDIF}
     ;

Const ISS_NSNDVersionStr = '0.8.5';

      {$IFDEF _ISS_NSND_SLOWMODE_}
       ISS_NSNDName     = 'No Sound (Mixed mode)';
       ISS_NSNDLongDesc = 'No Sound Device Driver (Mixed mode)';
      {$ELSE}
       ISS_NSNDName     = 'No Sound';
       ISS_NSNDLongDesc = 'No Sound Device Driver';
      {$ENDIF}

Var ISS_NSNDDevice : ISS_TSoundDevice; { * No sound Device Structure * }
    ISS_NSNDDriver : ISS_TSoundDriver; { * No sound Device Driver * }

Procedure ISS_NSNDDevInit;

Implementation

{$IFDEF _ISS_NSND_SLOWMODE_}
 Var ISS_NSNDPlayFreq     : DWord;  { * Current playing (not mixing!) freq. * }
     ISS_NSNDMixBufSize   : DWord;   { * Current mixing buffer size * }
{$ENDIF}

Function ISS_NSNDDetect : Boolean;
Begin
 ISS_NSNDDetect:=True;
End;

Function ISS_NSNDInit : Boolean;
Begin
 ISS_NSNDInit:=True;
End;

Function ISS_NSNDDone : Boolean;
Begin
 ISS_NSNDDone:=True;
End;

{$IFNDEF _ISS_NSND_SLOWMODE_}
 Function ISS_NSNDLoadSample(SStruc : ISS_PSample) : Boolean;
 Begin
  ISS_NSNDLoadSample:=True;
 End;
 Function ISS_NSNDFreeSample(SStruc : ISS_PSample) : Boolean;
 Begin
  ISS_NSNDFreeSample:=True;
 End;
{$ENDIF}

Function ISS_NSNDSetVolume(Volume : DWord) : Boolean;
Begin
 ISS_NSNDSetVolume:=True;
End;

Function ISS_NSNDStartOutput(PeriodicCall  : Pointer) : Boolean;
Begin
 ISS_NSNDStartOutput:=False;

 {$IFDEF _ISS_NSND_SLOWMODE_}

  ISS_NSNDPlayFreq:=5000;
  ISS_NSNDMixBufSize:=ISS_NSNDPlayFreq Div 50;
  ISS_TimerDiff:=ISS_TimerSpeed Div 50;

  ISS_MixerInit(ISS_NSNDPlayFreq,ISS_NSNDMixBufSize,0,0,ISS_NSNDDevice.DevType);
  With ISS_MixerData^ Do Begin
    MixBufOffs:=DWord(MixBufPtr);
   End;

  If ISS_StartTimer(PeriodicCall,ISS_TimerSpeed Div 50) Then Begin
    ISS_NSNDStartOutput:=True;
    {$IFDEF _ISS_NSND_DEBUGMODE_}
     WriteLn('DEV_INIT: Starting ',ISS_NSNDName,' output...');
    {$ENDIF}
   End Else Begin
    {$IFDEF _ISS_NSND_DEBUGMODE_}
     WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_NSNDName,' output!');
     ISS_MixerDone;
    {$ENDIF}
   End;

 {$ELSE}

  If ISS_StartTimer(PeriodicCall,(ISS_TimerSpeed Div 140)) Then Begin
    ISS_NSNDStartOutput:=True;
    ISS_TimerDiff:=ISS_TimerSpeed Div 140;
    {$IFDEF _ISS_NSND_DEBUGMODE_}
     WriteLn('DEV_INIT: Starting ',ISS_NSNDName,' output...');
    {$ENDIF}
   End Else Begin
    {$IFDEF _ISS_NSND_DEBUGMODE_}
     WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_NSNDName,' output!');
    {$ENDIF}
   End;

 {$ENDIF}

End;

Function ISS_NSNDStopOutput(PeriodicCall : Pointer) : Boolean;
Begin
 ISS_NSNDStopOutput:=ISS_StopTimer(PeriodicCall);
 {$IFDEF _ISS_NSND_SLOWMODE_}
  ISS_MixerDone;
 {$ENDIF}
 {$IFDEF _ISS_NSND_DEBUGMODE_}
   WriteLn('DEV_INIT: ',ISS_NSNDName,' output stopped.');
 {$ENDIF}
End;

{$IFNDEF _ISS_NSND_SLOWMODE_}
 Procedure ISS_NSNDUpdateOutput_Fast; { * Updates the sound output in fast mode* }
 Var ChannelCounter : Word;
 Begin
  { * We tell the player, that we did everything it requested... :) * }
  For ChannelCounter:=0 To ISS_ActiveSSChannels-1 Do Begin
    With ISS_VirtualChannels^[ChannelCounter] Do Begin

      { * Stop a Channel ? * }
      If (VChControl And ISS_CCStop)>0 Then Begin
        Dec(VChControl,ISS_CCStop);
       End;

      { * Start a Sample ? * }
      If (VChControl And ISS_CCSample)>0 Then Begin
        Dec(VChControl,ISS_CCSample);
       End;

      { * Change Period ? * }
      If (VChControl And ISS_CCPeriod)>0 Then Begin
        Dec(VChControl,ISS_CCPeriod);
       End;

      { * Change Volume ? * }
      If (VChControl And ISS_CCVolume)>0 Then Begin
        Dec(VChControl,ISS_CCVolume);
       End;

      { * Change Panning ? * }
      If (VChControl And ISS_CCPanning)>0 Then Begin
        Dec(VChControl,ISS_CCPanning);
       End;

      { * Still channel Active ? * }
      If (VChControl And ISS_CCActive)>0 Then Begin
        Dec(VChControl,ISS_CCActive);
       End;

    End;
   End;

 End;
{$ENDIF}

{ * This procedure assigns the device driver procedures * }
Procedure ISS_NSNDDevInit;
Begin
 With ISS_NSNDDriver Do Begin
   Detect    :=@ISS_NSNDDetect;
   Init      :=@ISS_NSNDInit;
   Done      :=@ISS_NSNDDone;
   SetVolume :=@ISS_NSNDSetVolume;
   StartOut  :=@ISS_NSNDStartOutput;
   StopOut   :=@ISS_NSNDStopOutput;
   {$IFDEF _ISS_NSND_SLOWMODE_}
    LoadSample:=@ISS_MixerLoadSample;
    FreeSample:=@ISS_MixerFreeSample;
    UpdateOut :=@ISS_MixerUpdateOutput;
   {$ELSE}
    LoadSample:=@ISS_NSNDLoadSample;
    FreeSample:=@ISS_NSNDFreeSample;
    UpdateOut :=@ISS_NSNDUpdateOutput_Fast;
   {$ENDIF}
  End;
 ISS_NSNDDevice.DevDriver:=@ISS_NSNDDriver;

 {$IFDEF _ISS_NSND_DEBUGMODE_}
  WriteLn('DEV_INIT: Device - ',ISS_NSNDLongDesc,' ',ISS_NSNDVersionStr);
 {$ENDIF}

 { * No sound always available, so we simply assign 'hardware' parameters * }
 With ISS_NSNDDevice Do Begin
   DevAvail   :=True;         { * Device is available (for detection) * }
   DevName    :=ISS_NSNDName; { * Name of the device * }
   DevType    :=ISS_DevMono+ISS_DevSigned+ISS_Dev16Bit+ISS_DevMixed; { * Device Type * }
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
