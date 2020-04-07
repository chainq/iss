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

{ * ISS_DISK.PAS - Diskwriter Device Driver                               }
{             OS - GO32V2 only.                                           }

{ * NOTE: THIS DEVICE DRIVER IS FOR MIXER DEBUGGING ONLY. * }
{ * IT'S A FAST&DIRTY HACK FROM THE SLOW NOSOUND DRIVER. * }
{ * DO _NOT_ USE IT AT ALL. :) * }

{$INCLUDE ISS_SET.INC}
Unit ISS_Disk;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     ISS_Mix, { * Uses the mixer of course * }
     ISS_CPU; { * Uses the CPU unit to be endian-independent * }

Const ISS_DISKVersionStr = '0.2.0';

      ISS_DISKName     = 'Diskwriter Device';
      ISS_DISKLongDesc = 'Diskwriter Device For Debugging';

      ISS_DISKDumpFileName = 'iss_disk.wav';

Var ISS_DISKDevice : ISS_TSoundDevice; { * Diskwriter Device Structure * }
    ISS_DISKDriver : ISS_TSoundDriver; { * Diskwriter Device Driver * }

    ISS_DISKDumpFile     : File;
    ISS_DISKDumpDataSize : DWord;

    ISS_DISKBuffer : Pointer;

Procedure ISS_DISKDevInit;
Procedure ISS_DISKIRQ;

Implementation

Type ISS_TDISKWAVFileHeader = Packed Record
       rID             : Array[0..3] Of Char; { * 'RIFF' * }
       rLen            : LongInt;             { * RIFF file length * }
       wID             : Array[0..3] Of Char; { * 'WAVE' - file type * }
       fID             : Array[0..3] Of Char; { * 'fmt ' - format desc. * }
       fLen            : LongInt;             { * WAVE length * }
       wFormatTag      : Word;
       nChannels       : Word;                { * Channels : 1-mono 2-stereo * }
       nSamplesPerSec  : LongInt;             { * Samples/Sec (Freq) * }
       { * nAvgBytesPerSec:=nSamplesPerSec*nChannels*(nBitsPerSample Div 8) * }
       nAvgBytesPerSec : LongInt;
       { * nBlockAlign:=nChannels*(nBitsPerSample Div 8) * }
       nBlockAlign     : Word;
       nBitsPerSample  : Word;                { * Bits : 8/16/?? * }
       dID             : Array[0..3] Of Char; { * 'data' * }
       dLen            : LongInt;             { * Length of data in byte * }
      End;

Var ISS_DISKTimerFreq      : Single; { * Timer frequency for Slow Nosound. * }
    ISS_DISKPlayFreq       : DWord;  { * Current playing (not mixing!) freq. * }
    ISS_DISKPlayerCallFreq : Single;
    ISS_DISKMixBufSize     : DWord;   { * Current mixing buffer size * }
    ISS_DISKPeriodicCall   : Procedure; { * Pointer to the tracker code * }
    ISS_DISKWAVFileHeader  : ISS_TDISKWAVFileHeader;


Procedure ISS_DISKIRQ;
Begin
 { * Filling up the buffer * }
 ISS_DISKPeriodicCall;

 { * Scratching out all the shit to the disk... * }
{
 With ISS_MixerData^ Do Begin
   BlockWrite(ISS_DISKDumpFile,ISS_DISKBuffer^,MixBufSize*4);
   Inc(ISS_DISKDumpDataSize,MixBufSize*4);
  End;
}
End;

Function ISS_DISKDetect : Boolean;
Begin
 ISS_DISKDetect:=True;
End;

Function ISS_DISKInit : Boolean;
Begin
 ISS_DISKInit:=True;
End;

Function ISS_DISKDone : Boolean;
Begin
 ISS_DISKDone:=True;
End;

Function ISS_DISKSetVolume(Volume : DWord) : Boolean;
Begin
 ISS_DISKSetVolume:=True;
End;

Function ISS_DISKStartOutput(PeriodicCall  : Pointer) : Boolean;
Const ISS_TimerSpeed = 1193180;
Begin

 ISS_DISKStartOutput:=False;
 ISS_DISKPlayFreq:=44100;
 ISS_DISKMixBufSize:=512;
 ISS_DISKTimerFreq:=(ISS_DISKPlayFreq/ISS_DISKMixBufSize);
 ISS_DISKPlayerCallFreq:=ISS_DISKTimerFreq;
 ISS_TimerDiff:=Round(ISS_TimerSpeed/ISS_DISKPlayerCallFreq);

 DWord(ISS_DISKPeriodicCall):=DWord(PeriodicCall);

 GetMem(ISS_DISKBuffer,ISS_DISKMixBufSize*8);
 ISS_MixerInit(ISS_DISKPlayFreq,ISS_DISKMixBufSize,0,DWord(ISS_DISKBuffer),ISS_DISKDevice.DevType);

 {ISS_MixerInit(ISS_DISKPlayFreq,ISS_DISKMixBufSize);}

 Assign(ISS_DISKDumpFile,ISS_DISKDumpFileName);
 Rewrite(ISS_DISKDumpFile,1);

 { * Assigning word values * }
 FillChar(ISS_DISKWAVFileHeader,SizeOf(ISS_DISKWAVFileHeader),#0);
 With ISS_DISKWAVFileHeader Do Begin
   rID            :='RIFF'; { * 'RIFF' * }
   rLen           :=CLEL(SizeOf(ISS_DISKWAVFileHeader)-8); { * RIFF file length * }
   wID            :='WAVE'; { * 'WAVE' - file type * }
   fID            :='fmt '; { * 'fmt ' - format desc. * }
   fLen           :=CLEL($10);    { * WAVE length * }
   wFormatTag     :=CLEW($0001);  { * 1-uncompressed * }
   nChannels      :=CLEW($0002);  { * Channels : 1-mono 2-stereo * }
   nSamplesPerSec :=CLEL(ISS_DISKPlayFreq);  { * Samples/Sec (Freq) * }
   nBitsPerSample :=CLEW(16);     { * Bits : 8/16/?? * }
   { * nAvgBytesPerSec:=nSamplesPerSec*nChannels*(nBitsPerSample Div 8) * }
   nAvgBytesPerSec:=CLEL(nSamplesPerSec*nChannels*(nBitsPerSample Div 8));
   { * nBlockAlign:=nChannels*(nBitsPerSample Div 8) * }
   nBlockAlign    :=CLEW(nChannels*(nBitsPerSample Div 8));
   dID            :='data'; { * 'data' * }
   dLen           :=CLEL(0);      { * Length of data in byte * }
  End;
 BlockWrite(ISS_DISKDumpFile,ISS_DISKWAVFileHeader,
            SizeOf(ISS_DISKWAVFileHeader));
 ISS_DISKDumpDataSize:=0;

 If IOResult=0 Then Begin
   ISS_DISKStartOutput:=True;
   {$IFDEF _ISS_DISK_DEBUGMODE_}
    WriteLn('DEV_INIT: Mixing Frequency:',ISS_DISKPlayFreq,'Hz - Buffer Size:',ISS_DISKMixBufSize,' bytes');
    WriteLn('          Theoretical IRQ Frequency:',ISS_DISKTimerFreq:0:6,'Hz');
    WriteLn('DEV_INIT: Starting ',ISS_DISKName,' output into file: ',ISS_DISKDumpFileName);
   {$ENDIF}
  End Else Begin
   {$IFDEF _ISS_DISK_DEBUGMODE_}
    WriteLn('DEV_FAIL: ERROR! Failed to start ',ISS_DISKName,' output!');
    ISS_MixerDone;
   {$ENDIF}
  End;
End;

Function ISS_DISKStopOutput(PeriodicCall : Pointer) : Boolean;
Begin
 With ISS_DISKWAVFileHeader Do Begin
    rLen:=CLEL(ISS_DISKDumpDataSize+SizeOf(ISS_DISKWAVFileHeader)-8);
    dLen:=CLEL(ISS_DISKDumpDataSize+0);
   End;
 Seek(ISS_DISKDumpFile,0);
 BlockWrite(ISS_DISKDumpFile,ISS_DISKWAVFileHeader,
            SizeOf(ISS_DISKWAVFileHeader));
 Close(ISS_DISKDumpFile);

 ISS_MixerDone;
 FreeMem(ISS_DISKBuffer,ISS_DISKMixBufSize*8);
 ISS_DISKStopOutput:=(IOResult=0);
End;

{ * This procedure assigns the device driver procedures * }
Procedure ISS_DISKDevInit;
Begin
 With ISS_DISKDriver Do Begin
   Detect    :=@ISS_DISKDetect;
   Init      :=@ISS_DISKInit;
   Done      :=@ISS_DISKDone;
   LoadSample:=@ISS_MixerLoadSample;
   FreeSample:=@ISS_MixerFreeSample;
   SetVolume :=@ISS_DISKSetVolume;
   StartOut  :=@ISS_DISKStartOutput;
   StopOut   :=@ISS_DISKStopOutput;
   UpdateOut :=@ISS_MixerUpdateOutput;
  End;
 ISS_DISKDevice.DevDriver:=@ISS_DISKDriver;

 {$IFDEF _ISS_DISK_DEBUGMODE_}
  WriteLn('DEV_INIT: Device - ',ISS_DISKLongDesc,' ',ISS_DISKVersionStr);
 {$ENDIF}

 { * Diskwriter always available, so we simply assign 'hardware' parameters * }
 { * Since it is for debugging only, we doesn't do much file error checking. * }
 With ISS_DISKDevice Do Begin
   DevAvail   :=True;         { * Device is available (for detection) * }
   DevName    :=ISS_DISKName; { * Name of the device * }
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
