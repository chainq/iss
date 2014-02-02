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

{ * ISS_VAR .PAS - Extended module (XM) loader                            }
{             OS - Platform Independent                                   }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$IOCHECKS OFF}
Unit ISS_XM;

Interface

Uses ISS_Var,  { * Uses the system variables and types * }
     ISS_CPU;  { * Uses endian-specific functions * }

Const ISS_XMHandlerVerStr = '1.2.0'; { * Handler Version Str * }
      ISS_XMHandlerVer    = $120;    { * Handler Version Num * }

Var ISS_XMHandler : ISS_TModuleHandler; { * Handler Declaration * }

Procedure ISS_XMHandlerInit; { * Handler init code * }

Implementation

Const {$IFDEF _ISS_LOAD_CREATELOGFILE_}
       XMDebugLogName = 'xmload.log'; { * Debug Log Filename * }
      {$ENDIF}

      { * Sample Type Consts * } { * XM Values * }
      XM_Smp16BitData    = %00010000; { * 16bit SampleData * }
      XM_SmpForwardLoop  = %00000001; { * Forward Looping * }
      XM_SmpPingPongLoop = %00000010; { * Bidirectional Looping * }

Type { * XM File Format Header * }
     ISS_TXMHeader = Packed Record
       XMID    : Array[0..16] Of Char; { * 'Extended Module: ' * }
       XMTitle : Array[0..19] Of Char; { * Module name, padded with zeroes * }
       XM1A    : Char; { * $1A 'End Of File' For DOS 'Type' Command * }

       XMTracker : Array[0..19] Of Char; { * Tracker Name * }
       XMVersion : Word; { * Version Number, Hi byte major, low minor * }

       XMHeadSize : DWord; { * File Header Size * }
       XMSongLen  : Word;  { * Song Length (in pattern order table) * }
       XMRestart  : Word;  { * Song Restart Position * }
       XMChannels : Word;  { * Number Of Channels (2-32) * }
       XMPatterns : Word;  { * Number Of Patterns (max 256) * }
       XMInstr    : Word;  { * Number Of Instruments (max 128) * }
       XMFlags    : Word;  { * Module Flags * }

       XMTempo    : Word; { * Module Tempo * }
       XMBPM      : Word; { * Module BPM * }

       XMOrder    : Array[0..255] Of Byte; { * Order Table * }
      End;
     ISS_PXMHeader = ^ISS_TXMHeader;

     { * XM Pattern Header * }
     ISS_TXMPatternHeader = Packed Record
       XMPHeaderL  : DWord; { * Pattern Header Length * }
       XMPPackType : Byte;  { * Pattern Pack Type (Always = 0) * }
       XMPRowsNum  : Word;  { * Number of Rows in the Pattern * }
       XMPDataSize : Word;  { * Packed PatternData Size * }
      End;
     ISS_PXMPatternHeader = ^ISS_TXMPatternHeader;

     { * XM Instrument Header * }
     ISS_TXMInstrument = Packed Record
       XMISize   : DWord; { * Instrument Size * }
       XMIName   : Array[0..21] Of Char; { * Instrument Name * }
       XMIType   : Byte; { * Instrument Type (Always 0) * }
       XMISmpNum : Word; { * Number of samples in this instrument * }

       XMIHeaderSize : DWord; { * Instrument Header Size * }
       XMINoteTable  : Array[1..96] Of Byte; { * Sample Number for all notes * }

       XMIVolEnvPoints : Array[0..23] Of Word; { * Volume Envelope Points * }
       XMIPanEnvPoints : Array[0..23] Of Word; { * Panning Envelope Points * }

       XMIVolEnvPNum : Byte; { * Volume Envelope Points * }
       XMIPanEnvPNum : Byte; { * Panning Envelope Points * }

       XMIVolEnvSustain   : Byte; { * Volume Sustain Point * }
       XMIVolEnvLoopStart : Byte; { * Volume Loop Start Point * }
       XMIVolEnvLoopEnd   : Byte; { * Volume Loop End Point * }

       XMIPanEnvSustain   : Byte; { * Panning Sustain Point * }
       XMIPanEnvLoopStart : Byte; { * Panning Loop Start Point * }
       XMIPanEnvLoopEnd   : Byte; { * Panning Loop End Point * }

       XMIVolEnvType : Byte; { * Volume Envelope Type * }
       XMIPanEnvType : Byte; { * Panning Envelope Type * }

       XMIVibratoType  : Byte; { * Vibrato Type * }
       XMIVibratoSweep : Byte; { * Vibrato Sweep * }
       XMIVibratoDepth : Byte; { * Vibrato Depth * }
       XMIVibratoRate  : Byte; { * Vibrato Rate * }

       XMIVolFadeOut : Word; { * Volume FadeOut * }
       Reserved      : Word;
      End;
     ISS_PXMInstrument = ^ISS_TXMInstrument;

     { * XM Sample Header * }
     ISS_TXMSample = Packed Record
       XMSSize       : DWord;
       XMSLoopStart  : DWord;
       XMSLoopLength : DWord;
       XMSVolume     : Byte;
       XMSFineTune   : ShortInt;
       XMSType       : Byte;
       XMSPanning    : Byte;
       XMSRelNote    : ShortInt; { * Relative Note Number (signed byte) * }
       XMSReserved   : Byte;
       XMSName       : Array[0..21] Of Char;
      End;
     ISS_PXMSample = ^ISS_TXMSample;

Var XMHeader   : ISS_PXMHeader;
    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     XMDebugLog : Text;
    {$ENDIF}

    XMInsOffs   : Pointer; { * Pointer to current instrument offset * }

{ * ISS_XMHandlerDebugInit                                                }
{                                                                         }
{ . Description: Opens the debug file. Call it only from ISS_Load!        }
{                This call is unsafe. (No error checking.)                }
Procedure ISS_XMHandlerDebugInit;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  Assign(XMDebugLog,XMDebugLogName);
  Rewrite(XMDebugLog);
  WriteLn('ISS_HNDL: XM Handler is creating logfile : ',XMDebugLogName);
  WriteLn(XMDebugLog);
  WriteLn(XMDebugLog,' * Inquisition Sound Server version ',ISS_VersionStr,
                     ' - XM Handler Debug Log File');
  WriteLn(XMDebugLog,' * Created by handler version ',ISS_XMHandlerVerStr);
  WriteLn(XMDebugLog,' * Copyright (C) 1998-2004 by Charlie/Inquisition');
  WriteLn(XMDebugLog);
 {$ENDIF}
End;

{ * ISS_XMHandlerDebugDone                                                }
{                                                                         }
{ . Description: Closes the debug file. Call it only from ISS_Load!       }
{                This call is unsafe. (No error checking.)                }
Procedure ISS_XMHandlerDebugDone;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog);
  WriteLn(XMDebugLog,' * END OF FILE.');
  WriteLn(XMDebugLog);
  Close(XMDebugLog);
 {$ENDIF}
End;

{ * ISS_XMCheckModule                                                     }
{                                                                         }
{ . Description: Checks the possibility that the current module can be    }
{                 loaded with this handler. Call it only from ISS_Load!   }
Function ISS_XMCheckModule : Boolean;
Begin
 XMHeader:=ISS_XMHandler.ModuleMem;
 With XMHeader^ Do Begin
 If (XMID='Extended Module: ') Then ISS_XMCheckModule:=True
                               Else ISS_XMCheckModule:=False;
  End;
End;

{ * ISS_XMLoadHeader                                                      }
{                                                                         }
{ . Description: Loads the current module's header.                       }
{                Call it only from ISS_Load!                              }
Function ISS_XMLoadHeader : Boolean;
Var BufString : String;
    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     Counter   : Word;
    {$ENDIF}
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' * Loading module header...');
 {$ENDIF}
 XMHeader:=ISS_XMHandler.ModuleMem;
 With XMHeader^ Do Begin
   With ISS_XMHandler.ModulePtr^ Do Begin

     BufString:=XMTitle;           { * Assigning the module title * }
     MTitle   :=BufString;
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Module Title          : ',BufString);
     {$ENDIF}
     MTracker :=ISS_TrackerID_FT2; { * Assigning the tracker type * }

     MFlags   :=CLEW(XMFlags);    { * Assigning the module flags * }
     MChannels:=CLEW(XMChannels); { * Assigning number of Channels * }
     MPatternNum:=CLEW(XMPatterns); { * Assigning number of Patterns (max 256) * }

     { * LoadPatterns will create an empty pattern in this case... * }
     If MPatternNum=0 Then MPatternNum:=1;

     MInstrNum:=CLEW(XMInstr); { * Assigning number Of Instruments (max 128) * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Number Of Channels    : ',MChannels);
      WriteLn(XMDebugLog,'   - Number Of Patterns    : ',MPatternNum);
      WriteLn(XMDebugLog,'   - Number Of Instruments : ',MInstrNum);
     {$ENDIF}

     MTempo   :=CLEW(XMTempo);    { * Assigning the default tempo * }
     MBPM     :=CLEW(XMBPM);      { * Assigning the default BPM * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Default Tempo/BPM     : ',MTempo,'/',MBPM);
     {$ENDIF}

     MSongLength:=CLEW(XMSongLen); { * Assigning number of orders * }
     MRestart   :=CLEW(XMRestart); { * Assigning the restart position * }
     MOrders    :=XMOrder;   { * Assigning the order table * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Song Length (Orders)  : ',MSongLength);
      WriteLn(XMDebugLog,'   - Song Restart Position : ',MRestart);
      WriteLn(XMDebugLog);
      Write(XMDebugLog,'   - Order Table : ');
      For Counter:=0 To MSongLength-1 Do Begin
        If Counter<MSongLength-1 Then Write(XMDebugLog,MOrders[Counter],',')
                                 Else WriteLn(XMDebugLog,MOrders[Counter]);
       End;
     {$ENDIF}

    End;
  End;

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' ');
 {$ENDIF}

 ISS_XMLoadHeader:=True;
End;

{ * ISS_XMLoadPatterns                                                    }
{                                                                         }
{ . Description: Loads the current module's patterns.                     }
{                Call it only from ISS_Load!                              }
Function ISS_XMLoadPatterns : Boolean;
Var CurrentOffset        : Pointer;
    CurrentPatternHeader : ISS_PXMPatternHeader;
    Counter : DWord;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' * Loading patterns...');
 {$ENDIF}

 XMHeader:=ISS_XMHandler.ModuleMem;
 With XMHeader^ Do Begin

   DWord(CurrentOffset):=DWord(ISS_XMHandler.ModuleMem)+60+CLEL(XMHeadSize);

   { * If there is no pattern, we'll create one for the player... * }
   If XMPatterns=0 Then Begin

     With ISS_XMHandler.ModulePtr^.MPatterns[0]^ Do Begin
       PatSize   :=64*CLEW(XMChannels);
       PatRowsNum:=64;
       GetMem(PatRows,PatSize);
       FillChar(PatRows^,PatSize,#128);
       {$IFDEF _ISS_LOAD_CREATELOGFILE_}
        WriteLn(XMDebugLog,'   - There was no pattern data. Empty pattern created.');
       {$ENDIF}
      End;

    End Else Begin

     For Counter:=0 To CLEW(XMPatterns)-1 Do Begin
       CurrentPatternHeader:=CurrentOffset;
       With CurrentPatternHeader^ Do Begin
         With ISS_XMHandler.ModulePtr^.MPatterns[Counter]^ Do Begin
           PatSize   :=CLEW(XMPDataSize);
           PatRowsNum:=CLEW(XMPRowsNum);
           {$IFDEF _ISS_LOAD_CREATELOGFILE_}
            WriteLn(XMDebugLog,'   - Pattern ',Counter,'.');
            WriteLn(XMDebugLog,'     - Packed Pattern Size : ',PatSize,' bytes');
            WriteLn(XMDebugLog,'     - Number of Rows      : ',PatRowsNum);
           {$ENDIF}
           { * Checking for empty pattern * }
           If PatSize=0 Then Begin
             { * Creating Empty Pattern * }
             PatSize:=PatRowsNum*CLEW(XMChannels);
             GetMem(PatRows,PatSize);
             FillChar(PatRows^,PatSize,#128);
             Inc(DWord(CurrentOffset),CLEL(XMPHeaderL));
             {$IFDEF _ISS_LOAD_CREATELOGFILE_}
              WriteLn(XMDebugLog,'     - Pattern is empty.');
             {$ENDIF}
            End Else Begin
             { * Allocating Memory for Pattern Data * }
             GetMem(PatRows,PatSize);
             Inc(DWord(CurrentOffset),CLEL(XMPHeaderL));
             Move(CurrentOffset^,PatRows^,PatSize); { * Moving Pattern Data * }
             Inc(DWord(CurrentOffset),PatSize);
            End;

          End;
        End;
      End;

    End;

   { * Creating another empty pattern, if not all possible patterns stored * }
   { * in the XM file. This will handle "phantom" patterns in the order table * }
   If CLEW(XMPatterns)<255 Then Begin
     With ISS_XMHandler.ModulePtr^ Do Begin
       With MPatterns[MPatternNum]^ Do Begin
         PatSize   :=64*CLEW(XMChannels);
         PatRowsNum:=64;
         GetMem(PatRows,PatSize);
         FillChar(PatRows^,PatSize,#128);
        End;
      End;
     { * Now scanning order table for "phantom" patterns, and force them * }
     { * to use the empty pattern just created... * }
     With ISS_XMHandler.ModulePtr^ Do Begin
       For Counter:=0 To MSongLength-1 Do Begin
         If MOrders[Counter]>MPatternNum Then MOrders[Counter]:=MPatternNum
        End;
      End;
    End;

  End;

 XMInsOffs:=CurrentOffset;
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog);
 {$ENDIF}

 ISS_XMLoadPatterns:=True;
End;

{ * ISS_XMLoadInstruments                                                 }
{                                                                         }
{ . Description: Loads the current module's instruments and samples.      }
{                Call it only from ISS_Load!                              }
Function ISS_XMLoadInstruments : Boolean;

Type PInteger  = ^Integer;
     PShortInt = ^ShortInt;

Var CurrentOffset     : Pointer;
    CurrentInstrument : ISS_PXMInstrument;
    CurrentSample     : ISS_PXMSample;

    BufString : String;
    BufValue1 : Integer; { * Used for delta conversion * }
    BufValue2 : Integer; { * Used for delta conversion * }

    Counter   : DWord;
    Counter2  : DWord;
    Counter3  : DWord;

    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     XMLoadedSmp : Word; { * Number of loaded samples (debug) * }
    {$ENDIF}

Begin

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  XMLoadedSmp:=0;
  WriteLn(XMDebugLog,' * Loading instruments...');
 {$ENDIF}

 CurrentOffset:=XMInsOffs;
 XMHeader:=ISS_XMHandler.ModuleMem;
 With XMHeader^ Do Begin

   For Counter:=1 To CLEW(XMInstr) Do Begin
     CurrentInstrument:=CurrentOffset;
     With CurrentInstrument^ Do Begin
       With ISS_XMHandler.ModulePtr^.MInstruments[Counter]^ Do Begin
         BufString:=XMIName;
         IName:=BufString; { * Assigning instrument name * }

         INoteTable:=XMINoteTable; { * Assigning notetable * }

         With IVolumeEnv Do Begin { * Assigning volume envelope values * }
           EnvType     :=XMIVolEnvType; { * Envelope Type * }
           EnvPointsNum:=XMIVolEnvPNum;    { * Number Of Envelope Points * }
           EnvSustain  :=XMIVolEnvSustain; { * Envelope Sustain Point * }
           EnvLoopStart:=XMIVolEnvLoopStart; { * Envelope Loop Start Point * }
           EnvLoopEnd  :=XMIVolEnvLoopEnd;   { * Envelope Loop End Point * }
           For Counter2:=0 To 11 Do Begin { * Envelope Points * }
             With EnvPoints[Counter2] Do Begin
               EPPosition:=CLEW(XMIVolEnvPoints[Counter2*2]);
               EPValue   :=CLEW(XMIVolEnvPoints[Counter2*2+1]);
              End;
            End;
          End;
         IVolFadeOut:=CLEW(XMIVolFadeOut); { * Assigning Volume FadeOut * }

         With IPanningEnv Do Begin { * Assigning panning envelope values * }
           EnvType     :=XMIPanEnvType; { * Envelope Type * }
           EnvPointsNum:=XMIPanEnvPNum;    { * Number Of Envelope Points * }
           EnvSustain  :=XMIPanEnvSustain; { * Envelope Sustain Point * }
           EnvLoopStart:=XMIPanEnvLoopStart; { * Envelope Loop Start Point * }
           EnvLoopEnd  :=XMIPanEnvLoopEnd;   { * Envelope Loop End Point * }
           For Counter2:=0 To 11 Do Begin { * Envelope Points * }
             With EnvPoints[Counter2] Do Begin
               EPPosition:=CLEW(XMIPanEnvPoints[Counter2*2]);
               EPValue   :=CLEW(XMIPanEnvPoints[Counter2*2+1]);
              End;
            End;
          End;

         { * Assign Autovibrato Values * }
         IVibType:=XMIVibratoType;   { * Vibrato Type * }
         IVibSweep:=XMIVibratoSweep; { * Vibrato Sweep * }
         IVibDepth:=XMIVibratoDepth; { * Vibrato Depth * }
         IVibRate:=XMIVibratoRate;   { * Vibrato Rate * }

         ISampleNum:=CLEW(XMISmpNum); { * Assigning number of samples * }

         {$IFDEF _ISS_LOAD_CREATELOGFILE_}
          WriteLn(XMDebugLog,'   - Instrument ',Counter,'.');
          WriteLn(XMDebugLog,'     - Instrument Size   : ',CLEL(XMISize),' bytes');
          WriteLn(XMDebugLog,'     - Instrument Name   : ',IName);
          WriteLn(XMDebugLog,'     - Number Of Samples : ',ISampleNum);
          If ISampleNum>0 Then Begin
            WriteLn(XMDebugLog,'     - Autovibrato Type  : ',IVibType);
            WriteLn(XMDebugLog,'     - Autovibrato Sweep : ',IVibSweep);
            WriteLn(XMDebugLog,'     - Autovibrato Depth : ',IVibDepth);
            WriteLn(XMDebugLog,'     - Autovibrato Rate  : ',IVibRate);
            Write  (XMDebugLog,'     - Note Table : ');
            For Counter2:=1 To 96 Do Begin
              Write(XMDebugLog,INoteTable[Counter2],' ');
             End;
            WriteLn(XMDebugLog);

            Write  (XMDebugLog,'     - Volume Envelope :');
            For Counter2:=0 To 11 Do Begin
              With IVolumeEnv.EnvPoints[Counter2] Do
                Write(XMDebugLog,' Pos:',EPPosition,',Vol:',EPValue);
             End;
            WriteLn(XMDebugLog);
            WriteLn(XMDebugLog,'     - Sustain point :',IVolumeEnv.EnvSustain);
            WriteLn(XMDebugLog,'     - Loop start point :',IVolumeEnv.EnvLoopStart);
            WriteLn(XMDebugLog,'     - Loop end point :',IVolumeEnv.EnvLoopEnd);
           End;
         {$ENDIF}

         Inc(DWord(CurrentOffset),CLEL(XMISize));
         If ISampleNum>0 Then Begin
           For Counter2:=0 To ISampleNum-1 Do Begin

             { * Allocating Memory for sample header * }
             New(ISamples[Counter2]);
             CurrentSample:=CurrentOffset;
             With CurrentSample^ Do Begin
               With ISamples[Counter2]^ Do Begin

                 { * Assigning Sample Values * }
                 SName    :=XMSName; { * Sample Name * }
                 SLength  :=CLEL(XMSSize); { * Sample Size * }
                 SDRAMOffs:=0;

                 If CLEL(XMSLoopLength)>0 Then Begin
                   SLoopStart:=CLEL(XMSLoopStart); { * Sample Loop Start * }
                   SLoopEnd  :=CLEL(XMSLoopLength)+CLEL(XMSLoopStart); { * Loop End * }
                  End Else Begin
                   SLoopStart:=0;
                   SLoopEnd:=0;
                  End;

                 { * Sample Type Conversion to GUS values * }
                 SType:=0;
                 If (XMSType And XM_Smp16BitData)>0 Then
                   SType:=SType Or ISS_Smp16BitData;
                 If (SLoopEnd-SLoopStart)>0 Then Begin
                   If (XMSType And XM_SmpForwardLoop)>0 Then Begin
                     SType:=SType Or ISS_SmpForwardLoop; End;
                   If (XMSType And XM_SmpPingPongLoop)>0 Then Begin
                     SType:=SType Or ISS_SmpPingPongLoop; End;
                  End;

                 SVolume   :=XMSVolume;    { * Sample Volume * }
                 SFineTune :=XMSFineTune;  { * Sample FineTune * }
                 SRelNote  :=XMSRelNote;   { * Sample Relative Note * }
                 SPanning  :=XMSPanning;   { * Sample Panning * }

                 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
                  WriteLn(XMDebugLog,'    - Sample ',Counter2,'.');
                  WriteLn(XMDebugLog,'      - Sample Name : ',SName);
                  WriteLn(XMDebugLog,'      - Sample Size : ',SLength,' bytes');
                  WriteLn(XMDebugLog,'      - Sample Loop Start : ',SLoopStart,'.');
                  WriteLn(XMDebugLog,'      - Sample Loop End   : ',SLoopEnd,'.');
                 {$ENDIF}
                 Inc(DWord(CurrentOffset),SizeOf(ISS_TXMSample));
                End;
              End;
            End;

           { * Loading Sample Data * }
           For Counter2:=0 TO ISampleNum-1 Do Begin
             With ISamples[Counter2]^ Do Begin
               If SLength>0 Then Begin
                 GetMem(SData,SLength); { * Allocating Memory for Sample Data * }
                 Move(CurrentOffset^,SData^,SLength); { * Moving SampleData * }
                 Inc(DWord(CurrentOffset),SLength);
                 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
                  Inc(XMLoadedSmp); { * Inc number of loaded samples * }
                 {$ENDIF}

                 { * Delta Conversion * }
                 If (SType And ISS_Smp16BitData)>0 Then Begin
                   { * 16bit sampledata * }
                   BufValue2:=0;
                   For Counter3:=0 To (SLength Div 2)-1 Do Begin
                     BufValue1:=CLEW(PInteger(SData)[Counter3])+BufValue2;
                     PInteger(SData)[Counter3]:=BufValue1;
                     BufValue2:=BufValue1;
                    End;

                   {$IFDEF _ISS_LOAD_CREATELOGFILE_}
                    WriteLn(XMDebugLog,'    - Sample ',Counter2,
                                       '. data is 16 bits.');
                   {$ENDIF}

                  End Else Begin
                   { * 8bit sampledata * }
                   BufValue2:=0;
                   For Counter3:=0 To SLength-1 Do Begin
                     BufValue1:=PShortInt(SData)[Counter3]+BufValue2;
                     PShortInt(SData)[Counter3]:=BufValue1;
                     BufValue2:=BufValue1;
                    End;
                   SLength:=SLength;

                   {$IFDEF _ISS_LOAD_CREATELOGFILE_}
                    WriteLn(XMDebugLog,'    - Sample ',Counter2,
                                       '. data is 8 bits.');
                   {$ENDIF}
                  End;
                End;
              End;
            End;

          End;
        End;
      End;
    End;

  End;

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,'   - Number of samples loaded: ',XMLoadedSmp);
  WriteLn(XMDebugLog);
 {$ENDIF}

 ISS_XMLoadInstruments:=True;
End;

{ * Decodes the specified XM pattern. Should be optimized, because it's slow. * }
Procedure ISS_XMDecodePattern(EncPat : ISS_PPattern;
                              DecPat : ISS_PDecodedPattern);
Var BufPtr   : Pointer;
    BufValue : Byte;
    Counter  : DWord;
    Counter2 : DWord;
Begin
 BufPtr:=EncPat^.PatRows;

 For Counter:=1 To EncPat^.PatRowsNum Do Begin
   For Counter2:=0 To ISS_CurrentModule^.MChannels-1 Do Begin
     With DecPat^[Counter,Counter2] Do Begin

       BufValue:=Byte(BufPtr^);
       Inc(DWord(BufPtr),1);

       { * Packed Note? * }
       If (BufValue And %10000000)>0 Then Begin
         { * Yes, it's packed unpack note * }

         { * Note Follows? * }
         If (BufValue And %00000001)>0 Then Begin
           RNote:=Byte(BufPtr^);
           Inc(DWord(BufPtr),1);
          End Else Begin
           RNote:=0;
          End;

         { * Instrument Follows? * }
         If (BufValue And %00000010)>0 Then Begin
           RInstr:=Byte(BufPtr^);
           Inc(DWord(BufPtr),1);
          End Else Begin
           RInstr:=0;
          End;

         { * Volume Column Follows? * }
         If (BufValue And %00000100)>0 Then Begin
           RVolCol:=Byte(BufPtr^);
           Inc(DWord(BufPtr),1);
          End Else Begin
           RVolCol:=0;
          End;

         { * Effect Type Follows? * }
         If (BufValue And %00001000)>0 Then Begin
           RFXType:=Byte(BufPtr^);
           Inc(DWord(BufPtr),1);
          End Else Begin
           RFXType:=0;
          End;

         { * Effect Parameter Follows? * }
         If (BufValue And %00010000)>0 Then Begin
           RFXParm:=Byte(BufPtr^);
           Inc(DWord(BufPtr),1);
          End Else Begin
           RFXParm:=0;
          End;

        End Else Begin
         { * No, it's unpacked, just copy values * }

         { * Copies 5 bytes to unpacked pattern data * }
         RNote:=BufValue;
         RInstr:=Byte(BufPtr^);  Inc(DWord(BufPtr),1);
         RVolCol:=Byte(BufPtr^); Inc(DWord(BufPtr),1);
         RFXType:=Byte(BufPtr^); Inc(DWord(BufPtr),1);
         RFXParm:=Byte(BufPtr^); Inc(DWord(BufPtr),1);

        End;

       { * Now convert Exx effect code to 36+ effect code * }
       If RFXType=14 Then Begin
         RFXType:=((RFXParm And $0F0) Shr 4)+36;
         RFXParm:=(RFXParm And $00F);
        End;

      End;
    End;
  End;

End;


{ * This procedure assigns the handler procedures * }
Procedure ISS_XMHandlerInit;
Begin
 FillChar(ISS_XMHandler,SizeOf(ISS_XMHandler),#0);
 With ISS_XMHandler Do Begin
   DebugInit      :=@ISS_XMHandlerDebugInit;
   DebugDone      :=@ISS_XMHandlerDebugDone;
   CheckModule    :=@ISS_XMCheckModule;
   LoadHeader     :=@ISS_XMLoadHeader;
   LoadPatterns   :=@ISS_XMLoadPatterns;
   LoadInstruments:=@ISS_XMLoadInstruments;
   DecodePattern  :=@ISS_XMDecodePattern;
  End;
 {$IFDEF _ISS_LOAD_DEBUGMODE_}
  WriteLn('HND_INIT: FastTracker 2 .XM handler ',ISS_XMHandlerVerStr);
 {$ENDIF}
End;

Begin
End.
