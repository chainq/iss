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

Uses ISS_Var; { * Uses the system variables and types * }

Const ISS_XMLoaderVerStr = '1.1.11'; { * Loader Version Str * }
      ISS_XMLoaderVer    = $11B;    { * Loader Version Num * }

Var ISS_XMLoader : ISS_TModuleLoader; { * Loader Declaration * }

Procedure ISS_XMLoaderInit; { * Loader init code * }

Implementation

Const {$IFDEF _ISS_LOAD_CREATELOGFILE_}
       XMDebugLogName = 'xmloader.log'; { * Debug Log Filename * }
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

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoaderDebugInit                                                   ³}
{³                                                                          ³}
{³. Description : Opens the debug file. Call it only from ISS_Load! Unsafe. ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
Procedure ISS_XMLoaderDebugInit;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  Assign(XMDebugLog,XMDebugLogName);
  Rewrite(XMDebugLog);
  WriteLn('ISS_LOAD: XM Loader is creating logfile : ',XMDebugLogName);
  WriteLn(XMDebugLog,#13,#10,' * Inquisition Sound Server version ',
          ISS_VersionStr,' - XM Loader Debug Log File');
  WriteLn(XMDebugLog,' * Created by loader version : ',ISS_XMLoaderVerStr);
  WriteLn(XMDebugLog,' * Code by Charlie/Inquisition',#13,#10);
 {$ENDIF}
End;

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoaderDebugDone                                                   ³}
{³                                                                          ³}
{³. Description : Closes the debug file. Call it only from ISS_Load! Unsafe ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
Procedure ISS_XMLoaderDebugDone;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  Close(XMDebugLog);
 {$ENDIF}
End;

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoaderCheckModule                                                 ³}
{³                                                                          ³}
{³. Description : Checks the possibility that the current module can be     ³}
{³                loaded with this loader. Call it only from ISS_Load!      ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
Function ISS_XMCheckModule : Boolean;
Begin
 XMHeader:=ISS_XMLoader.ModuleMem;
 With XMHeader^ Do Begin
 If (XMID='Extended Module: ') Then ISS_XMCheckModule:=True
                               Else ISS_XMCheckModule:=False;
  End;
End;

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoadHeader                                                        ³}
{³                                                                          ³}
{³. Description : Loads the current module's header.                        ³}
{³                Call it only from ISS_Load!                               ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
Function ISS_XMLoadHeader : Boolean;
Var BufString : String;
    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     Counter   : Word;
    {$ENDIF}
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' * Loading module header...');
 {$ENDIF}
 XMHeader:=ISS_XMLoader.ModuleMem;
 With XMHeader^ Do Begin
   With ISS_XMLoader.ModulePtr^ Do Begin

     BufString:=XMTitle;           { * Assigning the module title * }
     MTitle   :=BufString;
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Module Title          : ',BufString);
     {$ENDIF}
     MTracker :=ISS_TrackerID_FT2; { * Assigning the tracker type * }

     MFlags   :=XMFlags;    { * Assigning the module flags * }
     MChannels:=XMChannels; { * Assigning number of Channels * }
     MPatternNum:=XMPatterns; { * Assigning number of Patterns (max 256) * }

     { * LoadPatterns will create an empty pattern in this case... * }
     If MPatternNum=0 Then MPatternNum:=1;

     MInstrNum:=XMInstr; { * Assigning number Of Instruments (max 128) * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Number Of Channels    : ',MChannels);
      WriteLn(XMDebugLog,'   - Number Of Patterns    : ',MPatternNum);
      WriteLn(XMDebugLog,'   - Number Of Instruments : ',MInstrNum);
     {$ENDIF}

     MTempo   :=XMTempo;    { * Assigning the default tempo * }
     MBPM     :=XMBPM;      { * Assigning the default BPM * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Default Tempo/BPM     : ',MTempo,'/',MBPM);
     {$ENDIF}

     MSongLength:=XMSongLen; { * Assigning number of orders * }
     MRestart   :=XMRestart; { * Assigning the restart position * }
     MOrders    :=XMOrder;   { * Assigning the order table * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(XMDebugLog,'   - Song Length (Orders)  : ',MSongLength);
      WriteLn(XMDebugLog,'   - Song Restart Position : ',MRestart,#13,#10);
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

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoadPatterns                                                      ³}
{³                                                                          ³}
{³. Description : Loads the current module's patterns.                      ³}
{³                Call it only from ISS_Load!                               ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
Function ISS_XMLoadPatterns : Boolean;
Var CurrentOffset        : Pointer;
    CurrentPatternHeader : ISS_PXMPatternHeader;
    Counter : DWord;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' * Loading patterns...');
 {$ENDIF}

 XMHeader:=ISS_XMLoader.ModuleMem;
 With XMHeader^ Do Begin

   DWord(CurrentOffset):=DWord(ISS_XMLoader.ModuleMem)+60+XMHeadSize;

   { * If there is no pattern, we'll create one for the player... * }
   If XMPatterns=0 Then Begin

     With ISS_XMLoader.ModulePtr^.MPatterns[0]^ Do Begin
       PatSize   :=64*XMChannels;
       PatRowsNum:=64;
       GetMem(PatRows,PatSize);
       FillChar(PatRows^,PatSize,#128);
       {$IFDEF _ISS_LOAD_CREATELOGFILE_}
        WriteLn(XMDebugLog,'   - There was no pattern data. Empty pattern created.');
       {$ENDIF}
      End;

    End Else Begin

     For Counter:=0 To XMPatterns-1 Do Begin
       CurrentPatternHeader:=CurrentOffset;
       With CurrentPatternHeader^ Do Begin
         With ISS_XMLoader.ModulePtr^.MPatterns[Counter]^ Do Begin
           PatSize   :=XMPDataSize;
           PatRowsNum:=XMPRowsNum;
           {$IFDEF _ISS_LOAD_CREATELOGFILE_}
            WriteLn(XMDebugLog,'   - Pattern ',Counter,'.');
            WriteLn(XMDebugLog,'     - Packed Pattern Size : ',PatSize,' bytes');
            WriteLn(XMDebugLog,'     - Number of Rows      : ',PatRowsNum);
           {$ENDIF}

           { * Checking for empty pattern * }
           If PatSize=0 Then Begin
             { * Creating Empty Pattern * }
             PatSize:=PatRowsNum*XMChannels;
             GetMem(PatRows,PatSize);
             FillChar(PatRows^,PatSize,#128);
             Inc(DWord(CurrentOffset),XMPHeaderL);
             {$IFDEF _ISS_LOAD_CREATELOGFILE_}
              WriteLn(XMDebugLog,'     - Pattern is empty.');
             {$ENDIF}
            End Else Begin
             { * Allocating Memory for Pattern Data * }
             GetMem(PatRows,PatSize);
             Inc(DWord(CurrentOffset),XMPHeaderL);
             Move(CurrentOffset^,PatRows^,PatSize); { * Moving Pattern Data * }
             Inc(DWord(CurrentOffset),PatSize);
            End;
          End;
        End;
      End;

    End;

   { * Creating another empty pattern, if not all possible patterns stored * }
   { * in the XM file. This will handle "phantom" patterns in the order table * }
   If XMPatterns<255 Then Begin
     With ISS_XMLoader.ModulePtr^ Do Begin
       With MPatterns[MPatternNum]^ Do Begin
         PatSize   :=64*XMChannels;
         PatRowsNum:=64;
         GetMem(PatRows,PatSize);
         FillChar(PatRows^,PatSize,#128);
        End;
      End;
     { * Now scanning order table for "phantom" patterns, and force them * }
     { * to use the empty pattern just created... * }
     With ISS_XMLoader.ModulePtr^ Do Begin
       For Counter:=0 To MSongLength-1 Do Begin
         If MOrders[Counter]>MPatternNum Then MOrders[Counter]:=MPatternNum
        End;
      End;
    End;

  End;

 XMInsOffs:=CurrentOffset;
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,#13,#10);
 {$ENDIF}

 ISS_XMLoadPatterns:=True;
End;

{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³* ISS_XMLoadInstruments                                                   ³}
{³                                                                          ³}
{³. Description : Loads the current module's instruments and samples.       ³}
{³                Call it only from ISS_Load!                               ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
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

    XMLoadedSmp : Word; { * Number of loaded samples (debug) * }

Begin

 XMLoadedSmp:=0;
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(XMDebugLog,' * Loading instruments...');
 {$ENDIF}

 CurrentOffset:=XMInsOffs;
 XMHeader:=ISS_XMLoader.ModuleMem;
 With XMHeader^ Do Begin

   For Counter:=1 To XMInstr Do Begin
     CurrentInstrument:=CurrentOffset;
     With CurrentInstrument^ Do Begin
       With ISS_XMLoader.ModulePtr^.MInstruments[Counter]^ Do Begin
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
               EPPosition:=XMIVolEnvPoints[Counter2*2];
               EPValue   :=XMIVolEnvPoints[Counter2*2+1];
              End;
            End;
          End;
         IVolFadeOut:=XMIVolFadeOut; { * Assigning Volume FadeOut * }

         With IPanningEnv Do Begin { * Assigning panning envelope values * }
           EnvType     :=XMIPanEnvType; { * Envelope Type * }
           EnvPointsNum:=XMIPanEnvPNum;    { * Number Of Envelope Points * }
           EnvSustain  :=XMIPanEnvSustain; { * Envelope Sustain Point * }
           EnvLoopStart:=XMIPanEnvLoopStart; { * Envelope Loop Start Point * }
           EnvLoopEnd  :=XMIPanEnvLoopEnd;   { * Envelope Loop End Point * }
           For Counter2:=0 To 11 Do Begin { * Envelope Points * }
             With EnvPoints[Counter2] Do Begin
               EPPosition:=XMIPanEnvPoints[Counter2*2];
               EPValue   :=XMIPanEnvPoints[Counter2*2+1];
              End;
            End;
          End;

         { * Assign Autovibrato Values * }
         IVibType:=XMIVibratoType;   { * Vibrato Type * }
         IVibSweep:=XMIVibratoSweep; { * Vibrato Sweep * }
         IVibDepth:=XMIVibratoDepth; { * Vibrato Depth * }
         IVibRate:=XMIVibratoRate;   { * Vibrato Rate * }

         ISampleNum:=XMISmpNum; { * Assigning number of samples * }

         {$IFDEF _ISS_LOAD_CREATELOGFILE_}
          WriteLn(XMDebugLog,'   - Instrument ',Counter,'.');
          WriteLn(XMDebugLog,'     - Instrument Size   : ',XMISize,' bytes');
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

         Inc(DWord(CurrentOffset),XMISize);
         If XMISmpNum>0 Then Begin
           For Counter2:=0 To XMISmpNum-1 Do Begin

             { * Allocating Memory for sample header * }
             New(ISamples[Counter2]);
             CurrentSample:=CurrentOffset;
             With CurrentSample^ Do Begin
               With ISamples[Counter2]^ Do Begin

                 { * Assigning Sample Values * }
                 SName    :=XMSName; { * Sample Name * }
                 SLength  :=XMSSize; { * Sample Size * }
                 SDRAMOffs:=0;

                 If XMSLoopLength>0 Then Begin
                   SLoopStart:=XMSLoopStart; { * Sample Loop Start * }
                   SLoopEnd  :=XMSLoopLength+XMSLoopStart; { * Loop End * }
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
           For Counter2:=0 TO XMISmpNum-1 Do Begin
             With ISamples[Counter2]^ Do Begin
               If SLength>0 Then Begin
                 GetMem(SData,SLength); { * Allocating Memory for Sample Data * }
                 Move(CurrentOffset^,SData^,SLength); { * Moving SampleData * }
                 Inc(DWord(CurrentOffset),SLength);
                 Inc(XMLoadedSmp); { * Inc number of loaded samples (debug) * }

                 { * Delta Conversion * }
                 If (SType And ISS_Smp16BitData)>0 Then Begin
                   { * 16bit sampledata * }
                   BufValue2:=0;
                   For Counter3:=0 To (SLength Div 2)-1 Do Begin
                     BufValue1:=PInteger(SData)[Counter3]+BufValue2;
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
  WriteLn(XMDebugLog,'   - Number of samples loaded: ',XMLoadedSmp,#13,#10);
 {$ENDIF}

 ISS_XMLoadInstruments:=True;
End;

{ * This procedure assigns the loader procedures * }
Procedure ISS_XMLoaderInit;
Begin
 FillChar(ISS_XMLoader,SizeOf(ISS_XMLoader),#0);
 With ISS_XMLoader Do Begin
   DebugInit      :=@ISS_XMLoaderDebugInit;
   DebugDone      :=@ISS_XMLoaderDebugDone;
   CheckModule    :=@ISS_XMCheckModule;
   LoadHeader     :=@ISS_XMLoadHeader;
   LoadPatterns   :=@ISS_XMLoadPatterns;
   LoadInstruments:=@ISS_XMLoadInstruments;
  End;
 {$IFDEF _ISS_LOAD_DEBUGMODE_}
  WriteLn('LDR_INIT: FastTracker 2 .XM loader ',ISS_XMLoaderVerStr);
 {$ENDIF}
End;

Begin
End.
