{
  Copyright (c) 1998-2003,2014  Karoly Balogh <charlie@amigaspirit.hu>

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

{ * ISS_MOD .PAS - Protracker module (MOD) handler (Amiga rules! :)       }
{             OS - Platform Independent                                   }
{            CPU - Endian Independent (using ISS_CPU unit)                }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$IOCHECKS OFF}
Unit ISS_MOD;

Interface

Uses ISS_Var, { * Uses the system variables and types * }
     ISS_CPU; { * Uses endian-specific functions * }

Const ISS_MODHandlerVerStr = '0.6.1'; { * Handler Version Str * }
      ISS_MODHandlerVer    = $061;    { * Handler Version Num * }

Var ISS_MODHandler : ISS_TModuleHandler; { * Handler Declaration * }

Procedure ISS_MODHandlerInit; { * Handler init code * }

Implementation

{$IFDEF _ISS_LOAD_CREATELOGFILE_}
Const MODDebugLogName = 'modload.log'; { * Debug Log Filename * }
{$ENDIF}

Const MODFileIDsNum = 46;
      MODFileIDs : Array[1..MODFileIDsNum,0..3] Of Char =
                 ('M.K.','M!K!','FLT4','N.T.', { * Traditional 4 channel mods * }
                  'CD81','TDZ1','TDZ2','TDZ3', { * Some custom IDs * }
                  'TDZ4','TDZ5','TDZ6','TDZ7',
                  'TDZ8','TDZ9',
                  '1CHN','2CHN','3CHN','4CHN', { * Standard multichannel mods * }
                  '5CHN','6CHN','7CHN','8CHN',
                  '9CHN','10CH','11CH','12CH',
                  '13CH','14CH','15CH','16CH',
                  '17CH','18CH','19CH','20CH',
                  '21CH','22CH','23CH','24CH',
                  '25CH','26CH','27CH','28CH',
                  '29CH','30CH','31CH','32CH');

      MODNoteTable : Array[0..84] Of Word = (
                  $0CFF, $0C44, $0B94, $0AED, $0A50, $09BC, $0930,
                  $08AC, $0830, $07BA, $074B, $06E2, $067F, $0622,
                  $05CA, $0577, $0528, $04DE, $0498, $0456, $0418,
                  $03DD, $03A5, $0371, $0340, $0311, $02E5, $02BB,
                  $0294, $026F, $024C, $022B, $020C, $01EE, $01D3,
                  $01B9, $01A0, $0188, $0172, $015E, $014A, $0138,
                  $0126, $0116, $0106, $00F7, $00E9, $00DC, $00D0,
                  $00C4, $00B9, $00AF, $00A5, $009C, $0093, $008B,
                  $0083, $007C, $0075, $006E, $0068, $0062, $005D,
                  $0057, $0053, $004E, $004A, $0045, $0041, $003E,
                  $003A, $0037, $0034, $0031, $002E, $002C, $0029,
                  $0027, $0025, $0023, $0021, $001F, $001D, $001C,
                  0);

Type { * MOD File Format Header * }
     ISS_TMODHeader = Packed Record
       MODLength : Byte; { * Order list length * }
       MODRepeat : Byte; { * Repeat position it Noisetracker modules, otherwise 127. * }
       MODOrders : Array[0..127] Of Byte; { * Order list * }
       MODID     : Array[0..3] Of Char; { * Module ID * }
      End;
     ISS_PMODHeader = ^ISS_TMODHeader;

     { * MOD Sample Header * }
     ISS_TMODSample = Packed Record
       MODSmpName       : Array[0..21] Of Char; { * Sample name * }
       MODSmpLength     : Word; { * Sample length. Multiply by 2 to get the real size * }
       MODSmpFineTune   : Byte; { * Sample finetune value. (signed) Only the lower 4 bits used. * }
       MODSmpVolume     : Byte; { * Sample volume (0-64, unsigned) * }
       MODSmpLoopStart  : Word; { * Loop start. Multiply by two, to get the real value * }
       MODSmpLoopLength : Word; { * Loop end. Multiply by two, to get the real value * }
      End;
     ISS_PMODSample = ^ISS_TMODSample;

Var MODHeader   : ISS_PMODHeader;
    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     MODDebugLog : Text;
    {$ENDIF}

    MODChNum  : Byte; { * Number of channels in the current module * }
    MODSmpNum : Byte; { * Number of samples in the current module * }

    MODSmpOffs : Pointer; { * Pointer to sampledata offset * }

{/--------------------------------------------------------------------------\}
{|* ISS_MODHandlerDebugInit                                                 |}
{|                                                                          |}
{|. Description : Opens the debug file. Call it only from ISS_Load! Unsafe. |}
{\--------------------------------------------------------------------------/}
Procedure ISS_MODHandlerDebugInit;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  Assign(MODDebugLog,MODDebugLogName);
  Rewrite(MODDebugLog);
  WriteLn('ISS_HNDL: MOD Handler is creating logfile : ',MODDebugLogName);
  WriteLn(MODDebugLog);
  WriteLn(MODDebugLog,' * Inquisition Sound Server version ',
          ISS_VersionStr,' - MOD Handler Debug Log File');
  WriteLn(MODDebugLog,' * Created by handler version ',ISS_MODHandlerVerStr);
  WriteLn(MODDebugLog,' * Copyright (C) 1998-2003 by Charlie/Inquisition');
  WriteLn(MODDebugLog);
 {$ENDIF}
End;

{/--------------------------------------------------------------------------\}
{|* ISS_MODHandlerDebugDone                                                 |}
{|                                                                          |}
{|. Description : Closes the debug file. Call it only from ISS_Load! Unsafe |}
{\--------------------------------------------------------------------------/}
Procedure ISS_MODHandlerDebugDone;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog);
  WriteLn(MODDebugLog,' * END OF FILE.');
  WriteLn(MODDebugLog);
  Close(MODDebugLog);
 {$ENDIF}
End;

{/--------------------------------------------------------------------------\}
{|* ISS_MODCheckModule                                                      |}
{|                                                                          |}
{|. Description : Checks if it's possible to load the current module with   |}
{|                this handler. Call it only from ISS_Load!                 |}
{\--------------------------------------------------------------------------/}
Function ISS_MODCheckModule : Boolean;
Var IDCounter : DWord;
Begin
 ISS_MODCheckModule:=False;

 DWord(MODHeader):=DWord(ISS_MODHandler.ModuleMem)+950;

 { * Searching for the correct module ID * }
 With MODHeader^ Do Begin
   IDCounter:=1;
   While (IDCounter<=MODFileIDsNum) And (MODID<>MODFileIDs[IDCounter]) Do Begin
     Inc(IDCounter);
    End;
  End;

 { * If module not supported, we exit * }
 If (IDCounter>MODFileIDsNum) Then Exit;

 { * Setting channel and samplenum variables, according to the module type * }
 MODSmpNum:=31; { * Default samplenumber for most of the MOD formats * }
 Case IDCounter Of
   1..3       : MODChNum:=4; { * Standard 4ch modules * }
   4          : Begin MODChNum:=4; MODSmpNum:=15; End; { * Noisetracker modules * }
   5          : MODChNum:=8;
   6..14      : MODChNum:=IDCounter-5;
   15..46     : MODChNum:=IDCounter-14; { * Standard multichannel modules * }
   Else Exit; { * If module not supported, we exit * }
  End;

 ISS_MODCheckModule:=True;
End;

{/--------------------------------------------------------------------------\}
{|* ISS_MODLoadHeader                                                       |}
{|                                                                          |}
{|. Description : Loads the current module's header.                        |}
{|                Call it only from ISS_Load!                               |}
{\--------------------------------------------------------------------------/}
Function ISS_MODLoadHeader : Boolean;
Type TMODTitle = Array[0..19] Of Char;
     PMODTitle = ^TMODTitle;
Var BufString  : String;
    Counter    : Word;
    BufPatNum  : Byte;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog,' * Loading module header...');
 {$ENDIF}
 MODHeader:=ISS_MODHandler.ModuleMem;
 BufString:=PMODTitle(MODHeader)^; { * Assigning the module title * }

 DWord(MODHeader):=DWord(MODHeader)+950;
 With MODHeader^ Do Begin
   With ISS_MODHandler.ModulePtr^ Do Begin

     MTitle   :=BufString;
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(MODDebugLog,'   - Module Title          : ',BufString);
     {$ENDIF}
     MTracker :=ISS_TrackerID_PRO; { * Assigning the tracker type * }

     MFlags   :=0;    { * Assigning the module flags * }
     MChannels:=MODChNum; { * Assigning number of Channels * }
     { * Assigning number of Patterns (max 256) * }
     BufPatNum:=0;
     For Counter:=0 To 127 Do Begin
       If MODOrders[Counter]>BufPatNum Then BufPatNum:=MODOrders[Counter];
      End;
     MPatternNum:=BufPatNum;

     MInstrNum:=MODSmpNum; { * Assigning number Of Instruments (max 128) * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(MODDebugLog,'   - Number Of Channels    : ',MChannels);
      WriteLn(MODDebugLog,'   - Number Of Patterns    : ',MPatternNum+1);
      WriteLn(MODDebugLog,'   - Number Of Instruments : ',MInstrNum);
     {$ENDIF}

     MTempo   :=6;   { * Assigning the default tempo * }
     MBPM     :=125; { * Assigning the default BPM * }
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(MODDebugLog,'   - Default Tempo/BPM     : ',MTempo,'/',MBPM);
     {$ENDIF}

     MSongLength:=MODLength;  { * Assigning number of orders * }
     { * Assigning the restart position * }
     If MODRepeat-1>MODLength Then
       MRestart:=MODLength-1
      Else
       MRestart:=MODRepeat-1;
     { * Assigning the order table * }
     For Counter:=0 To MSongLength-1 Do Begin
       MOrders[Counter]:=MODOrders[Counter];
      End;
     {$IFDEF _ISS_LOAD_CREATELOGFILE_}
      WriteLn(MODDebugLog,'   - Song Length (Orders)  : ',MSongLength);
      WriteLn(MODDebugLog,'   - Song Restart Position : ',MRestart); 
      WriteLn(MODDebugLog);
      Write(MODDebugLog,'   - Order Table : ');
      For Counter:=0 To MSongLength-1 Do Begin
        If Counter<MSongLength-1 Then Write(MODDebugLog,MOrders[Counter],',')
                                 Else WriteLn(MODDebugLog,MOrders[Counter]);
       End;
     {$ENDIF}

    End;
  End;

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog,' ');
 {$ENDIF}

 ISS_MODLoadHeader:=True;
End;

{/--------------------------------------------------------------------------\}
{|* ISS_MODLoadPatterns                                                     |}
{|                                                                          |}
{|. Description : Loads the current module's patterns.                      |}
{|                Call it only from ISS_Load!                               |}
{\--------------------------------------------------------------------------/}
Function ISS_MODLoadPatterns : Boolean;
Type PByte = ^Byte;
Var CurrentOffset : Pointer;
    Counter       : DWord;
    Counter2      : DWord;
    Counter3      : DWord;
    Counter4      : DWord;
    BufPatSize    : DWord;
    BufPeriod     : DWord;
    BufAddr       : PByte;
Begin
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog,' * Loading patterns...');
 {$ENDIF}

 { * Calculating pattern size * }
 BufPatSize:=MODChNum*64*4;

 DWord(MODHeader):=DWord(ISS_MODHandler.ModuleMem)+950;
 With MODHeader^ Do Begin

   DWord(CurrentOffset):=DWord(ISS_MODHandler.ModuleMem)+1084;

   With ISS_MODHandler.ModulePtr^ Do Begin
     For Counter:=0 To MPatternNum Do Begin
       With ISS_MODHandler.ModulePtr^.MPatterns[Counter]^ Do Begin
         PatSize   :=BufPatSize;
         PatRowsNum:=64;
         {$IFDEF _ISS_LOAD_CREATELOGFILE_}
          WriteLn(MODDebugLog,'   - Pattern ',Counter,'.');
          WriteLn(MODDebugLog,'     - Pattern Size   : ',PatSize,' bytes');
          WriteLn(MODDebugLog,'     - Number of Rows : ',PatRowsNum);
         {$ENDIF}

         GetMem(PatRows,PatSize); { * Allocating Memory for Pattern Data * }
         Move(CurrentOffset^,PatRows^,PatSize); { * Moving Pattern Data * }
         Inc(DWord(CurrentOffset),PatSize);

         { * Converting note values in the pattern * }
         BufAddr:=PatRows;
         For Counter2:=0 To 63 Do Begin
           For Counter3:=0 To MODChNum-1 Do Begin
             BufPeriod:=((BufAddr[0] And $0F) Shl 8) + BufAddr[1];
             If BufPeriod>0 Then Begin
               Counter4:=0;
               While (Counter4<84) And (MODNoteTable[Counter4]>BufPeriod) Do Begin
                 Inc(Counter4);
                End;
               BufAddr[1]:=Counter4+13;
              End;
             Inc(DWord(BufAddr),4);
            End;
          End;

        End;
      End;
    End;

  End;

 MODSmpOffs:=CurrentOffset;
 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog);
 {$ENDIF}

 ISS_MODLoadPatterns:=True;
End;

{/--------------------------------------------------------------------------\}
{|* ISS_MODLoadInstruments                                                  |}
{|                                                                          |}
{|. Description : Loads the current module's instruments and samples.       |}
{|                Call it only from ISS_Load!                               |}
{\--------------------------------------------------------------------------/}
Function ISS_MODLoadInstruments : Boolean;
Var CurrentSmpData : Pointer;
    CurrentSmp     : ISS_PMODSample;

    BufString : String;

    BufLoopStart  : Word; { * For endian conversion... * }
    BufLoopLength : Word;

    Counter   : DWord;
    Counter2  : DWord;

    {$IFDEF _ISS_LOAD_CREATELOGFILE_}
     MODLoadedSmp : Word; { * Number of loaded samples * }
    {$ENDIF}
Begin

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  MODLoadedSmp:=0;
  WriteLn(MODDebugLog,' * Loading samples...');
 {$ENDIF}

 DWord(CurrentSmp):=DWord(ISS_MODHandler.ModuleMem)+20;
 CurrentSmpData:=MODSmpOffs;

 MODHeader:=ISS_MODHandler.ModuleMem;
 With MODHeader^ Do Begin

   For Counter:=1 To MODSmpNum Do Begin
     With CurrentSmp^ Do Begin
       With ISS_MODHandler.ModulePtr^.MInstruments[Counter]^ Do Begin
         BufString:=MODSmpName;
         IName:=BufString; { * Setting instrument name * }

         { * Clearing notetable, since it's not used with MODs * }
         For Counter2:=1 To 96 Do INoteTable[Counter2]:=0;

         { * Clearing volume envelope values, since not used... * }
         With IVolumeEnv Do Begin
           EnvType     :=0; { * Envelope Type * }
           EnvPointsNum:=0; { * Number Of Envelope Points * }
           EnvSustain  :=0; { * Envelope Sustain Point * }
           EnvLoopStart:=0; { * Envelope Loop Start Point * }
           EnvLoopEnd  :=0; { * Envelope Loop End Point * }
           For Counter2:=0 To 11 Do Begin { * Clearing Envelope Points * }
             With EnvPoints[Counter2] Do Begin
               EPPosition:=0;
               EPValue   :=0;
              End;
            End;
          End;
         IVolFadeOut:=0; { * Clearing Volume FadeOut * }

         { * Clearing panning envelope values too... * }
         With IPanningEnv Do Begin
           EnvType     :=0; { * Envelope Type * }
           EnvPointsNum:=0; { * Number Of Envelope Points * }
           EnvSustain  :=0; { * Envelope Sustain Point * }
           EnvLoopStart:=0; { * Envelope Loop Start Point * }
           EnvLoopEnd  :=0; { * Envelope Loop End Point * }
           For Counter2:=0 To 11 Do Begin { * Clearing Envelope Points * }
             With EnvPoints[Counter2] Do Begin
               EPPosition:=0;
               EPValue   :=0;
              End;
            End;
          End;

         { * Clear Autovibrato Values * }
         IVibType :=0; { * Vibrato Type * }
         IVibSweep:=0; { * Vibrato Sweep * }
         IVibDepth:=0; { * Vibrato Depth * }
         IVibRate :=0; { * Vibrato Rate * }

         { * Setting number of samples, which is always 1 with MODs * }
         ISampleNum:=1;

         { * Allocating Memory for sample header * }
         New(ISamples[0]);
         With ISamples[0]^ Do Begin

           { * Assigning Sample Values * }
           SName    :=MODSmpName; { * Sample Name * }
           SDRAMOffs:=0;

           { * Let's convert thoose cool bigendian values into some * }
           { * littleendian mess... * }
           {SLength      :=((Lo(MODSmpLength)*$100)+Hi(MODSmpLength))*2;} { * Sample Size * }
           SLength      :=CBEW(MODSmpLength)*2;
           {BufLoopStart :=((Lo(MODSmpLoopStart)*$100)+Hi(MODSmpLoopStart))*2;}
           BufLoopStart :=CBEW(MODSmpLoopStart)*2;
           {BufLoopLength:=((Lo(MODSmpLoopLength)*$100)+Hi(MODSmpLoopLength))*2;}
           BufLoopLength:=CBEW(MODSmpLoopLength)*2;
           If (BufLoopLength>2) Then Begin
             SLoopStart:=BufLoopStart; { * Sample Loop Start * }
             SLoopEnd  :=BufLoopLength+BufLoopStart; { * Loop End * }
            End Else Begin
             SLoopStart:=0;
             SLoopEnd:=0;
            End;

           { * Sample Type Conversion to GUS values * }
           SType:=0;
           If (BufLoopLength>2) Then Begin
             SType:=SType Or ISS_SmpForwardLoop;
            End;

           If MODSmpFineTune>7 Then
             SFineTune:=MODSmpFineTune-16
            Else
             SFineTune:=MODSmpFineTune;

           SVolume   :=MODSmpVolume; { * Sample Volume * }
           SRelNote  :=0;   { * Sample Relative Note * }
           SPanning  :=128;

           {$IFDEF _ISS_LOAD_CREATELOGFILE_}
            WriteLn(MODDebugLog,'  - Sample ',Counter,'.');
            WriteLn(MODDebugLog,'    - Sample Name : ',SName);
            WriteLn(MODDebugLog,'    - Sample Size : ',SLength,' bytes');
            WriteLn(MODDebugLog,'    - Sample Volume     : ',SVolume,'.');
            WriteLn(MODDebugLog,'    - Sample FineTune   : ',SFineTune,'.');
            WriteLn(MODDebugLog,'    - Sample Loop Start : ',SLoopStart,'.');
            WriteLn(MODDebugLog,'    - Sample Loop End   : ',SLoopEnd,'.');
           {$ENDIF}

           If SLength>0 Then Begin
             GetMem(SData,SLength); { * Allocating Memory for Sample Data * }
             Move(CurrentSmpData^,SData^,SLength); { * Moving SampleData * }
             Inc(DWord(CurrentSmpData),SLength);
             {$IFDEF _ISS_LOAD_CREATELOGFILE_}
              Inc(MODLoadedSmp); { * Inc number of loaded samples * }
             {$ENDIF}
             {$IFDEF _ISS_LOAD_CREATELOGFILE_}
              WriteLn(MODDebugLog,'    - Sample ',Counter,
                                  '. data is 8 bits.');
             {$ENDIF}

            End;
          End;
        End;
      End;
     Inc(DWord(CurrentSmp),SizeOf(ISS_TMODSample));
    End;

  End;

 {$IFDEF _ISS_LOAD_CREATELOGFILE_}
  WriteLn(MODDebugLog,'   - Number of samples loaded: ',MODLoadedSmp); 
  WriteLn(MODDebugLog);
 {$ENDIF}

 ISS_MODLoadInstruments:=True;
End;


{ * Decodes the specified MOD pattern. * }
Procedure ISS_MODDecodePattern(EncPat : ISS_PPattern;
                               DecPat : ISS_PDecodedPattern);
Var PatBufPtr : Pointer;
    BufValue  : Array[0..3] Of Byte;
    Counter   : DWord;
    Counter2  : DWord;
Begin
 PatBufPtr:=EncPat^.PatRows;

 For Counter:=1 To EncPat^.PatRowsNum Do Begin
   For Counter2:=0 To ISS_CurrentModule^.MChannels-1 Do Begin
     With DecPat^[Counter,Counter2] Do Begin

       DWord(BufValue):=DWord(PatBufPtr^);
       Inc(DWord(PatBufPtr),4);

       { * Copies 5 bytes to unpacked pattern data * }
       RNote  :=BufValue[1];
       RInstr :=(BufValue[0] And $00F0)+(BufValue[2] Shr 4);
       RVolCol:=0;
       RFXType:=(BufValue[2] And $000F);
       RFXParm:=(BufValue[3]);

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
Procedure ISS_MODHandlerInit;
Begin
 FillChar(ISS_MODHandler,SizeOf(ISS_MODHandler),#0);
 With ISS_MODHandler Do Begin
   DebugInit      :=@ISS_MODHandlerDebugInit;
   DebugDone      :=@ISS_MODHandlerDebugDone;
   CheckModule    :=@ISS_MODCheckModule;
   LoadHeader     :=@ISS_MODLoadHeader;
   LoadPatterns   :=@ISS_MODLoadPatterns;
   LoadInstruments:=@ISS_MODLoadInstruments;
   DecodePattern  :=@ISS_MODDecodePattern;
  End;
 {$IFDEF _ISS_LOAD_DEBUGMODE_}
  WriteLn('HND_INIT: Protracker .MOD handler ',ISS_MODHandlerVerStr);
 {$ENDIF}
End;

Begin
End.
