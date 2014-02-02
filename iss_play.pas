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

{ * ISS_PLAY.PAS - XM module player                                       }
{             OS - Platform Independent                                   }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }

Unit ISS_Play;

Interface

Uses ISS_Var, { * Uses system variables and types * }
     ISS_Sys; { * Uses system functions * }


Type ISS_TPlayerChannel = Record
       ChControl : DWord; { * Control Messages (to the devices) * }

       ChNote    : Byte;    { * Channel Note * }
       ChInstr   : Byte;    { * Channel Instrument * }
       ChVolume  : Integer; { * Channel Volume * }
       ChPanning : Byte;    { * Channel Panning * }
       ChFXType  : Byte;    { * Channel Effect * }
       ChVFXType : Byte;    { * Channel Volume Effect * }

       ChPeriod  : Word; { * Channel Period * }

       ChRNote   : Byte; { * Note of the current row * }
       ChRInstr  : Byte; { * Instrument of the current row * }
       ChRVolume : Byte; { * Volume of the current row * }

       { * Effect Variables * }
       ChPortaUpData       : Byte; { * 01 Portamento Up * }
       ChPortaDownData     : Byte; { * 02 Portamento Down * }
       ChPortaToNoteData   : Byte; { * 03 PortaToNote * }
       ChPortaToNotePeriod : Word; { * 03 PortaToNote * }

       ChVibSpeed          : Byte; { * 04 Vibrato * }
       ChVibDepth          : Byte; { * 04 Vibrato * }
       ChVibPosition       : Byte; { * 04 Vibrato * }
       ChVibWaveForm       : Byte; { * 04 Vibrato * }

       ChSampleOffset : DWord; { * 09 Set Sample Offset * }

       ChVolSlideData   : ShortInt; { * 10 Volume Slide * }

       ChGVolSlideData  : ShortInt; { * 17 Global Volume Slide * }

       { * Volume Effect Variables * }
       ChVVolSlideData : Byte; { * VolCol FX - Volume Slide Data * }
       ChVPanSlideData : Word; { * VolCol FX - Panning Slide Data * }

       ChFXTick : Byte; { * All 'Tick based' effects * }

      End;

     ISS_TPlayer = Record
       BPMVal : DWord; { * Module Tempo (1193180*5)/(2*BPM) * }
       BPMCnt : DWord; { * Timer Ticks counter * }
       BPMInc : DWord;
       BPM    : Byte;

       Speed    : Byte;  { * Module Speed  * }
       SpeedCnt : Byte;  { * Tempo counter * }
       TickCnt  : Byte;  { * Tick counter  * }

       Order   : Word; { * Current Order Pos (Can be used for synchro) * }
       Pattern : Word; { * Current Pattern   (Can be used for synchro) * }
       Row     : Word; { * Current Row       (Can be used for synchro) * }
       Rows    : Word; { * Rows in current pattern * }

       CChannel : Word; { * Current Channel number * }
       CNote    : Word; { * Current Note * }
       CInstr   : Word; { * Current Instrument number * }
       CVolume  : Word; { * Current Volume * }
       CVFXType : Word; { * Current Volume command * }
       CVFXParm : Word; { * Current Volume command parameter * }
       CFXType  : Word; { * Current Command * }
       CFXParm  : Word; { * Current Command parameter * }

       PatternOffset : ISS_PPattern; { * Current Pattern Offset * }

       PatternDelay : Byte;
       NextOrder    : Integer;
       NextRow      : Integer;

       Channels : Array[0..ISS_MaxPlChannels-1] Of ISS_TPlayerChannel;

      End;
     ISS_PPlayer = ^ISS_TPlayer;

     { * Unpacked Pattern Structures * }
     ISSPlay_TUnpackedPattern = Array[1..256,0..31] Of ISS_TPatternRow;
     ISSPlay_PUnpackedPattern = ^ISSPlay_TUnpackedPattern;


Var ISS_Player   : ISS_PPlayer;
    ISS_MusicEnd : Boolean; { * True if the current music reached it's end. * }

    ISSPlay_InAction       : Boolean;     { * True if player in work * }
    { * Pointer to current _UNPACKED_ pattern data * }
    ISSPlay_CurrentPatternData : ISSPlay_PUnpackedPattern;

Function  ISS_GetOrder : Word;
Procedure ISS_SetOrder(OrderToSet : Word);

Function ISS_InitModulePlay(Module : ISS_PModule) : Boolean;
Function ISS_StartModulePlay : Boolean;
Function ISS_StopModulePlay : Boolean;
Function ISS_DoneModulePlay : Boolean;

Implementation

{ * Internal variables used by effect processing * }
Var FXToCall : Procedure(FXParam : Word);
    FXToDo   : Procedure;

{ * >>> F O R W A R D  D E C L A R A T I O N S <<< * }

Procedure ISSPlay_DoFX(FXNum : DWord); Forward; { * Used by: ProcTick * }
Procedure ISSPlay_StartNote; Forward; { * Used by: DoNoteDelay * }

{ * >>> E F F E C T  F U N C T I O N S <<< * }
{$INCLUDE ISS_PLFX.PAS}


{ * >>> I N T E R N A L  F U N C T I O N S <<< * }

{ * Starts an instrument on the current channel (ISS_Player^.CChannel) * }
{ * with the channel's current note and instrument. (see the declaration) * }
Procedure ISSPlay_StartInstrument;
Begin
 With ISS_Player^ Do Begin
   With Channels[CChannel] Do Begin

     ISS_StartInstrument(CChannel,ChInstr,ChNote);
     ChPeriod:=ISS_GetPeriod(CChannel);

    End;
  End;
End;

{ * Starts a note on the current channel (ISS_Player^.CChannel) * }
Procedure ISSPlay_StartNote;
Begin
 With ISS_Player^ Do Begin

   With Channels[CChannel] Do Begin
     If CInstr<>0 Then Begin
       ChInstr:=CInstr;
      End;
     ChNote:=CNote;

     Case ChNote Of
       FXKeyOff : Begin ISS_KeyOff(CChannel); End;
       0..96 : Begin
              ISS_InitInstrument(CChannel);
              If (CFXType<>FXPortaNote) And
                 (CVFXType<>FXVolPortaNote) Then Begin
                ISSPlay_StartInstrument;
               End;
             End;

      End;
    End;

  End;
End;

{ * Process a volume effect * }
Procedure ISSPlay_ProcessVolumeFX(FXNum : DWord; FXParam : Word);
Begin
 If FXNum>10 Then Exit;
 FXToCall:=FXVolProcs[FXNum].Proc;
 FXToCall(FXParam);
End;

{ * Process an effect * }
Procedure ISSPlay_ProcessFX(FXNum : DWord; FXParam : Word);
Begin
 If FXNum>51 Then Exit;
 FXToCall:=FXProcs[FXNum].Proc;
 FXToCall(FXParam);
End;

{ * Process Volume Effects * }
Procedure ISSPlay_DoVolumeFX(FXNum : DWord);
Begin
 If FXNum>10 Then Exit;
 FXToDo:=FXVolProcs[FXNum].Sust;
 FXToDo;
End;

{ * Execute an ongoing Effect * }
Procedure ISSPlay_DoFX(FXNum : DWord);
Begin
 If FXNum>51 Then Exit;
 FXToDo:=FXProcs[FXNum].Sust;
 FXToDo;
End;

{ * Execute ongoing Effects * }
Procedure ISSPlay_DoEffects;
Var Counter : DWord;
Begin
 With ISS_Player^ Do Begin
   Inc(TickCnt);

   { * Do volume effects _before_ normal effects * }
   For Counter:=0 To ISS_CurrentModule^.MChannels-1 Do Begin
     CChannel:=Counter;
     ISSPlay_DoVolumeFX(Channels[Counter].ChVFXType);
    End;

   { * Now do normal effects * }
   For Counter:=0 To ISS_CurrentModule^.MChannels-1 Do Begin
     CChannel:=Counter;
     ISSPlay_DoFX(Channels[Counter].ChFXType);
    End;

  End;
End;


{ * Processes the current row, starts note and effects * }
Procedure ISSPlay_ProcessRow;
Var Counter : DWord;
Begin
 With ISS_Player^ Do Begin

   For Counter:=0 To ISS_CurrentModule^.MChannels-1 Do Begin

     CChannel:=Counter; { * Setting current channel for effects * }

     With Channels[Counter] Do Begin
       With ISSPlay_CurrentPatternData^[Row,Counter] Do Begin
         CNote   :=RNote;  { * Reading Note * }
         ChRNote :=RNote;
         CInstr  :=RInstr; { * Reading Instrument * }
         ChRInstr:=RInstr;

         CVolume:=RVolCol; { * Reading Volume * }
         CFXType:=RFXType; { * Reading Effect Type * }
         CFXParm:=RFXParm; { * Reading Effect Parameter * }
        End;
      End;

     { * This is needed for arpeggio processing, because arpeggio * }
     { * effect code is zero, but it always has a parameter which * }
     { * differs from zero. * }
     If (CFXType=0) And (CFXParm=0) Then CFXType:=FXNoEffect;

     { * If actual effect isn't the previous effect, restore the period * }
     With Channels[Counter] Do Begin
       If CFXType<>ChFXType Then Begin
         If ISS_GetPeriod(CChannel)<>ChPeriod Then
           ISS_SetPeriod(CChannel,ChPeriod);
        End;
      End;

     { * Setting Volume Commands * }
     CVFXType:=FXVolNoEffect;
     If CVolume>$50 Then Begin { * Volume>$50 => Volume Command * }
       CVFXType:=((CVolume-$60) Shr 4)+1;
       CVFXParm:=(CVolume-$60) And $0F;
       CVolume :=0;
      End;

     { * Start note if there is no note delay and note<>0 * }
     If (CFXType<>FXNoteDelay) And (CNote<>0) Then ISSPlay_StartNote;

     With Channels[Counter] Do Begin
       { * Process Channel Volume * }
       If CVolume<>0 Then Begin
         { * If CVolume<>0 set the channel volume from volume column * }
         ChVolume:=CVolume-$10;
        End Else Begin
         { * If CVolume=0 and CInst<>0 set sample vol. as channel volume * }
         If CInstr<>0 Then Begin
           With ISS_VirtualChannels^[CChannel] Do Begin
             If VChSmpAddr<>Nil Then ChVolume:=VChSmpAddr^.SVolume;
            End;
          End;
        End;
       ISS_SetVolume(CChannel,ChVolume);

       { * Process Channel Panning * }
       If CInstr<>0 Then Begin
         With ISS_VirtualChannels^[CChannel] Do Begin
           If VChSmpAddr<>Nil Then ChPanning:=VChSmpAddr^.SPanning;
           ISS_SetPanning(CChannel,ChPanning);
          End;
        End;

      End;

     { * Processing Volume Effects * }
     ISSPlay_ProcessVolumeFX(CVFXType,CVFXParm);

     { * Processing Effects * }
     ISSPlay_ProcessFX(CFXType,CFXParm);

     { * Save command values here, so the command proc * }
     { * can check the previous command number         * }
     Channels[Counter].ChFXType :=CFXType;
     Channels[Counter].ChVFXType:=CVFXType;

    End;

  End;
End;

{ * Changes the row to ISS_Player^.NextRow * }
Procedure ISSPlay_DoChangeRow;
Begin
 With ISS_Player^ Do Begin

   If PatternOffset<>Nil Then Begin
     If PatternOffset^.PatRowsNum<NextRow Then Row:=0 Else Row:=NextRow;
    End;

   NextRow:=-1;

  End;
End;

{ * Unpacks the current pattern. Should be optimized, because it's slow. * }
Procedure ISSPlay_UnpackPattern;
Var BufPtr   : Pointer;
    BufValue : Byte;
    Counter  : DWord;
    Counter2 : DWord;
Begin
 With ISS_Player^ Do Begin

   BufPtr:=PatternOffset^.PatRows;

   For Counter:=1 To Rows Do Begin
     For Counter2:=0 To ISS_CurrentModule^.MChannels-1 Do Begin
       With ISSPlay_CurrentPatternData^[Counter,Counter2] Do Begin

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
End;

{ * Changes the order to ISS_Player^.NextOrder * }
Procedure ISSPlay_DoChangeOrder;
Begin
 With ISS_Player^ Do Begin
   With ISS_CurrentModule^ Do Begin

     { * Loop the song if next order > song length * }
     If NextOrder>MSongLength-1 Then Begin
       ISS_MusicEnd:=True;
       Order:=MRestart;
      End Else Order:=NextOrder;

     Pattern:=MOrders[Order];
     PatternOffset:=MPatterns[Pattern];
     If PatternOffset=Nil Then Rows:=64 { * Play a 64 rows empty pattern * }
                          Else Rows:=PatternOffset^.PatRowsNum;

     ISSPlay_UnpackPattern; { * Unpacks new actual pattern * }
     NextOrder:=-1; { * Reset Order Value * }

    End;
  End;
End;

{ * Process a pattern change or a row * }
Procedure ISSPlay_PlayerTick;
Begin
 { * Process Speed * }
 With ISS_Player^ Do Begin
   Inc(SpeedCnt);
   If Speed=SpeedCnt Then Begin
     SpeedCnt:=0; TickCnt:=0;

     { * Process Pattern Delay and Music Order * }
     If PatternDelay>0 Then Begin
       Dec(PatternDelay); ISSPlay_DoEffects;
       Exit; { * Do not process the Row => Delay the pattern * }
      End;

     { * Pattern Break Or Jump Pattern Command Occurs * }
     If NextOrder<>-1 Then ISSPlay_DoChangeOrder
      Else Begin
       If Row>=Rows Then Begin { * If actual Row is pattern length. * }
         NextRow  :=0; { * Jump to the next order (with Row=0) * }
         NextOrder:=Order+1;
         ISSPlay_DoChangeOrder;
        End;
      End;

     If NextRow<>-1 Then ISSPlay_DoChangeRow; { * Change Row Number * }

     Inc(Row); { * Go to next Row * }
     ISSPlay_ProcessRow; { * Process Row * }

    End Else ISSPlay_DoEffects;

  End;
End;

{ * The main timer procedure * }
Procedure ISSPlay_PeriodicProc;
Begin
 With ISS_Player^ Do Begin

   Inc(BPMCnt,ISS_TimerDiff);
   Repeat
    If BPMCnt<BPMVal Then Begin ISS_UpdateOutput; Exit; End;
    Dec(BPMCnt,BPMVal);

    ISSPlay_PlayerTick;    { * Player Main Tick * }
    ISS_UpdateInstruments; { * Updating Instruments * }

   Until False; { * Hacking an Endless Loop :) * }

  End;
End;


{ * >>> P U B L I C  F U N C T I O N S <<< * }

{ * Returns Current Order * }
Function ISS_GetOrder : Word;
Begin
 ISS_GetOrder:=0;
 If Not ISSPlay_InAction Then Exit;
 ISS_GetOrder:=ISS_Player^.Order;
End;

{ * Sets Order * }
Procedure ISS_SetOrder(OrderToSet : Word);
Begin
 If Not ISSPlay_InAction Then Exit;
 With ISS_Player^ Do Begin
   With ISS_CurrentModule^ Do Begin
     If OrderToSet>MSongLength-1 Then OrderToSet:=MSongLength-1;
    End;
   NextRow:=0;
   NextOrder:=OrderToSet;
  End;
End;


Function ISS_InitModulePlay(Module : ISS_PModule) : Boolean;
Var Counter      : DWord;
    Counter2     : DWord;
    {$IFDEF _ISS_PLAY_DEBUGMODE_}
     LoadedSmpNum : Word;
    {$ENDIF}
Begin
 ISS_InitModulePlay:=False;
 If Not ISSPlay_InAction Then Begin

   { * Is module pointer valid? * }
   If (Module=Nil) Or (Module^.MID<>ISS_ModuleID) Then Begin
     { * ERROR CODE! * }
     { * Invalid module pointer specified. * }
     Exit;
    End;

   { * Resetting Player Variables * }
   New(ISS_Player); { * Allocating Variable Memory * }
   FillChar(ISS_Player^,SizeOf(ISS_TPlayer),#0);
   ISS_MusicEnd:=False;

   { * Setting Current Module * }
   ISS_CurrentModule:=Module;

   With ISS_CurrentModule^ Do Begin

     {$IFDEF _ISS_PLAY_DEBUGMODE_}
      WriteLn('ISS_PLAY: Initializing module player...');
     {$ENDIF}

     { * Sets BPM/Tempo values * }
     ProcSetBPM(MBPM);
     With ISS_Player^ Do Begin
       Speed   :=MTempo;
       SpeedCnt:=Speed-1;
      End;

     { * Set the active channels number * }
     If ISS_MaxPlChannels<MChannels Then Begin
       { * ERROR CODE! * }
       { * Too many channels for the player * }
       ISS_InitModulePlay:=False;
       Exit;
      End;
     ISS_SetActiveChannels(MChannels);

     {$IFDEF _ISS_PLAY_DEBUGMODE_}
      WriteLn('ISS_PLAY: Loading samples to the player device...');
      Write  ('ISS_PLAY: ');
      LoadedSmpNum:=0;
     {$ENDIF}
     { * Uploading Samples to the player device * }
     For Counter:=1 To MInstrNum Do Begin
       With MInstruments[Counter]^ Do Begin
         If ISampleNum>0 Then Begin
           For Counter2:=0 To ISampleNum-1 Do Begin
             If ISamples[Counter2]^.SLength>0 Then Begin
               If ISS_LoadSample(ISamples[Counter2]) Then Begin
                 {$IFDEF _ISS_PLAY_DEBUGMODE_}
                  Write('*');
                  Inc(LoadedSmpNum);
                 {$ENDIF}
                End Else Begin
                 {$IFDEF _ISS_PLAY_DEBUGMODE_}
                  Write('!');
                  Inc(LoadedSmpNum);
                 {$ENDIF}
                End;
              End;
            End;
          End;
        End;
      End;
     {$IFDEF _ISS_PLAY_DEBUGMODE_}
      WriteLn(' [',LoadedSmpNum,'] - DONE');
     {$ENDIF}

     { * Allocating Unpacked Pattern Memory * }
     New(ISSPlay_CurrentPatternData);

    End;

  End;
 ISS_InitModulePlay:=True;
End;

Function ISS_StartModulePlay : Boolean;
Begin
 ISS_StartModulePlay:=False;
 If Not ISSPlay_InAction Then Begin

   { * Is module pointer valid? * }
   If (ISS_CurrentModule=Nil) Or
      (ISS_CurrentModule^.MID<>ISS_ModuleID) Then Begin
     { * ERROR CODE! * }
     { * Invalid module pointer specified. * }
     Exit;
    End;

   { * Setting global volume to maximum * }
   ISS_GlobalPlVolume:=64;

   ISSPlay_InAction:=ISS_StartOutput(@ISSPlay_PeriodicProc);
   ISS_StartModulePlay:=ISSPlay_InAction;

   { * Set playing flag on in the active module header * }
   If ISSPlay_InAction Then Begin
     With ISS_CurrentModule^ Do MStatus:=MStatus Or ISS_StPlaying;
    End;

  End;
End;

Function ISS_StopModulePlay : Boolean;
Begin
 ISS_StopModulePlay:=False;
 If ISSPlay_InAction Then Begin
   ISS_StopModulePlay:=ISS_StopOutput;
   ISSPlay_InAction:=False;

   { * Set playing flag off in the active module header * }
   With ISS_CurrentModule^ Do Dec(MStatus,ISS_StPlaying);
  End;
End;

Function ISS_DoneModulePlay : Boolean;
Var Counter, Counter2 : DWord;
    {$IFDEF _ISS_PLAY_DEBUGMODE_}
     FreedSmpNum : Word;
    {$ENDIF}
Begin
 ISS_DoneModulePlay:=False;
 If ISSPlay_Inaction Then Exit;

 With ISS_CurrentModule^ Do Begin
   {$IFDEF _ISS_PLAY_DEBUGMODE_}
    WriteLn('ISS_PLAY: Clearing samples from the player device...');
    Write  ('ISS_PLAY: ');
    FreedSmpNum:=0;
   {$ENDIF}
   { * Clearing samples from the player device * }
   For Counter:=1 To MInstrNum Do Begin
     With MInstruments[Counter]^ Do Begin
       If ISampleNum>0 Then Begin
         For Counter2:=0 To ISampleNum-1 Do Begin
           If ISamples[Counter2]^.SLength>0 Then Begin
             If ISS_FreeSample(ISamples[Counter2]) Then Begin
               {$IFDEF _ISS_PLAY_DEBUGMODE_}
                Write('*');
                Inc(FreedSmpNum);
               {$ENDIF}
              End Else Begin
               {$IFDEF _ISS_PLAY_DEBUGMODE_}
                Write('!');
                Inc(FreedSmpNum);
               {$ENDIF}
              End;
            End;
          End;
        End;
      End;
    End;
   {$IFDEF _ISS_PLAY_DEBUGMODE_}
    WriteLn(' [',FreedSmpNum,'] - DONE');
   {$ENDIF}
  End;

 { * Deallocating Unpacked Pattern Memory * }
 Dispose(ISSPlay_CurrentPatternData);
 { * Deallocating Player Variables * }
 Dispose(ISS_Player);
 { * Clearing currentmodule pointer * }
 ISS_CurrentModule:=Nil;

 ISS_DoneModulePlay:=True;
End;

{ * Main procedure initializes some variables * }
Begin
 ISSPlay_InAction:=False;
End.
