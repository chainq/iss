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

{ * Inquisition's Timer Services Unit                   * }
{ * ISS_TIM - Timer Unit (GO32V2 Only!)                 * }
{ * Note: req. FPC version 1.0.0+ for GO32V2 to compile * }

{$ASMMODE INTEL}
{$MODE FPC}
Unit ISS_Tim;

Interface

Uses GO32;

Const ISS_TimerSpeed  : DWord = 1193180;
      ISS_MaxTimers   = $8; { * Maximum Number of Timers  * }

      TimerIRQ        = $8; { * HW IRQ Number * }

      ISS_TENoFree   = $01; { * Can't add new timer. All timers locked. * }
      ISS_TENotFound = $02; { * Can't find specified Timer, to stop. * }

Type TTimerStruc = Record
       TSpeed     : DWord;
       TCount     : DWord;     { * Tick Counter * }
       TPrevCount : DWord;     { * Tick Counter state at prev. activity * }
       TProc      : Pointer;   { * Procedure To Call Offset * }
       TActive    : Boolean;   { * 1 If The Timer Is On * }
      End;

Var ISS_TimersData : Array[1..ISS_MaxTimers] Of TTimerStruc;
    ISS_TimerError : DWord; { * Contains the last timer error code. * }

Function ISS_StartTimer(Var NewTProc : Pointer; NewTSpeed : DWord) : Boolean;
Function ISS_StopTimer(Var TimerProc : Pointer) : Boolean;
Function ISS_GetTimerNumber(TimerProc : Pointer) : DWord;

Implementation

Var TimerSpeed      : DWord;
    OldTimer        : TSegInfo;
    OldTimerCnt     : DWord;
    NewIRQActive    : Boolean;
    NewTimerHandler : TSegInfo;
    BackupDS        : Word; External Name '___v2prt0_ds_alias';


Procedure UpdateUserTimers;
Type Proc   = Procedure;
Var Counter : Word;
Begin
 For Counter:=1 To ISS_MaxTimers Do Begin
   With ISS_TimersData[Counter] Do Begin
     If TActive Then Begin
       Inc(TCount,TimerSpeed);
       If (TCount>TSpeed) Then Begin
         Dec(TCount,TSpeed);
         TPrevCount:=TCount;
         Proc(TProc); { * Calling the specified routine * }
        End;
      End;
    End;
  End;
End;
Procedure UpdateUserTimers_Dummy; Begin End;

Procedure SysTimerIRQ; Assembler;
Asm
  CLI
  PUSH   DS
  PUSH   ES
  PUSH   FS
  PUSH   GS
  PUSHAD
  MOV    AX,CS:[BackupDS]
  MOV    DS,AX
  MOV    ES,AX
  MOV    AX,DosMemSelector
  MOV    FS,AX

  CALL   UpdateUserTimers

  MOV    EAX,TimerSpeed
  ADD    OldTimerCnt,EAX
  CMP    OldTimerCnt,$10000
  JB     @NotUpdateClock

    SUB   OldTimerCnt,$10000
    INC   WORD PTR FS:[1132]
    JNZ   @Timer_2
    INC   WORD PTR FS:[1134]
    @Timer_2:
    MOV  AX,$018
    CMP  FS:[1134],AX
    JNZ  @Timer_3
    MOV  AX,$0B0
    CMP  FS:[1132],AX
    JNZ  @Timer_3
    MOV  WORD PTR FS:[1134],$0
    MOV  WORD PTR FS:[1132],$0
    MOV  BYTE PTR FS:[1136],$1
    @Timer_3:

  @NotUpdateClock:
  MOV   DX,$20 { * Interrupt request acknowledge * }
  MOV   AL,$20
  OUT   DX,AL
  POPAD
  POP   GS
  POP   FS
  POP   ES
  POP   DS
  IRET
End;
Procedure SysTimerIRQ_Dummy; Begin End;

Procedure SetTimerSpeed(NewTimerSpeed : DWord);
Begin
 If NewTimerSpeed<>TimerSpeed Then Begin
   Asm
    PUSH EAX
    CLI
    MOV  AL,00110110B
    OUT  43H,AL
    MOV  EAX,NEWTIMERSPEED
    OUT  40H,AL
    MOV  AL,AH
    OUT  40H,AL
    STI
    POP  EAX
   End;
   TimerSpeed:=NewTimerSpeed;
  End;
End;

Function GetTimerSpeed : DWord;
Var Counter  : DWord;
    TFastest : DWord;
Begin
 TFastest:=$10000;
 For Counter:=1 To ISS_MaxTimers Do Begin
   If ISS_TimersData[Counter].TActive And
      (ISS_TimersData[Counter].TSpeed < TFastest) Then
     TFastest:=ISS_TimersData[Counter].TSpeed;
  End;
 GetTimerSpeed:=TFastest;
End;

Function ISS_StartTimer(Var NewTProc : Pointer; NewTSpeed : DWord) : Boolean;
Var Counter : Word;
    TNumber : Word;
Begin
 Counter:=0; TNumber:=0;
 Repeat
  Inc(Counter);
  If Not ISS_TimersData[Counter].TActive Then TNumber:=Counter;
 Until (TNumber<>0) Or (Counter=ISS_MaxTimers);
 If TNumber=0 Then Begin
   ISS_TimerError:=ISS_TENoFree;
   ISS_StartTimer:=False;
   Exit;
  End;
 If Not NewIRQActive Then Begin
   Lock_Data(ISS_TimersData,SizeOf(ISS_TimersData));
   Lock_Data(DosMemSelector,SizeOf(DosMemSelector));
   Lock_Code(@SysTimerIRQ,DWord(@SysTimerIRQ_Dummy)-DWord(@SysTimerIRQ));
   Lock_Code(@UpdateUserTimers,DWord(@UpdateUserTimers_Dummy)-DWord(@UpdateUserTimers));
   NewTimerHandler.Offset:=@SysTimerIRQ;
   NewTimerHandler.Segment:=Get_CS;
   Get_PM_Interrupt(TimerIRQ,OldTimer);
   Set_PM_Interrupt(TimerIRQ,NewTimerHandler);
  End;
 ISS_TimersData[TNumber].TSpeed:=NewTSpeed;
 ISS_TimersData[TNumber].TCount:=0;
 ISS_TimersData[TNumber].TProc:=NewTProc;
 ISS_TimersData[TNumber].TActive:=True;
 SetTimerSpeed(GetTimerSpeed);
 ISS_StartTimer:=True;
End;

Function ISS_StopTimer(Var TimerProc : Pointer) : Boolean;
Var TNumber    : Word;
    Counter    : Word;
    LastTimer  : Boolean;
Begin
 Disable;
 TNumber:=0;
 For Counter:=1 To ISS_MaxTimers Do Begin
   With ISS_TimersData[Counter] Do Begin
     If TActive And (TProc=TimerProc) Then TNumber:=Counter;
    End;
  End;
 If TNumber=0 Then Begin
   ISS_TimerError:=ISS_TENotFound;
   ISS_StopTimer:=False;
   Enable;
  End Else Begin
   ISS_TimersData[TNumber].TActive:=False;
   LastTimer:=True;
   For Counter:=1 To ISS_MaxTimers Do Begin
     If ISS_TimersData[Counter].TActive=True Then LastTimer:=False;
    End;
   If LastTimer Then Begin
     TimerSpeed:=0;
     SetTimerSpeed($10000);
     Set_PM_Interrupt(TimerIRQ,OldTimer);
     Unlock_Data(DosMemSelector,SizeOf(DosMemSelector));
     Unlock_Data(ISS_TimersData,SizeOf(ISS_TimersData));
     UnLock_Code(@SysTimerIRQ,LongInt(@SysTimerIRQ_Dummy)-LongInt(@SysTimerIRQ));
     UnLock_Code(@UpdateUserTimers,DWord(@UpdateUserTimers_Dummy)-DWord(@UpdateUserTimers));
    End;
   ISS_StopTimer:=True;
   Enable;
  End;
End;

Function ISS_GetTimerNumber(TimerProc : Pointer) : DWord;
Var Counter : DWord;
Begin
 For Counter:=1 To ISS_MaxTimers Do Begin
   With ISS_TimersData[Counter] Do Begin
     If TActive And (TProc=TimerProc) Then Begin
       ISS_GetTimerNumber:=Counter;
       Exit;
      End;
    End;
  End;
End;

Begin
 FillChar(ISS_TimersData,SizeOf(ISS_TimersData),#0);
 NewIRQActive:=False;
 TimerSpeed:=0;
End.
