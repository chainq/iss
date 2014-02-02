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

{ * ISS_HNDL.PAS - High level fileformat handler routines                 }
{             OS - Platform Independent                                   }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
{$IOCHECKS OFF}
Unit ISS_Load;

Interface

Uses ISS_Var { * Uses the system variables and types * }
     {$IFDEF _ISS_XM_INCLUDE_}
      ,ISS_XM  { * Includes the XM handler * }
     {$ENDIF}
     {$IFDEF _ISS_S3M_INCLUDE_}
      ,ISS_S3M { * Includes the S3M handler * }
     {$ENDIF}
     {$IFDEF _ISS_MOD_INCLUDE_}
      ,ISS_MOD { * Includes the MOD handler * }
     {$ENDIF}
     {$IFDEF _ISS_LOAD_IDSMODE_}
      ,IDS_LOAD
     {$ENDIF}
     ;

{$IFNDEF _ISS_LOAD_NOFILEMODE_}
Function ISS_LoadModule(FileName : String; Var Module : ISS_PModule) : Boolean;
{$ENDIF}
{$IFDEF _ISS_LOAD_IDSMODE_}
Function ISS_IDSLoadModule(DFHandle : IDS_PDataFile; FileName : String;
                           Var Module : ISS_PModule) : Boolean;
{$ENDIF}
Function ISS_LoadInternalModule(ModMem : Pointer;
                            Var Module : ISS_PModule) : Boolean;
Function ISS_FreeModule(Var Module : ISS_PModule) : Boolean;
Procedure ISS_DecodePattern(EncPat : ISS_PPattern; DecPat : ISS_PDecodedPattern);

Function ISS_InitHandlers : Boolean;

Implementation

Var ISS_HandlerOK  : Boolean; { * True if there is an usable handler * }
    ISS_HandlerNum : DWord;   { * Number of handlers * }

    { * Handlers * }
    ISS_Handler    : Array[1..ISS_MaxHandlers] Of ISS_PModuleHandler;


{$IFDEF _ISS_LOAD_IDSMODE_}
Function ISS_IDSLoadModule(DFHandle : IDS_PDataFile; FileName : String;
                           Var Module : ISS_PModule) : Boolean;
Var ModuleFile : IDS_PFile;
Begin
 ISS_IDSLoadModule:=False;
 If DFHandle=Nil Then Exit;
 If Not ISS_HandlerOk Then Exit;

 ModuleFile:=IDS_OpenFile(DFHandle,FileName);
 If ModuleFile=Nil Then Exit;

 With ModuleFile^ Do Begin
   ISS_IDSLoadModule:=ISS_LoadInternalModule(FData,Module);
  End;
 IDS_CloseFile(ModuleFile);
End;
{$ENDIF}

{$IFNDEF _ISS_LOAD_NOFILEMODE_}

{ * ISS_LoadModule                                                          }
{                                                                           }
{ . Description : Opens a module file, reads it to the memory, and calls    }
{                 ISS_LoadInternalModule procedure to load the module to    }
{                 the player. Returns a pointer to the loaded module        }
{                 structure, and the error code.                            }
{                                                                           }
{ . Parameters  : FileName - [I] The file name of the module to be loaded.  }
{                 Module   - [O] Pointer to the loaded module structure.    }
{                                                                           }
{ . Returns     : A Boolean value, true if successful, false if not         }

Function ISS_LoadModule(FileName : String; Var Module : ISS_PModule) : Boolean;
Var ModuleFile : File;
    ModuleSize : DWord;
    ModuleMem  : Pointer;
Begin
 ISS_LoadModule:=False;
 If ISS_HandlerOK Then Begin
   { * Opening the file * }
   Assign(ModuleFile, FileName);
   FileMode:=0;
   Reset(ModuleFile,1);
   If IOResult<>0 Then Begin
     { * ERROR CODE! * }
     Close(ModuleFile);
     Exit;
    End;

   { * Loading the file into memory * }
   ModuleSize:=FileSize(ModuleFile);
   GetMem(ModuleMem,ModuleSize);     { * Allocating memory * }
   BlockRead(ModuleFile,ModuleMem^,ModuleSize); { * Loading file * }
   If IOResult<>0 Then Begin
     { * ERROR CODE! * }
     FreeMem(ModuleMem,ModuleSize); { * Freeing up memory * }
     Close(ModuleFile);
     Exit;
    End;

   { * Loading Module * }
   ISS_LoadModule:=ISS_LoadInternalModule(ModuleMem,Module);

   FreeMem(ModuleMem,ModuleSize); { * Freeing up memory * }
   Close(ModuleFile); { * Closing the file * }

  End;
End;
{$ENDIF}

{ * ISS_LoadInternalModule                                                  }
{                                                                           }
{ . Description : Loads a module from the specified memory area using the   }
{                 low level module loading routines, and returns a pointer  }
{                 to the module structure, and the error code.              }
{                                                                           }
{ . Parameters  : ModMem - [I] A pointer to the "raw" module to be loaded.  }
{                 Module - [O] Pointer to the loaded module structure.      }
{                                                                           }
{ . Returns     : A Boolean value, true if successful, false if not         }

Function ISS_LoadInternalModule(ModMem : Pointer;
                            Var Module : ISS_PModule) : Boolean;
Var Counter : DWord;
Begin
 ISS_LoadInternalModule:=False;

 { * Is the pointer specified free? * }
 If (Module<>Nil) And (Module^.MID=ISS_ModuleID) Then Begin
   { * ERROR CODE! * }
   Exit;
  End;

 Module:=Nil;
 If ISS_HandlerOK Then Begin

   { * Selecting Handler * }
   Counter:=0;
   Repeat
    Inc(Counter);
    If Counter>ISS_HandlerNum Then Begin
      { * ERROR CODE! * }
      Exit;
     End;
    ISS_Handler[Counter]^.ModuleMem:=ModMem;
   Until ISS_Handler[Counter]^.CheckModule();

   { * Loading Module * }
   With ISS_Handler[Counter]^ Do Begin

     {$IFDEF _ISS_LOAD_DEBUGMODE_}
      DebugInit();
     {$ENDIF}

     New(ModulePtr); { * Allocating memory for the header * }
     If Not LoadHeader() Then Begin { * Loading Header * }
       { * ERROR CODE! * }
       Dispose(ModulePtr);
       Exit;
      End;
     ModulePtr^.MStatus:=0;        { * Clearing status * }
     ModulePtr^.MHandler:=Counter; { * Setting handler number * }

     { * Allocating pattern header memory * }
     For Counter:=0 To ModulePtr^.MPatternNum Do Begin
       New(ModulePtr^.MPatterns[Counter]);
      End;
     If Not LoadPatterns() Then Begin { * Loading Patterns * }
       { * Deallocating pattern header memory if loading failed * }
       For Counter:=0 To ModulePtr^.MPatternNum Do Begin
         Dispose(ModulePtr^.MPatterns[Counter]);
        End;
       { * ERROR CODE! * }
       Dispose(ModulePtr);
       Exit;
      End;

     { * Allocating instrument header memory * }
     For Counter:=1 To ModulePtr^.MInstrNum Do Begin
       New(ModulePtr^.MInstruments[Counter]);
      End;
     If Not LoadInstruments() Then Begin { * Loading Instruments * }
       { * Allocating instrument header memory * }
       For Counter:=1 To ModulePtr^.MInstrNum Do Begin
         Dispose(ModulePtr^.MInstruments[Counter]);
        End;
       { * Dellocating pattern header memory if loading failed * }
       For Counter:=0 To ModulePtr^.MPatternNum Do Begin
         Dispose(ModulePtr^.MPatterns[Counter]);
        End;
       { * ERROR CODE! * }
       Dispose(ModulePtr);
       Exit;
      End;

     {$IFDEF _ISS_LOAD_DEBUGMODE_}
      DebugDone();
     {$ENDIF}

     ModulePtr^.MID:=ISS_ModuleID;
     Module:=ModulePtr;
     ISS_LoadInternalModule:=True;

    End;
  End;
End;

{ * ISS_FreeModule                                                          }
{                                                                           }
{ . Description : Frees up the memory area allocated by the ISS_LoadModule  }
{                 function. Returns the error code.                         }
{                                                                           }
{ . Parameters  : Module - [I] Pointer to the loaded module structure.      }
{                                                                           }
{ . Returns     : A Boolean value, true if successful, false if not         }

Function ISS_FreeModule(Var Module : ISS_PModule) : Boolean;
Var Counter  : DWord;
    Counter2 : DWord;
Begin
 ISS_FreeModule:=False;
 If ISS_HandlerOK Then Begin

   { * Specified pointer not points to a loaded module? * }
   If (Module=Nil) Or (Module^.MID<>ISS_ModuleID) Then Begin
     { * ERROR CODE! * }
     Exit;
    End;

   {$IFDEF _ISS_LOAD_DEBUGMODE_}
    Write('LDR_INIT: Deallocating module memory... ');
   {$ENDIF}

   With Module^ Do Begin

     { * Module is loaded to the player device? * }
     If MStatus>0 Then Begin
       { * ERROR CODE! * }
       Exit;
      End;

     { * Freeing up instrument memory * }
     For Counter:=1 To MInstrNum Do Begin
       With MInstruments[Counter]^ Do Begin

         { * Free up samples memory * }
         If ISampleNum>0 Then Begin
           For Counter2:=0 To ISampleNum-1 Do Begin
             With ISamples[Counter2]^ Do Begin
               If SLength>0 Then FreeMem(SData,SLength);
              End;
             Dispose(ISamples[Counter2]);
            End;
          End;

        End;
       Dispose(MInstruments[Counter]);
      End;

     { * Freeing up pattern memory * }
     For Counter:=0 To MPatternNum Do Begin
       { * Free up pattern data memory * }
       With MPatterns[Counter]^ Do Begin
         FreeMem(PatRows,PatSize);
        End;
       Dispose(MPatterns[Counter]);
      End;
    End;

   Module^.MID:='!?!?'; { * Destroying module ID * }
   Dispose(Module);     { * Freeing up module header * }

   {$IFDEF _ISS_LOAD_DEBUGMODE_}
    WriteLn('DONE.');
   {$ENDIF}

   ISS_FreeModule:=True;
  End;
End;

{ * Decodes a pattern using specified handler * }
Procedure ISS_DecodePattern(EncPat : ISS_PPattern; DecPat : ISS_PDecodedPattern);
Begin
 If ISS_CurrentModule<>NIL Then Begin
   With ISS_CurrentModule^ Do Begin
     ISS_Handler[MHandler]^.DecodePattern(EncPat,DecPat);
    End;
  End;
End;


{ * Initializes the low-level fileformat-handler routines. * }
Function ISS_InitHandlers : Boolean;
Begin
 ISS_HandlerNum:=0;

 {$IFDEF _ISS_XM_INCLUDE_}
  Inc(ISS_HandlerNum);
  ISS_XMHandlerInit;
  ISS_Handler[ISS_HandlerNum]:=@ISS_XMHandler;
 {$ENDIF}

 {$IFDEF _ISS_S3M_INCLUDE_}
  Inc(ISS_HandlerNum);
  ISS_S3MHandlerInit;
  ISS_Handler[ISS_HandlerNum]:=@ISS_S3MHandler;
 {$ENDIF}

 {$IFDEF _ISS_MOD_INCLUDE_}
  Inc(ISS_HandlerNum);
  ISS_MODHandlerInit;
  ISS_Handler[ISS_HandlerNum]:=@ISS_MODHandler;
 {$ENDIF}

 If ISS_HandlerNum<>0 Then ISS_HandlerOK:=True Else ISS_HandlerOk:=False;

 {$IFDEF _ISS_LOAD_DEBUGMODE_}
  If ISS_HandlerOK Then
    WriteLn('HND_INIT: ',ISS_HandlerNum,' Module Handler(s) Initialized')
   Else
    WriteLn('HND_INIT: WARNING! No Module Handlers Included!');
 {$ENDIF}

 ISS_InitHandlers:=ISS_HandlerOK;
End;

Begin
End.
