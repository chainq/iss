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

{ * Most simple moduleplayer possible with ISS. * }
Program ISSPlay;

Uses Crt,
     ISS_Play,ISS_Sys,ISS_Load,ISS_Var
     {$IFDEF OS2}
      ,DOSCalls
     {$ENDIF}
     ;

Var Music    : ISS_PModule;
    FileName : String;

Begin
 If ParamCount=0 Then Begin
   WriteLn('PLY_FAIL: No filename specified!');
   WriteLn;
   Exit;
  End;
 WriteLn;

 FileName:=ParamStr(1);

 { * Initializes the device drivers and the loaders. This procedure was * }
 { * done by the units' startup code, in version 0.1.6 and below, but * }
 { * beginning with 0.1.7 this should be called by the user program. * }
 { * This allows more flexible integration of the soundsystem into custom * }
 { * environments. * }
 { * IMPORTANT: This _MUST_ be the first function called, and _MUST NOT_ * }
 { *            be called twice or more! * }
 ISS_InitSystem;

 { * Selects the best device automatically, and sets up the soundsystem * }
 If Not ISS_AutoSetup Then Begin
   WriteLn('PLY_FAIL: No devices available for playback!');
   Halt(1);
  End;

 { * Inits the selected device and the user level system structures. * }
 If Not ISS_Init Then Begin
   WriteLn('PLY_FAIL: Could not initialize selected device!');
   Halt(1);
  End;

 WriteLn;
 WriteLn('PLY_LOAD: Loading module...');
 If Not ISS_LoadModule(FileName,Music) Then Begin
   WriteLn('PLY_FAIL: Failed to load module!');
   ISS_Done;
   Halt(1);
  End;

 ISS_InitModulePlay(Music);

 WriteLn('PLY_PLAY: Starting playback...');
 ISS_StartModulePlay; { * Starts the player... * }
 WriteLn('PLY_PLAY: Playing... (Press any key to exit)');

 Repeat
  {$IFDEF OS2}
   DosSleep(32); { * Moolteetaskeen' roolz 8) * }
  {$ENDIF}
 Until Keypressed;  { * Waiting for the user * }

 WriteLn('PLY_PLAY: Stopping playback.');
 ISS_StopModulePlay; { * Stops the player * }

 WriteLn('PLY_DONE: Shutting down module player.');
 ISS_DoneModulePlay;

 WriteLn('PLY_DONE: Freeing up module.');
 ISS_FreeModule(Music); { * Frees up memory allocated for the module * }

 { * Shuts down selected device and deinitalizes the system. * }
 { * After this, you can call ISS_Init again, if you need. You _MUST NOT_ * }
 { * call ISS_InitSystem again! * }
 ISS_Done;

 While Keypressed Do Readkey;
End.
