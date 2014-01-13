{ þ Most simple moduleplayer possible with ISS. þ }
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

 { þ Initializes the device drivers and the loaders. This procedure was þ }
 { þ done by the units' startup code, in version 0.1.6 and below, but þ }
 { þ beginning with 0.1.7 this should be called by the user program. þ }
 { þ This allows more flexible integration of the soundsystem into custom þ }
 { þ environments. þ }
 { þ IMPORTANT: This _MUST_ be the first function called, and _MUST NOT_ þ }
 { þ            be called twice or more! þ }
 ISS_InitSystem;

 { þ Selects the best device automatically, and sets up the soundsystem þ }
 If Not ISS_AutoSetup Then Begin
   WriteLn('PLY_FAIL: No devices available for playback!');
   Halt(1);
  End;

 { þ Inits the selected device and the user level system structures. þ }
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
 ISS_StartModulePlay; { þ Starts the player... þ }
 WriteLn('PLY_PLAY: Playing... (Press any key to exit)');

 Repeat
  {$IFDEF OS2}
   DosSleep(32); { þ Moolteetaskeen' roolz 8) þ }
  {$ENDIF}
 Until Keypressed;  { þ Waiting for the user þ }

 WriteLn('PLY_PLAY: Stopping playback.');
 ISS_StopModulePlay; { þ Stops the player þ }

 WriteLn('PLY_DONE: Shutting down module player.');
 ISS_DoneModulePlay;

 WriteLn('PLY_DONE: Freeing up module.');
 ISS_FreeModule(Music); { þ Frees up memory allocated for the module þ }

 { þ Shuts down selected device and deinitalizes the system. þ }
 { þ After this, you can call ISS_Init again, if you need. You _MUST NOT_ þ }
 { þ call ISS_InitSystem again! þ }
 ISS_Done;

 While Keypressed Do Readkey;
End.