{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ ISS_OS2M.PAS - OS/2 Warp Multimedia (MMOS/2) Subsystem Interface       ³}
{³                  Work started     : 2001.06.18.                          ³}
{³                  Last modification: 2001.06.29.                          ³}
{³             OS - EMX (OS/2) only.                                        ³}
{³                                                                          ³}
{³            ISS - Inquisition Sound Server for Free Pascal                ³}
{³                  Code by Karoly Balogh (a.k.a. Charlie/iNQ)              ³}
{³                  Copyright (C) 1998-2001 Inquisition                     ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ The information provided here is based on the IBM OS/2 Warp Developers ³}
{³   Toolkit, and the Virtual Pascal/2 RTL sources. Since I own a full and  ³}
{³   legal version of IBM VAC++ 4.0, and VP/2 is free, lawyers should go to ³}
{³   hell. IBM sucks, give us better and FREE docs about OS/2 programming!  ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿}
{³ þ This is not the full MMOS/2 interface of course. It contains only the  ³}
{³   required parts to have working DART stuff. It should be a part of the  ³}
{³   FPC/2 RTL, but since i'm so lazy, it's not. :) Maybe this will change  ³}
{³   in the future. (Send me some e-mails, if you need more...)             ³}
{ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ}
{$INCLUDE ISS_SET.INC}
{$MODE FPC}

{$HINTS OFF} { þ Enable this if you modify the source! þ }
{$NOTES OFF} { þ Enable this if you modify the source! þ }
Unit ISS_OS2M;

Interface


{ þ >>> O S 2 M E D E F <<< þ }

Const { þ WaveForm datatype þ }
      DATATYPE_WAVEFORM       = $0001;


{ þ >>> M C I O S / 2 <<< þ }

Const { þ MCI command messages identifiers þ }
      MCI_OPEN                      = 1;
      MCI_CLOSE                     = 2;
      MCI_PLAY                      = 4;
      MCI_STOP                      = 6;

      { þ Common message flags. $00000$x are reserved for common flags. þ }
      MCI_WAIT                      = $00000002;

      { þ MCI Device Type Constants þ }
      MCI_DEVTYPE_WAVEFORM_AUDIO    = 7;
      MCI_DEVTYPE_AUDIO_AMPMIX      = 9;

      { þ Wave format tag defines þ }
      MCI_WAVE_FORMAT_PCM           = DATATYPE_WAVEFORM;

      { þ Parameters and flags for the MCI_OPEN message þ }
      { þ $000$X00 are reserved for MCI_OPEN flags þ }
      MCI_OPEN_TYPE_ID              = $00001000;
      MCI_OPEN_SHAREABLE            = $00002000;

Type  { þ Parameters for default command messages with empty parameter lists þ }
      MCI_Generic_Parms = Record
        hwndCallback : Cardinal;
       End;
      PMCI_Generic_Parms = ^MCI_Generic_Parms;

      { þ Parameters for the AMP MCI_OPEN message þ }
      MCI_Amp_Open_Parms = Record
        hwndCallback   : Cardinal; { þ PM window handle for MCI notify message þ }
        usDeviceID     : Word;     { þ Device ID returned to user þ }
        usReserved0    : Word;     { þ Reserved field þ }
        pszDeviceType  : PChar;    { þ Device name from SYSTEM.INI þ }
        pszElementName : PChar;    { þ Typically a file name or NULL þ }
        pszAlias       : PChar;    { þ Optional device alias þ }
        pDevDataPtr    : Pointer;  { þ Pointer to device data þ }
       End;
      PMCI_Amp_Open_Parms = ^MCI_Amp_Open_Parms;

Function MCISendCommand(usDeviceID: Word; usMessage: Word; ulParam1: Cardinal;
                        Var Param2; usUserParm: Word): Cardinal; CDecl;
Function MCIGetErrorString(ulError: Cardinal; pszBuffer: PChar; usLength: Word): Cardinal; CDecl;


{ þ >>> M E E R R O R <<< þ }

Const { þ MCI Device Manager Error Return Codes þ }
      MCIERR_BASE                       = 5000;
      MCIERR_SUCCESS                    = 0;

      { þ Sync/Stream Manager Error Return codes þ }
      MEBASE                            = (MCIERR_BASE + 500);
      ERROR_DEVICE_UNDERRUN             = (MEBASE + 127);


{ þ >>> M M I O O S / 2 <<< þ }

Type Wave_Header = Record
       usFormatTag       : Word;     { þ Type of wave format þ }
       usChannels        : Word;     { þ Number of channels þ }
       ulSamplesPerSec   : Cardinal; { þ Sampling rate þ }
       ulAvgBytesPerSec  : Cardinal; { þ Avg bytes per sec þ }
       usBlockAlign      : Word;     { þ Block Alignment in bytes þ }
       usBitsPerSample   : Word;     { þ Bits per sample þ }
      End;
     PWave_Header = ^Wave_Header;


Implementation


{ þ >>> M C I O S / 2 <<< þ }

Function MCISendCommand(usDeviceID: Word; usMessage: Word; ulParam1: Cardinal;
                        Var Param2; usUserParm: Word): Cardinal; CDecl;
External 'MDM' Index 1;

Function MCIGetErrorString(ulError: Cardinal; pszBuffer: PChar; usLength: Word): Cardinal; CDecl;
External 'MDM' Index 3;


Begin
End.
{ þ ISS_OS2M.PAS - (C) 2001 Charlie/Inquisition þ }
