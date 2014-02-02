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

{ * ISS_OS2M.PAS - OS/2 Warp Multimedia (MMOS/2) Subsystem Interface      } 
{             OS - EMX (OS/2) only.                                       } 

{ * The information provided here is based on the IBM OS/2 Warp Developers }
{   Toolkit, and the Virtual Pascal/2 RTL sources. Since I own a full and  }
{   legal version of IBM VAC++ 4.0, and VP/2 is free, lawyers should go to }
{   hell. IBM sucks, give us better and FREE docs about OS/2 programming!  }

{ * This is not the full MMOS/2 interface of course. It contains only the  }
{   required parts to have working DART stuff. It should be a part of the  }
{   FPC/2 RTL, but since i'm so lazy, it's not. :) Maybe this will change  }
{   in the future. (Send me some e-mails, if you need more...)             }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}

{$HINTS OFF} { * Enable this if you modify the source! * }
{$NOTES OFF} { * Enable this if you modify the source! * }
Unit ISS_OS2M;

Interface


{ * >>> O S 2 M E D E F <<< * }

Const { * WaveForm datatype * }
      DATATYPE_WAVEFORM       = $0001;


{ * >>> M C I O S / 2 <<< * }

Const { * MCI command messages identifiers * }
      MCI_OPEN                      = 1;
      MCI_CLOSE                     = 2;
      MCI_PLAY                      = 4;
      MCI_STOP                      = 6;

      { * Common message flags. $00000$x are reserved for common flags. * }
      MCI_WAIT                      = $00000002;

      { * MCI Device Type Constants * }
      MCI_DEVTYPE_WAVEFORM_AUDIO    = 7;
      MCI_DEVTYPE_AUDIO_AMPMIX      = 9;

      { * Wave format tag defines * }
      MCI_WAVE_FORMAT_PCM           = DATATYPE_WAVEFORM;

      { * Parameters and flags for the MCI_OPEN message * }
      { * $000$X00 are reserved for MCI_OPEN flags * }
      MCI_OPEN_TYPE_ID              = $00001000;
      MCI_OPEN_SHAREABLE            = $00002000;

Type  { * Parameters for default command messages with empty parameter lists * }
      MCI_Generic_Parms = Record
        hwndCallback : Cardinal;
       End;
      PMCI_Generic_Parms = ^MCI_Generic_Parms;

      { * Parameters for the AMP MCI_OPEN message * }
      MCI_Amp_Open_Parms = Record
        hwndCallback   : Cardinal; { * PM window handle for MCI notify message * }
        usDeviceID     : Word;     { * Device ID returned to user * }
        usReserved0    : Word;     { * Reserved field * }
        pszDeviceType  : PChar;    { * Device name from SYSTEM.INI * }
        pszElementName : PChar;    { * Typically a file name or NULL * }
        pszAlias       : PChar;    { * Optional device alias * }
        pDevDataPtr    : Pointer;  { * Pointer to device data * }
       End;
      PMCI_Amp_Open_Parms = ^MCI_Amp_Open_Parms;

Function MCISendCommand(usDeviceID: Word; usMessage: Word; ulParam1: Cardinal;
                        Var Param2; usUserParm: Word): Cardinal; CDecl;
Function MCIGetErrorString(ulError: Cardinal; pszBuffer: PChar; usLength: Word): Cardinal; CDecl;


{ * >>> M E E R R O R <<< * }

Const { * MCI Device Manager Error Return Codes * }
      MCIERR_BASE                       = 5000;
      MCIERR_SUCCESS                    = 0;

      { * Sync/Stream Manager Error Return codes * }
      MEBASE                            = (MCIERR_BASE + 500);
      ERROR_DEVICE_UNDERRUN             = (MEBASE + 127);


{ * >>> M M I O O S / 2 <<< * }

Type Wave_Header = Record
       usFormatTag       : Word;     { * Type of wave format * }
       usChannels        : Word;     { * Number of channels * }
       ulSamplesPerSec   : Cardinal; { * Sampling rate * }
       ulAvgBytesPerSec  : Cardinal; { * Avg bytes per sec * }
       usBlockAlign      : Word;     { * Block Alignment in bytes * }
       usBitsPerSample   : Word;     { * Bits per sample * }
      End;
     PWave_Header = ^Wave_Header;


Implementation


{ * >>> M C I O S / 2 <<< * }

Function MCISendCommand(usDeviceID: Word; usMessage: Word; ulParam1: Cardinal;
                        Var Param2; usUserParm: Word): Cardinal; CDecl;
External 'MDM' Index 1;

Function MCIGetErrorString(ulError: Cardinal; pszBuffer: PChar; usLength: Word): Cardinal; CDecl;
External 'MDM' Index 3;


Begin
End.
