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

{ * ISS_SET .INC - System Directives Setup File                           }
{             OS - Platform Independent                                   }

{ * >>> H A N D L E R  S E T T I N G S <<< * }

{$DEFINE _ISS_LOAD_DEBUGMODE_}
{ DEFINE _ISS_LOAD_NOFILEMODE_}
{ DEFINE _ISS_LOAD_IDSMODE_}
{ DEFINE _ISS_LOAD_CREATELOGFILE_}

{$DEFINE _ISS_XM_INCLUDE_}
{ DEFINE _ISS_S3M_INCLUDE_}
{$DEFINE _ISS_MOD_INCLUDE_}


{ * >>> D E V I C E  S E T T I N G S <<< * }

{$IFDEF GO32V2}
 {$DEFINE _ISS_SYS_SUPPORTED_}
 {$DEFINE _ISS_GUS_INCLUDE_}
 {$DEFINE _ISS_GUS_DEBUGMODE_}

 {$DEFINE _ISS_SB_INCLUDE_}
 { DEFINE _ISS_SB_FORCEDSPVERSION_} { * Dirty hack, will be removed! * }
 {$DEFINE _ISS_SB_DEBUGMODE_}

 {$DEFINE _ISS_NSND_INCLUDE_}
 {$DEFINE _ISS_NSND_DEBUGMODE_}
 {$DEFINE _ISS_NSND_SLOWMODE_}
{$ENDIF}

{$IFDEF OS2}
 {$DEFINE _ISS_SYS_SUPPORTED_}
 {$DEFINE _ISS_GUSNATIVE_INCLUDE_}
 {$DEFINE _ISS_GUSNATIVE_DEBUGMODE_}

 {$DEFINE _ISS_DART_INCLUDE_}
 {$DEFINE _ISS_DART_DEBUGMODE_}
{$ENDIF}

{$IFDEF WIN32}
 { DEFINE _ISS_SYS_SUPPORTED_}
 { DEFINE _ISS_WAVEOUT_INCLUDE_}
 {$DEFINE _ISS_WAVEOUT_DEBUGMODE_}

 { DEFINE _ISS_DSOUND_INCLUDE_}
 {$DEFINE _ISS_DSOUND_DEBUGMODE_}
{$ENDIF}

{$IFDEF LINUX}
 { DEFINE _ISS_SYS_SUPPORTED_}
 { DEFINE _ISS_OSS_INCLUDE_}
 {$DEFINE _ISS_OSS_DEBUGMODE_}
{$ENDIF}

{$IFDEF AMIGA}
 { DEFINE _ISS_SYS_SUPPORTED_}
 { DEFINE _ISS_AHI_INCLUDE_}
 {$DEFINE _ISS_AHI_DEBUGMODE_} 

 {$DEFINE _ISS_AMIGA_INCLUDE_}
 {$DEFINE _ISS_AMIGA_DEBUGMODE_}
{$ENDIF}

{$IFDEF MORPHOS}
 { DEFINE _ISS_SYS_SUPPORTED_}
 { DEFINE _ISS_AHI_INCLUDE_}
 {$DEFINE _ISS_AHI_DEBUGMODE_}
{$ENDIF}

{$IFNDEF _ISS_SYS_SUPPORTED_}
{$FATAL Support for this operating system is not implemented!}
{$ENDIF}


{ * Diskwriter device should be used for debugging purposes only, * }
{ * because it's not fail safe. * }
{$DEFINE _ISS_DISK_INCLUDE_}
{$DEFINE _ISS_DISK_DEBUGMODE_}


{ * >>> S O U N D S Y S T E M  S E T T I N G S <<< * }

{$DEFINE _ISS_MAIN_DEBUGMODE_}


{ * >>> P L A Y E R  S E T T I N G S <<< * }

{$DEFINE _ISS_PLAY_DEBUGMODE_}


{ * >>> G L O B A L  D E F I N E S <<< * }

{$IFDEF CPUI386}
{ * This is REQUIRED. If the record fields not aligned to WORD, * }
{ * the mixer WILL crash. This may change in the future. * }
 {$PACKRECORDS 2}
{$ELSE}
{$IFDEF CPUM68K}
{ * This is REQUIRED. If the record fields not aligned to DWORD, * }
{ * the mixer WILL crash. This may change in the future. * }
 {$PACKRECORDS 4}
{$ELSE}
{ * FIXME: have a default for now... I can't remember which is required... * }
 {$PACKRECORDS 2}
{$ENDIF}
{$ENDIF}