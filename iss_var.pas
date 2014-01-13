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

{ * ISS_VAR .PAS - System Variables and Types                             }
{             OS - Platform Independent                                   }

{$INCLUDE ISS_SET.INC}
{$MODE FPC}
Unit ISS_Var;

Interface

Const { * Main system constants * }
      ISS_Version    : DWord = $021;    { * Version Number * }
      ISS_VersionStr : PChar = '0.2.1'; { * Version Number String * }

      {$IFDEF GO32V2}
       ISS_PlatformID  = $0001;
       ISS_PlatformStr = 'DOS-GO32V2';
      {$ENDIF}
      {$IFDEF OS2}
       ISS_PlatformID  = $0002;
       ISS_PlatformStr = 'OS/2-EMX';
      {$ENDIF}
      {$IFDEF LINUX}
       ISS_PlatformID  = $0004;
       ISS_PlatformStr = 'Linux/i386';
      {$ENDIF}
      {$IFDEF WIN32}
       ISS_PlatformID  = $0008;
       ISS_PlatformStr = 'Win32';
      {$ENDIF}

      ISS_MaxDevices = 4;  { * Maximum Number of Devices * }
      ISS_MaxLoaders = 1;  { * Maximum Number of Loaders * }

      ISS_MaxSSChannels = 32; { * Sound System Channel Number * }
      ISS_MaxPlChannels = 32; { * Player Channel Number * }

      { * Tracker IDs (used for playing incompatible effects correctly) * }
      ISS_TrackerID_PRO = 1; { * Protracker * }
      ISS_TrackerID_ST3 = 2; { * ScreamTracker 3.x * }
      ISS_TrackerID_FT2 = 3; { * Fasttracker 2 * }

      { * Device Type Flags * }
      ISS_Dev8Bit      =   1; { * Device supports 8bit output/samples * }
      ISS_Dev16Bit     =   2; { * Device supports 16bit output/samples * }
      ISS_DevMono      =   4; { * Device supports mono output * }
      ISS_DevStereo    =   8; { * Device supports stereo output * }
      ISS_DevSigned    =  16; { * Device supports signed samples * }
      ISS_DevUnsigned  =  32; { * Device supports unsigned samples * }
      ISS_DevMixed     =  64; { * Device supports mixing * }
      ISS_DevWaveTable = 128; { * Device supports wavetable mode * }
      ISS_DevDRAM      = 256; { * Device has on-board DRAM * }

      { * Sample Type Consts * } { * GUS Values * }
      ISS_Smp16BitData    = %00000100; { * 16bit SampleData * }
      ISS_SmpNoLoop       = %00000000; { * No Looping * }
      ISS_SmpForwardLoop  = %00001000; { * Forward Looping * }
      ISS_SmpPingPongLoop = %00011000; { * Bidirectional Looping * }

      { * Virtual Channel Control Flags * }
      ISS_CCActive       =   1; { * Channel Activity Flag             (0) * }
      ISS_CCStop         =   2; { * Stop Channel                      (1) * }
      ISS_CCSample       =   4; { * Change Channel Sample             (2) * }
      ISS_CCVolume       =   8; { * Change Channel Volume             (3) * }
      ISS_CCPeriod       =  16; { * Change Channel Period             (4) * }
      ISS_CCPanning      =  32; { * Change Channel Panning            (5) * }
      ISS_CCVolFadeOut   =  64; { * Volume Fadeout in progress        (6) * }

      { * Frequency Mode Flags * }
      ISS_AmigaFreq      =  0; { * Amiga Frequency Mode (MOD,XM) * }
      ISS_LinearFreq     =  1; { * Linear Frequency Mode (XM) * }

      { * Envelope Type Flags * }
      ISS_EnvEnabled     = 1; { * Envelope Enabled * }
      ISS_EnvSustain     = 2; { * Envelope Sustain * }
      ISS_EnvLoop        = 4; { * Envelope Looped * }

      { * Internal Module ID * }
      ISS_ModuleID = 'INQM';

      { * Internal Module Status Flags * }
      ISS_StLoaded       = 1; { * Module loaded to device * }
      ISS_StPlaying      = 2; { * Module currently playing * }

      { * Amiga Period Table (The one from the XM documentation.) * }
      ISS_AmigaPeriodTable : Array[0..12*8-1] Of Word = (
          907,900,894,887,881,875,868,862,856,850,844,838,832,826,820,814,
          808,802,796,791,785,779,774,768,762,757,752,746,741,736,730,725,
          720,715,709,704,699,694,689,684,678,675,670,665,660,655,651,646,
          640,636,632,628,623,619,614,610,604,601,597,592,588,584,580,575,
          570,567,563,559,555,551,547,543,538,535,532,528,524,520,516,513,
          508,505,502,498,494,491,487,484,480,477,474,470,467,463,460,457);

      { * Sinus Table used by vibrato and tremolo * }
      ISS_SineTable : Array[0..127] Of Integer = (
             0,  6, 12, 18, 24, 31, 37, 43, 49, 55, 61, 68, 74, 79, 85, 91,
            97,103,109,114,120,125,131,136,141,146,151,156,161,166,171,175,
           180,184,188,193,197,201,204,208,212,215,218,221,224,227,230,233,
           235,237,240,242,244,245,247,248,250,251,252,253,253,254,254,254,
           255,254,245,254,253,253,252,251,250,248,247,245,244,242,240,237,
           235,233,230,227,224,221,218,215,212,208,204,201,197,193,188,184,
           180,175,171,166,161,156,151,146,141,136,131,125,120,114,109,103,
            97, 91, 85, 79 ,74, 68, 61, 55, 49, 43, 37, 31, 24, 18, 12,  6);


Type  { * >>> D A T A  T Y P E S <<< * }

      { * Internal Pattern Row Format (Same as used by FT2) * }
      ISS_TPatternRow = Record
        RNote   : Byte; { * Note (0-71) 0 = C-0 * }
        RInstr  : Byte; { * Instrument Number (0-128) * }
        RVolCol : Byte; { * Volume Column Byte * }
        RFXType : Byte; { * Effect Type * }
        RFXParm : Byte; { * Effect Parameter * }
       End;
      ISS_PPatternRow = ^ISS_TPatternRow;

      { * Internal Pattern Format * }
      ISS_TPattern = Record
        PatRowsNum : Word;    { * Number of Rows in the Pattern * }
        PatSize    : DWord;   { * Pattern Data Size * }
        PatRows    : Pointer; { * Pointer to XM-Packed Pattern Data * }
       End;
      ISS_PPattern = ^ISS_TPattern;

      { * Internal Sample Format * }
      ISS_TSample = Record
        SName      : Array[0..21] Of Char; { * Sample Name * }
        SLength    : DWord; { * Sample Length * }
        SLoopStart : DWord; { * Sample Loop Start * }
        SLoopEnd   : DWord; { * Sample Loop End * }
        SVolume    : Byte;  { * Sample Volume * }
        SFineTune  : ShortInt; { * Sample FineTune (signed byte -16..+15) * }
        SType      : Byte;  { * Sample Type * }
        SPanning   : Byte;  { * Sample Panning * }
        SRelNote   : ShortInt;  { * Sample Relative Note (signed byte) * }
        SData      : Pointer; { * Pointer to the sample data * }
        SDRAMOffs  : DWord; { * Sample Offset in Wavetable DRAM * }
       End;
      ISS_PSample = ^ISS_TSample;

      { * An Envelope Point * }
      ISS_TEnvPoint = Record
        EPPosition  : Word; { * Position of This Envelope Point * }
        EPValue     : Word; { * Envelope Value at this Point * }
       End;

      { * Internal Envelope Format * }
      ISS_TEnvelope = Record
        EnvType      : Byte; { * Envelope Type * }
        EnvPointsNum : Byte; { * Number Of Envelope Points * }
        EnvSustain   : Byte; { * Envelope Sustain Point * }
        EnvLoopStart : Byte; { * Envelope Loop Start Point * }
        EnvLoopEnd   : Byte; { * Envelope Loop End Point * }
        EnvPoints    : Array[0..11] Of ISS_TEnvPoint; { * Envelope Points * }
       End;

      { * Internal Instrument Format * }
      ISS_TInstrument = Record
        IName : Array[0..31] Of Char; { * Instrument Name * }

        INoteTable : Array[1..96] Of Byte; { * Sample Number for all notes * }

        IVolumeEnv  : ISS_TEnvelope; { * Volume Envelope Data * }
        IPanningEnv : ISS_TEnvelope; { * Panning Envelope Data * }

        IVibType    : Byte; { * Vibrato Type * }
        IVibSweep   : Byte; { * Vibrato Sweep * }
        IVibDepth   : Byte; { * Vibrato Depth * }
        IVibRate    : Byte; { * Vibrato Rate * }

        IVolFadeOut : Word; { * Volume FadeOut * }

        ISampleNum : Word; { * Number Of Samples in the Instrument * }
        ISamples   : Array[0..15] Of ISS_PSample; { * Pointer to Sample * }
       End;
      ISS_PInstrument = ^ISS_TInstrument;

      { * Internal Module Format Header * }
      ISS_TModule = Record
        MID      : Array[0..3] Of Char; { * Module ID. See the const above * }
        MTitle   : String[32]; { * Title of the Module * }
        MStatus  : Word; { * Status bits of the module * }

        MFlags   : Word; { * Module Flags * }
        MTracker : Word; { * Original Tracker (File Format) * }

        MChannels   : Word; { * Number of Channels in the Module * }
        MPatternNum : Word; { * Number of Patterns in the Module * }
        MSampleNum  : Word; { * Number of Samples in the Module * }
        MInstrNum   : Word; { * Number of Instruments in the Module * }

        MSongLength : Word; { * Song Length (Orders Num) * }
        MRestart    : Word; { * Song Restart Position (Order Num) * }
        MOrders     : Array[0..255] Of Byte; { * Pattern Order Table * }
        MPatterns   : Array[0..255] Of ISS_PPattern; { * Ptrs to patterns * }

        MTempo   : Byte; { * Default Speed * }
        MBPM     : Byte; { * Default Tempo (BPM) * }

        { * Pointers to instruments * }
        MInstruments : Array[1..128] Of ISS_PInstrument;

       End;
      ISS_PModule = ^ISS_TModule;


      { * >>> V I R T U A L  S O U N D  D E V I C E <<< * }

      { * A Virtual Device Channel * }
      ISS_TVirtualChannel = Record
        VChControl : DWord;  { * Device Controller Bits (See flags above) * }

        VChFreq    : DWord; { * Frequency for this device channel * }
        VChPeriod  : DWord; { * Note Period * }

        VChSmpAddr : ISS_PSample; { * Sample Structure Address * }
        VChInsAddr : ISS_PInstrument; { * Instrument Structure Address * }
        VChSmpOffs : DWord; { * Sample Start Offset (FX:SetSampOffs) * }

        VChVolume      : Byte;  { * Virtual Channel Volume (0-64) * }
        VChFinalVolume : Byte;  { * Final Volume (after instr+etc) (0-64) * }
        VChMute        : Boolean; { * Force Volume 0 on the channel * }

        VChPanning      : Byte; { * Virtual Channel Panning * }
        VChFinalPanning : Byte; { * Final Panning (after instr+etc) * }
        VChForceMono    : Boolean; { * Disable Panning * }

        VChFadeOutVolume : Word; { * Envelope : Volume Fadeout * }
        VChEnvVolume     : Byte; { * Envelope : Volume (0-64) * }
        VChEnvVolPoint   : Byte; { * Envelope : Current Vol Env Point * }
        VChEnvVolPos     : Word; { * Envelope : Current Vol Env Position * }

        VChEnvPanning    : Byte; { * Envelope : Panning (0-32) * }
        VChEnvPanPoint   : Byte; { * Envelope : Current Pan Env Point * }
        VChEnvPanPos     : Word; { * Envelope : Current Pan Env Position * }

        VChAVibPos       : Byte;  { * Autovibrato : Position * }
        VChAVibSwpPos    : Byte; { * Autovibrato : Sweep Position * }
        VChAVibPitch     : Integer; { * Autovibrato : Final pitch * }

        { * Debug Variables. Will be removed soon. * }
        VChDebug1  : DWord;
        VChDebug2  : DWord;
        VChDebug3  : DWord;

       End;

      { * Virtual Channels For Devices * }
      ISS_TVirtualChannels = Array[0..ISS_MaxSSChannels-1]
                             Of ISS_TVirtualChannel;
      ISS_PVirtualChannels = ^ISS_TVirtualChannels;

      { * >>> S Y S T E M  S T R U C T U R E S <<< * }

      { * Low-Level Loader Routines * }
      ISS_TModuleLoader = Record
        { * Variables * }
        ModuleMem  : Pointer; { * Pointer to the current 'raw' module * }
        ModulePtr  : ISS_PModule; { * Pointer to the loaded module * }
        ErrorCode  : DWord;  { * The last error code * }
        { * Procedures * }
        DebugInit  : Procedure; { * Inits the loader debug routines* }
        DebugDone  : Procedure; { * Shuts down the loader debug routines * }
        { * Returns true if the module is possible to load with the loader * }
        CheckModule     : Function : Boolean;
        LoadHeader      : Function : Boolean; { * Loads the header * }
        LoadPatterns    : Function : Boolean; { * Loads the patterns * }
        LoadInstruments : Function : Boolean; { * Loads the instruments * }
       End;
      ISS_PModuleLoader = ^ISS_TModuleLoader;

      { * Low-Level Device Routines * }
      ISS_TSoundDriver = Record { * Pointers for the player * }
        Detect     : Function : Boolean; { * True if device is available * }
        Init       : Function : Boolean; { * True if device init success * }
        Done       : Function : Boolean; { * True if device close success * }
        LoadSample : Function(SStruc : ISS_PSample) : Boolean;
        FreeSample : Function(SStruc : ISS_PSample) : Boolean;
        SetVolume  : Function(Volume : DWord) : Boolean;
        StartOut   : Function(PeriodicCall  : Pointer) : Boolean;
        StopOut    : Function(PeriodicCall : Pointer) : Boolean;
        UpdateOut  : Procedure; { * Updates the sound output * }
       End;
      ISS_PSoundDriver = ^ISS_TSoundDriver;

      { * Device Parameter Record * }
      ISS_TSoundDevice = Record { * Contains standard device-handling * }
        DevDriver   : ISS_PSoundDriver; { * Device handler procedures * }
        DevAvail    : Boolean; { * True if device is available * }
        DevName     : String; { * Name of the device * }
        DevHWVer    : String; { * Hardware version string (eg. SB DSP ver) * }
        DevSWVer    : String; { * Software version string (eg. OS/2 driver version) * }
        DevType     : Word;   { * Type of the device (see device flags) * }
        DevBaseport : Word;   { * Baseport * }
        DevIRQ      : Byte;   { * IRQ number * }
        DevDMA1     : Byte;   { * DMA channel 1 * }
        DevDMA2     : Byte;   { * DMA channel 2 * }
        DevDRAMSize : DWord;  { * Size of On-board memory in bytes * }
        DevFreq     : Word;   { * Maximum available playing frequency * }
        DevMaxChan  : Word;   { * Maximum number of channels available * }
        DevMixRate  : Word;   { * The mixrate the device currently using * }
        DevMode     : Word;   { * Mode the device using (devtype flags!) * }
       End;
      ISS_PSoundDevice = ^ISS_TSoundDevice;

Var ISS_ErrorCode : DWord;  { * Contains Latest Error Number * }

    { * Difference between the timer ticks, can be used for player sync * }
    ISS_TimerDiff : DWord;

    ISS_TimerFreq : DWord;

    { * Virtual Sound Device * }
    ISS_VirtualChannels : ISS_PVirtualChannels;

    { * The module currently playing * }
    ISS_CurrentModule : ISS_PModule;

    { * Active channel numbers * }
    ISS_ActiveSSChannels : Word;
    ISS_ActivePlChannels : Word;

    { * Global Volumes * }
    ISS_GlobalSSVolume : Byte; { * System Volume * }
    ISS_GlobalPlVolume : Byte; { * Player Volume * }

Implementation

Begin
 { * Setting variable defaults * }
 ISS_ActiveSSChannels:=0;
 ISS_ActivePlChannels:=0;
End.
