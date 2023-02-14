unit DeclareFun;

interface

uses DeclareGlobalVal;

function dsoHTSearchDevice(var DevInfo: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTSearchDevice';
function dsoHTSetSampleRate(nDeviceIndex: Smallint; nYTFormat: Smallint;
  var pRelayControl: RelayControl; var pSTControl: CONTROLDATA): Smallint;
  stdcall; external 'HTHardDll.dll' name 'dsoHTSetSampleRate';
function dsoHTSetCHAndTrigger(nDeviceIndex: Smallint;
  var rcRelayControl: RelayControl; nTimeDiv: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTSetCHAndTrigger';
// function dsoHTSetHTriggerLength(nDeviceIndex: Smallint; nBufferLen: Longint; HTriggerPos: Smallint; nTimeDiv: Smallint; nYTFormat: Smallint): Smallint; stdcall; external 'HTHardDll.dll' name 'dsoHTSetHTriggerLength';
function dsoHTGetState(nDeviceIndex: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTGetState';
// function dsoHTSetCHAndTriggerVB(nDeviceIndex: Smallint; var CHEnable: Smallint; var CHVoltDIV: Smallint; var CHCoupling: Smallint; var CHBWLimit: Smallint; nTriggerSource: Smallint; nTriggerFilt: Smallint; nALT: Smallint): Smallint; stdcall; external 'HTHardDll.dll' name 'dsoHTSetCHAndTriggerVB';
function dsoHTGetData(nDeviceInder: Smallint; var CH1Data: Smallint;
  var CH2Data: Smallint; var CH3Data: Smallint; var CH4Data: Smallint;
  var pSTControl: CONTROLDATA): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTGetData';
function dsoHTStartCollectData(nDeviceIndex: Smallint; startcontrol: Longint)
  : Smallint; stdcall; external 'HTHardDll.dll' name 'dsoHTStartCollectData';
function dsoHTSetTrigerMode(nDeviceIndex: Smallint; TriggerMode: Smallint;
  Slop: Smallint; TriCouple: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTSetTrigerMode';
function dsoHTSetVTriggerLevel(nDeviceIndex: Smallint; nPos: Smallint;
  chmod: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTSetVTriggerLevel';
function dsoHTSetCHPos(nDeviceInder: Smallint; CHVoltDIV: Smallint;
  CHPos: Smallint; CH: Smallint; chmod: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoHTSetCHPos';
function dsoInitHard(nDeviceInder: Smallint): Smallint; stdcall;
  external 'HTHardDll.dll' name 'dsoInitHard';
function dsoHTADCCHModGain(nDeviceInder: Smallint; nCHMod: Smallint): Smallint;
  stdcall; external 'HTHardDll.dll' name 'dsoHTADCCHModGain';
function dsoHTSetRamAndTrigerControl(DeviceIndex: Smallint; TimeDIV: Smallint;
  CHSet: Smallint; nTriggerSource: Smallint; isPeak: Smallint): Smallint;
  stdcall; external 'HTHardDll.dll' name 'dsoHTSetRamAndTrigerControl';
function HTDrawGrid(hDC: Longint; nLeft: Longint; nTop: Longint;
  nRight: Longint; nBottom: Longint; nHoriGridNum: Longint;
  nVertGridNum: Longint; nBright: Longint; IsGrid: Longint): Longint; stdcall;
  external 'HTDisplayDll.dll' name 'HTDrawGrid';
function HTDrawWaveInYTVB(hDC: Longint; Left: Longint; Top: Longint;
  Right: Longint; Bottom: Longint; R: Smallint; G: Smallint; B: Smallint;
  nDisTye: Smallint; var pData: Smallint; nLen: Longint; nDisLen: Longint;
  CenterData: Longint; nDisLeverPos: Smallint; Horizontal: Double;
  Vertical: Double; YTFormat: Smallint; AlreadLen: Longint): Longint; stdcall;
  external 'HTDisplayDll.dll' name 'HTDrawWaveInYTVB';
function CalAmplitude(pMaxData: shortint; pMinData: shortint; dbVoltDIV: real;
  nMaxData: shortint): real; stdcall;
  external 'MeasDLL.dll' name 'CalAmplitude';

implementation

// ==============================================================

end.
