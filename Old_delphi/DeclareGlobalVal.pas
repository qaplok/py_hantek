unit DeclareGlobalVal;

interface

// uses ...;


//

type
  RelayControl = record // Ïê¼ûSDK_HTHardDll_CHS.pdf
    bCHEnable: array [0 .. 3] of Longint;
    nCHVoltDIV: array [0 .. 3] of Smallint;
    nCHCoupling: array [0 .. 3] of Longint;
    bCHBWLimit: array [0 .. 3] of Smallint;
    nTrigSource: Smallint;
    bTrigFilt: Longint;
    nALT: Smallint;

    // constructor Create();
  end;
  { constructor RelayControl.Create();
    begin
    nTrigSource:=0;  bTrigFilt:=0;  nALT:=0;
    end; }

type
  CONTROLDATA = record // SDK_HTSoftDll_CHS.pdf

    nCHSet: Smallint;
    nTimeDiv: Smallint;
    nTriggerSource: Smallint;
    nHTriggerPos: Smallint;
    nVTriggerPos: Smallint;
    nTriggerSlope: Smallint;
    nBufferLen: Longint;
    nReadDataLen: Longint;
    nAlreadyReadLen: Longint;
    nALT: Smallint;
    nETSOpen: Smallint;
    nDriverCode: Smallint;
    nLastAddress: Longint;

    // constructor Create();
  end;
  { constructor CONTROLDATA.Create();
    begin
    nCHSet:=0;  nTimeDiv:=0;  nTriggerSource:=0;  nHTriggerPos:=0;
    nVTriggerPos:=0;  nTriggerSlope:=0;  nBufferLen:=0;
    nReadDataLen:=0;  nAlreadyReadLen:=0;  nALT:=0;  nETSOpen:=0;
    nDriverCode:=0;  nLastAddress:=0;
    end; }

type
  COLORREF = record
    R: Smallint;
    G: Smallint;
    B: Smallint;

    // constructor Create();
  end;
  { constructor COLORREF.Create();
    begin
    R:=0;  G:=0;  B:=0;
    end; }

type
  RECT = record
    Left: Smallint;
    Top: Smallint;
    Right: Longint;
    Bottom: Longint;

    // constructor Create();
  end;
  { constructor RECT.Create();
    begin
    Left:=0;  Top:=0;  Right:=0;  Bottom:=0;
    end; }

var
  DeviceNum, ReadOK, DisLen: Longint;
  DeviceIndex, TimeDIV, YTFormat, TriggerMode, TriggerSweep, TriggerSlope,
    Collect: Smallint;
  CH1SrcData: array [0 .. 4096] of Smallint;
  CH2SrcData: array [0 .. 4096] of Smallint;
  CH3SrcData: array [0 .. 4096] of Smallint;
  CH4SrcData: array [0 .. 4096] of Smallint;
  CH1Color, CH2Color, CH3Color, CH4Color: COLORREF;
  CalLevel: array [0 .. 578] of Smallint;
  stControl: CONTROLDATA;
  rcRelayControl: RelayControl;
  LeverPos: array [0 .. 3] of Smallint;
  StartNew: Boolean;
  pAmpLevel: array [0 .. 578] of Smallint;

implementation

// ==============================================================

end.
