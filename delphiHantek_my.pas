unit ComMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, Vcl.ComCtrls, ShellApi, DeclareGlobalVal,
  DeclareFun, Math;

type
  THistoData = array [0 .. 1, 0 .. 255] of byte;

  TForm1 = class(TForm)
    DisplayPicture: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;

    procedure AllOFF;
    procedure SwapTorquer;
    procedure Delay(msec: Longint);
    procedure Button_OpenClick(Sender: TObject);
    procedure Button_SettingsClick(Sender: TObject);
    procedure Button_SendClick(Sender: TObject);
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ProverkaSostoyaniy;
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AppMessage(var Msg: TMsg; var handled: Boolean);
    procedure InitializeVariables();
    procedure InitHard();
    procedure CollectData();
    procedure ReadData();
    procedure ClearCanvas(C: TCanvas; col: TColor = clWhite);
    procedure GetDataLoopTimer(Sender: TObject);
    procedure DrawGrid();
    procedure Main();
    procedure instruction();
    procedure WaitNeutral();
    procedure CheckRPM_UT_High();
    procedure waitBias();
    procedure CheckRPM_UTLT_Low();
    procedure InputVoltage(Voltage: real);
    procedure RecCurrentTrans(Current: real);
    procedure InputIncGtot();
    procedure InputPeackPeack();
    procedure CheckRPM_LT_High();
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ParrentWindow();
    procedure WaitTemp();
    procedure Button10Click(Sender: TObject);

  public
    { Public declarations }
    StateUT, StateLt: Integer;
    OldStateUT, OldStateLt: Integer;
    Temperature, GotovnostFlag, tempFlag, IncFlag: Integer;
    Inclination, GTotal: real;
    ParrentWnd, wndInternal, wndInternalT, wndInternalF, wndIntOK: hwnd;
    incStr, GtotStr: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function EnumChildren(hwnd: hwnd; lParam: lParam): BOOL; stdcall;
const
  TextBoxClass = 'Edit';
var
  ClassName: array [0 .. 259] of Char;
begin
  Result := True;
  GetClassName(hwnd, ClassName, Length(ClassName));
  if ClassName = TextBoxClass then
    TStrings(lParam).Add(IntToHex(hwnd, 8));
end;

(*
  // -------------------------------------------------------------------------
  function GetTreeViewText(hTVwnd: hwnd; memo1, memo2: TMemo): pchar;
  const
  MAX_TEXT = 256;
  type
  TVITEM = TTVITEM;
  var
  lpBuf: array [0 .. MAX_TEXT - 1] of Char;
  tvi: TVITEM;

  dwPId: DWORD;
  hProc: THANDLE;
  pText, pAddr: pointer;
  dwTmp: DWORD;
  i, i2, nCount, size: Integer;
  chan: Integer;
  is_chan: boolean;
  // ret, ret_usr, res: TStringList;
  begin
  // Get ThreadID and ProzessID
  if BOOL(GetWindowThreadProcessId(hTVwnd, @dwPId)) then
  // Get a handle to the process with the needed access
  hProc := OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_READ or
  PROCESS_VM_WRITE, FALSE, dwPId);
  if hProc <> 0 then
  begin
  // Allocate memory for thet text to hold in the target process ...
  pText := VirtualAllocEx(hProc, nil, MAX_TEXT, MEM_RESERVE or MEM_COMMIT,
  PAGE_READWRITE);
  // Allocate memory for the TVITEM structure in target process ...
  pAddr:
  = VirtualAllocEx(hProc, nil, sizeof(tvi), MEM_RESERVE or MEM_COMMIT,
  PAGE_READWRITE);
  // Get number of nodes in the target treeview ...
  nCount:
  = SendMessage(hTVwnd, TVM_GETCOUNT, 0, 0);
  // We want the text only ... set some flags in structure
  tvi.mask: = TVIF_TEXT;
  tvi.pszText: = pText;
  tvi.cchTextMax: = MAX_TEXT - 1;

  // Get root item handle ...
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM, TVGN_ROOT, 0));
  // Iterate through the whole treeview and collapse all root child items
  // (to avoid flickering and stuff)
  for i: = 0 to nCount - 1 do
  begin
  // Write our structure (with flags) to the process
  if WriteProcessMemory(hProc, pAddr, @tvi, sizeof(tvi), dwTmp) and
  // Get the stuff we need back
  (SendMessage(hTVwnd, TVM_GETITEM, 0, Integer(pAddr)) <> 0) and
  // Read the text from the pointer we got ... respect MAX_TEXT
  ReadProcessMemory(hProc, pText, @lpBuf, MAX_TEXT, dwTmp) then
  begin
  // If successful and not root item, collapse it.
  if (i > 0) then
  // Collapse the item ... next visible item will be a child item of the root, too
  SendMessage(hTVwnd, TVM_EXPAND, TVE_COLLAPSE, Integer(tvi.hItem));
  // Get handle to next visible item ...
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM,
  TVGN_NEXTVISIBLE, Integer(tvi.hItem)));
  // Break here if we face the last visible item ...
  if tvi.hItem = HTREEITEM(0) then
  break;
  end
  // If not successful break here ...
  else
  break;
  end;
  // Get root item again
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM, TVGN_ROOT, 0));
  // ... and iterate through the treeview once more ...
  for i: = 0 to (nCount - 1) do
  begin
  // Write our structure (with flags) to the process
  if WriteProcessMemory(hProc, pAddr, @tvi, sizeof(TV_ITEM), dwTmp) and
  // Get the stuff we need back
  (SendMessage(hTVwnd, TVM_GETITEM, 0, Integer(pAddr)) <> 0) and
  // Read the text from the pointer we got ... respect MAX_TEXT
  ReadProcessMemory(hProc, pText, @lpBuf, MAX_TEXT, dwTmp) then
  begin
  // If successful and not root item, get text .. .
  if (i > 0) then
  memo1.Lines.Append(string(lpBuf));
  // Get handle to next visible item ...
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM,
  TVGN_NEXTVISIBLE, Integer(tvi.hItem)));
  end
  else
  break;
  end;
  // Get root item again
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM, TVGN_ROOT, 0));
  // ... and iterate through the treeview once more ...
  for i: = 0 to (nCount - 1) do
  begin
  if WriteProcessMemory(hProc, pAddr, @tvi, sizeof(TV_ITEM), dwTmp) and
  (SendMessage(hTVwnd, TVM_GETITEM, 0, Integer(pAddr)) <> 0) and
  ReadProcessMemory(hProc, pText, @lpBuf, MAX_TEXT, dwTmp) then
  begin
  if (i > 0) then
  SendMessage(hTVwnd, TVM_EXPAND, TVE_EXPAND, Integer(tvi.hItem));
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM,
  TVGN_NEXTVISIBLE, Integer(tvi.hItem)));
  end
  else
  break;
  end;
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM, TVGN_ROOT, 0));
  for i: = 0 to (nCount - 1) do
  begin
  if WriteProcessMemory(hProc, pAddr, @tvi, sizeof(TV_ITEM), dwTmp) and
  (SendMessage(hTVwnd, TVM_GETITEM, 0, Integer(pAddr)) <> 0) and
  ReadProcessMemory(hProc, pText, @lpBuf, MAX_TEXT, dwTmp) then
  begin
  if (i > 0) then
  memo2.Lines.Append(string(lpBuf));
  tvi.hItem: = HTREEITEM(SendMessage(hTVwnd, TVM_GETNEXTITEM,
  TVGN_NEXTVISIBLE, Integer(tvi.hItem)));
  end
  else
  break;
  end;
  end; // else: "could not get process handle"

  if Assigned(pAddr) then
  VirtualFreeEx(hProc, pAddr, 0, MEM_RELEASE);
  if Assigned(pText) then
  VirtualFreeEx(hProc, pText, 0, MEM_RELEASE);
  CloseHandle(hProc);
  end;
  [ / code]

  [code]
  function RetrieveText(target: TMemo; hwnd: hwnd; wndclass: string): BOOL;
  (*
  - target is clear ... just the memo into which to output stuff
  - trgtwnd is the target's window handle
  - wndclass is the window class. this should be determined using
  GetWindowClass (). It's just to distinguish the types because
  listboxes are treated differently from treeviews or listviews.

  NOTE: this function ignores any errors:
  - the target could delete items meanwhile!
  - the target could replace a short item by a long one which will
  result in our buffer being too small -> buffer overflow.
*)
{ var
  err, num, // number of items
  i, // current index
  l: Integer; // length
  hProcess: THANDLE;
  dwprocID: DWORD;
  szText: pchar;
  begin
  if IsWindow(hwnd) then
  (*
  begin
  GetWindowThreadProcessId (hWnd, @dwprocID);
  hProcess: = Open Process (PROCESS_VM_OPERATION or PROCESS_VM_READ {or PROCESS_VM_WRITE, FALSE, dwprocID);
  end;
  if hProcess <> 0 then
  *)
  try
  if (lstrcmpi(@wndclass[1], 'Listbox') = 0) or
  (lstrcmpi(@wndclass[1], 'TListbox') = 0) then
  (*
  This check should be extended for more window classes ... also
  MFC class names need to be included.
  Although some windows are wrapped by classes, they ultimately
  can be controlled by the standard Win32 API.
  *)
  begin
  // Check wether the listbox hold strings (else it could be any data format ...
  if (GetWindowLong(hwnd, GWL_STYLE) and LBS_HASSTRINGS) <> 0 then
  begin
  // First get the number of entries in the listbox
  num:
  = SendMessage(hwnd, LB_GETCOUNT, 0, 0);
  // Only proceed if no error
  if num <> LB_ERR then
  for i: = 0 to num - 1 do
  begin
  // Get size of item in TChars and calculate the size needed in AnsiChars ...
  l:
  = (SendMessage(hwnd, LB_GETTEXTLEN, i, 0) + 1);
  // Allocate the mem in target process
  szText:
  = GetMemory(l);
  // VirtualAllocEx (hProcess, nil, l, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
  // Check for error
  if Assigned(szText) then
  try
  err:
  = SendMessage(hwnd, LB_GETTEXT, i, Integer(szText));
  if err <> LB_ERR then
  begin
  // Add to target memo
  if target.Text = '' then
  target.Text: = string(szText)
  else
  target.Text: = target.Text + # 13 # 10 + string(szText);
  end
  else; // Drop error message ...
  finally
  // Free the memory in the target process
  Freememory(szText);
  // VirtualFreeEx (hProcess, szText, 0, MEM_RELEASE);
  end;
  end;
  end;
  end;
  finally
  //
  finally
  close the handle // CloseHandle (hProcess);
  end
  else
  begin
  // Drop error message and fail ...
  exit;
  end;
  end;
}
// ------------------------------------------------------------------------



procedure TForm1.AppMessage(var Msg: TMsg; var handled: Boolean);
begin
  if (Msg.Message = WM_SYSCOMMAND) and (Msg.wParam = SC_SCREENSAVE) then
    handled := True;
end;


procedure TForm1.Button10Click(Sender: TObject);
begin
  ParrentWindow();
  Delay(200);
  WaitTemp();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnMessage := AppMessage;
  Main();
end;

procedure TForm1.GetDataLoopTimer(Sender: TObject);
var
  Result, i: Longint;
  Grid: RECT;
  PPromMin, PPromMax, P1Min, P1Max, P2Min, P2Max, P3Min, P3Max: Smallint;
begin
  // Button3.Enabled:=false;

  Grid.Left := 15;
  Grid.Top := 15;

  Grid.Right := DisplayPicture.Width - 15;
  Grid.Bottom := DisplayPicture.Height - 15;
  DrawGrid();
  CollectData();
  Result := HTDrawWaveInYTVB(DisplayPicture.Canvas.Handle, 15, 15,
    DisplayPicture.Width - 15, DisplayPicture.Height - 15, 255, 255, 0, 0,
    CH1SrcData[0], stControl.nReadDataLen, DisLen,
    Round(stControl.nReadDataLen / 2), LeverPos[0], 1.0, 1.0, 0,
    stControl.nAlreadyReadLen);
  Result := HTDrawWaveInYTVB(DisplayPicture.Canvas.Handle, 15, 15,
    DisplayPicture.Width - 15, DisplayPicture.Height - 15, 0, 255, 255, 0,
    CH2SrcData[0], stControl.nReadDataLen, DisLen,
    Round(stControl.nReadDataLen / 2), LeverPos[1], 1.0, 1.0, 0,
    stControl.nAlreadyReadLen);
  Result := HTDrawWaveInYTVB(DisplayPicture.Canvas.Handle, 15, 15 + 125,
    DisplayPicture.Width - 15, DisplayPicture.Height - 15 + 125, 255, 0, 255, 0,
    CH3SrcData[0], stControl.nReadDataLen, DisLen,
    Round(stControl.nReadDataLen / 2), LeverPos[2], 1.0, 1.0, 0,
    stControl.nAlreadyReadLen);
  Result := HTDrawWaveInYTVB(DisplayPicture.Canvas.Handle, 15, 15 + 50,
    DisplayPicture.Width - 15, DisplayPicture.Height - 15 + 50, 0, 255, 0, 0,
    CH4SrcData[0], stControl.nReadDataLen, DisLen,
    Round(stControl.nReadDataLen / 2), LeverPos[3], 1.0, 1.0, 0,
    stControl.nAlreadyReadLen);
  // HTDrawWaveInYTVB(HDC hDC,int left,int top,int right, int bottom, USHORT R, USHORT G,
  // USHORT B,USHORT nDisType,short* pSrcData,ULONG nSrcDataLen,ULONG nDisDataLen,ULONG nCenterData,
  // USHORT nDisLeverPos,double dbHorizontal,double dbVertical,USHORT nYTFormat,ULONG nScanLen);
  // HTDrawWaveInYT(HDC hDC,RECT Rect,COLORREF clrRGB,USHORT nDisType,short* pSrcData,ULONG nSrcDataLen,ULONG nDisDataLen,ULONG nCenterData,USHORT nDisLeverPos,double dbHorizontal,double dbVertical,USHORT nYTFormat,ULONG nScanLen);

  // HTDrawWaveInYT(pDC->GetSafeHdc(),Rect,clrRGB,nDisType,pData,nSrcDataLen,nDisDataLen,nCenterData,nDisLeverPos,dbHorizontal,dbVertical,nYTFormat,m_pDoc->m_Hard.m_stControl.nAlreadyReadLen);//DLL import
  // function CalAmplitude( pMaxData: shortint; pMinData: shortint; dbVoltDIV: real; nMaxData: shortint): real; stdcall; external 'MeasDLL.dll' name 'CalAmplitude';

  PPromMin := CH4SrcData[0];
  PPromMax := CH4SrcData[0];
  for i := 0 to stControl.nReadDataLen - 1 do
  begin
    if CH4SrcData[i] < PPromMin then
      PPromMin := CH4SrcData[i];
    if CH4SrcData[i] > PPromMax then
      PPromMax := CH4SrcData[i];
  end;
  P1Min := PPromMin;
  P1Max := PPromMax;

  PPromMin := CH2SrcData[0];
  PPromMax := CH2SrcData[0];
  for i := 0 to stControl.nReadDataLen - 1 do
  begin
    if CH2SrcData[i] < PPromMin then
      PPromMin := CH2SrcData[i];
    if CH2SrcData[i] > PPromMax then
      PPromMax := CH2SrcData[i];
  end;
  P2Min := PPromMin;
  P2Max := PPromMax;

  PPromMin := CH3SrcData[0];
  PPromMax := CH3SrcData[0];
  for i := 0 to stControl.nReadDataLen - 1 do
  begin
    if CH3SrcData[i] < PPromMin then
      PPromMin := CH3SrcData[i];
    if CH3SrcData[i] > PPromMax then
      PPromMax := CH3SrcData[i];
  end;
  P3Min := PPromMin;
  P3Max := PPromMax;

  if P1Min < 0 then
    edit1.Text := floattostrF(((P1Max + ABS(P1Min)) / 1.7) / 2,
      ffFixed, 0, 2)
  else
    edit1.Text := floattostrF(((P1Max - P1Min) / 1.7) / 2, ffFixed, 0, 2);

  if P2Min < 0 then
    edit2.Text := floattostrF(((P2Max + ABS(P2Min)) / 1.7) / 2,
      ffFixed, 0, 2)
  else
    edit2.Text := floattostrF(((P2Max - P2Min) / 1.7) / 2, ffFixed, 0, 2);

  if P3Min < 0 then
    edit3.Text := floattostrF(((P3Max + ABS(P3Min)) / 1.7) / 2,
      ffFixed, 0, 2)
  else
    edit3.Text := floattostrF(((P3Max - P3Min) / 1.7) / 2, ffFixed, 0, 2);

end;


procedure TForm1.InitializeVariables();
var
  ForceTriggerCnt: Smallint;
  i: Longint;
begin

  DeviceNum := 0;
  DeviceIndex := 0; // Йи±ёЛчТэЦµ
  LeverPos[0] := 0;
  LeverPos[1] := 0;
  LeverPos[2] := 0;
  LeverPos[3] := 0;
  TimeDIV := 21; // К±»щЛчТэЦµ    //14
  YTFormat := 0; // YTДЈКЅ
  stControl.nCHSet := 7; // 15; // 16ЅшЦЖПВКЗ0x0F±нКѕ4ёцНЁµАИ«ІїґтїЄ
  stControl.nTimeDiv := TimeDIV; // The index of time base 0 35.
  stControl.nTriggerSource := 0; // Trigger Source 0 3
  stControl.nHTriggerPos := 0; // Л®ЖЅґҐ·ўО»ЦГ
  stControl.nVTriggerPos := LeverPos[0]; // ґ№Ц±ґҐ·ўО»ЦГ
  stControl.nTriggerSlope := 0; // ТФЙПЙэСШЧчОЄґҐ·ў·ЅКЅ
  stControl.nBufferLen := 4096; // ІЙјЇКэѕЭµДі¤¶И
  stControl.nReadDataLen := 4096; // ¶БИЎКэѕЭµДі¤¶И
  stControl.nAlreadyReadLen := 0; // ТСѕ­¶БИЎµДі¤¶ИЈ¬ЅцФЪЙЁГи№ц¶ЇК№УГ
  DisLen := 2500; // ЖБД»ПФКѕЧЬµгКэ
  stControl.nALT := 0; // КЗ·сЅ»МжґҐ·ўЧўТвЅ»МжґҐ·ўКЗИнјю№¦ДЬ
  for i := 0 to 3 do
  begin
    rcRelayControl.bCHEnable[i] := 1; // їЄЖфНЁµА
    rcRelayControl.nCHVoltDIV[i] := 8; // ЙиЦГµзС№µµО»
    rcRelayControl.nCHCoupling[i] := 0; // ЙијЖсоєПДЈКЅ AC/DC
    rcRelayControl.bCHBWLimit[i] := 0; // КЗ·сїЄЖф20MВЛІЁ
  end; // i
  rcRelayControl.nTrigSource := stControl.nTriggerSource;
  rcRelayControl.bTrigFilt := 0;
  rcRelayControl.nALT := stControl.nALT;
  TriggerMode := 0; // ±ЯСШґҐ·ў
  TriggerSlope := 0; // ±ЯСШґҐ·ўµДґҐ·ўФґ
  TriggerSweep := 0; // ЧФ¶ЇґҐ·ў
  ReadOK := 0;
  StartNew := True;
  ForceTriggerCnt := 0;
  Collect := 1;
  for i := 0 to 578 do
  begin
    pAmpLevel[i] := 1024; // ёшУІјюПВ1024Ј¬КµјКЙП1024±нКѕ1.0Ј¬јґІ»ЧцРЮХэ
  end; // i

end;

procedure TForm1.InitHard();
var
  Result: Longint;
  i, nVolt: Smallint;
begin

  DeviceIndex := 0;
  Result := dsoInitHard(DeviceIndex); // УІјюБ¬ЅУєуµчУГµДµЪ¶юёцєЇКэ
  Result := dsoHTADCCHModGain(DeviceIndex, 4); // ЙиЦГДЈДв¶Л·щ¶ИРЮХэ
  Result := dsoHTSetSampleRate(DeviceIndex, YTFormat, rcRelayControl,
    stControl); // ЙиЦГІЙСщВК
  Result := dsoHTSetCHAndTrigger(DeviceIndex, rcRelayControl,
    stControl.nTimeDiv); // ЙиЦГНЁµАїЄ№ШєНµзС№µµО»
  Result := dsoHTSetRamAndTrigerControl(DeviceIndex, (stControl.nTimeDiv),
    (stControl.nCHSet), (stControl.nTriggerSource), 0); // ЙиЦГґҐ·ўФґ
  for i := 0 to 3 do
  begin
    Result := dsoHTSetCHPos(DeviceIndex, rcRelayControl.nCHVoltDIV[i],
      LeverPos[i], i, 4); // ЙиЦГНЁµАґ№Ц±О»ЦГ
  end; // i

  Result := dsoHTSetVTriggerLevel(DeviceIndex, LeverPos[0], 4);
  // ЙиЦГґҐ·ўґ№Ц±О»ЦГУлНЁµА1ПаН¬
  if TriggerMode = 0 then
  begin // EDGE
    Result := dsoHTSetTrigerMode(DeviceIndex, TriggerMode,
      stControl.nTriggerSlope, 0); // Из№ыКЗ±ЯСШґҐ·ўµчУГґЛєЇКэ
  end;

end;

procedure TForm1.CollectData();
var
  nState, Result: Longint;
  nStartControl: Smallint;
begin

  if (StartNew) then
  begin
    nStartControl := 0;
    nStartControl := nStartControl + IfThen(TriggerSweep = 0, 1, 0);
    //nStartControl := nStartControl + IfThen(TriggerSweep = 0, 1, 0);
    nStartControl := nStartControl + IfThen(YTFormat = 0, 0, 2);
    //nStartControl := nStartControl + IfThen(YTFormat = 0, 0, 2);
    nStartControl := nStartControl + IfThen(Collect = 1, 0, 4);
    //nStartControl := nStartControl + IfThen(Collect = 1, 0, 4);
    Result := dsoHTStartCollectData(DeviceIndex, nStartControl);
    StartNew := FALSE;
  end;
  nState := dsoHTGetState(DeviceIndex);
  if (nState and 2) = 2 then
  begin // ІЙјЇЅбКшІаїЄКј¶БКэѕЭ
    ReadData();
    StartNew := True;
  end
  else
  begin

    StartNew := FALSE;

  end;

end;

procedure TForm1.ReadData();
var
  i: Longint;
  Result: Smallint;
  CH1ReadData: array [0 .. 4096] of Smallint;
  CH2ReadData: array [0 .. 4096] of Smallint;
  CH3ReadData: array [0 .. 4096] of Smallint;
  CH4ReadData: array [0 .. 4096] of Smallint;
begin

  Result := dsoHTGetData(DeviceIndex, CH1ReadData[0], CH2ReadData[0],
    CH3ReadData[0], CH4ReadData[0], stControl);
  // ГїНЁµА1µЪjёцµгµДКµјКµзС№Цµ=(CH1ReadData[j]-LeverPos(0))*8*µзС№Цµ/255
  if Result = 1 then
  begin
    for i := 0 to stControl.nReadDataLen - 1 do
    begin
      CH1SrcData[i] := CH1ReadData[i] - (255 - LeverPos[0]);
      CH2SrcData[i] := CH2ReadData[i] - (255 - LeverPos[1]);
      CH3SrcData[i] := CH3ReadData[i] - (255 - LeverPos[2]);
      CH4SrcData[i] := CH4ReadData[i] - (255 - LeverPos[3]);
    end; // i
  end;
end;

procedure TForm1.DrawGrid();
var
  Result: Longint;
begin

  ClearCanvas(DisplayPicture.Canvas);
  Result := HTDrawGrid(DisplayPicture.Canvas.Handle, 15, 15,
    DisplayPicture.Width - 15, DisplayPicture.Height - 15, 10, 8, 200, 1);

end;

procedure TForm1.Main();
var
  DevInfo: array [0 .. 63] of Smallint;
  Result: Longint;

begin
   {
  InitializeVariables(); // іхКј»Ї±дБї
  DeviceNum := dsoHTSearchDevice(DevInfo[0]);
  if DeviceNum = 0 then
  begin
    ShowMessage('DSO not found!');
    Application.Terminate();
  end;
  InitHard(); // іхКј»ЇУІјю
  // TForm1.Visible := True;
  GetDataLoop.Enabled := True;
   }

end;

procedure TForm1.ClearCanvas(C: TCanvas; col: TColor = clWhite);
var
  prevBrushStyle: TBrushStyle;
  prevBrushColor: TColor;
begin
  prevBrushStyle := C.Brush.Style;
  prevBrushColor := C.Brush.Color;
  C.Brush.Style := bsSolid;
  C.Brush.Color := col;
  C.FillRect(C.ClipRect);
  C.Brush.Style := prevBrushStyle;
  C.Brush.Color := prevBrushColor;
end;


end.