unit ObjInspect;

(* BEGIN LICENSE BLOCK *********************************************************
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is ObjInspect.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2009 the Initial Developer. All Rights Reserved.
 *
 * Version 1.2 (Last updated 3-Oct-09)
 *
 * END LICENSE BLOCK **********************************************************)

interface

{$WARN UNSAFE_TYPE OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ScrollingCtrl, StdCtrls, ExtCtrls, Math;

type
  TEditorType = (etReadOnly, etEdit, etCombo, etComboList, etEllipsis, etEllipsisRO);
  TObjInspectorBase = class;

  TScrollItemOI = class(TScrollItem) //object inspector item
  private
    FItems: TStrings;
  protected
    procedure Paint; override;
  public
    editType: TEditorType;
    propName: string;
    propValue: string;
    constructor Create; override;
    destructor Destroy; override;
    property ValueOptions: TStrings read FItems;
  end;

  TEllipsisEvent = procedure(Sender: TObject; item: TScrollItemOI) of object;
  TValidateEvent = procedure(Sender: TObject; var NewValue: string;
    item: TScrollItemOI; var IsValid: boolean) of object;

  TComboboxEx = class(TCombobox)
  private
    fValidateInChange: boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DblClick; override;
    procedure CreateWnd; override;
    function  GetItemHt: Integer; override;
    procedure Change; override;
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure DoEnter; override;
  public
    HeightDx: integer;
    ObjInspector: TObjInspectorBase;
  end;

  TObjInspectorBase = class(TScrollingCtrl)
  private
    FDividerOffset   : integer;
    FMinDividerOffset: integer;
    FValueLeftMargin : integer;
    FMovingDivider   : boolean;
    FComboCtrl       : TComboboxEx;
    FItemEditor      : TItemEditor;
    FBtnCtrl         : TButton;
    FEllipsisEvent   : TEllipsisEvent;
    FValidateEvent   : TValidateEvent;
  protected
    procedure EditorPaint(Sender: TObject);
    procedure EditorBeforeEnter(Sender: TObject);
    procedure EditorExit(Sender: TObject);
    procedure BtnCtrlClick(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoValidation;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property DividerOffset: integer read FDividerOffset write FDividerOffset;
    property MinDividerOffset: integer read FMinDividerOffset write FMinDividerOffset;
    property ValueLeftMargin: integer read FValueLeftMargin write FValueLeftMargin;
    property OnEllipsis: TEllipsisEvent read FEllipsisEvent write FEllipsisEvent;
    property OnValidateChange: TValidateEvent read FValidateEvent write FValidateEvent;
  end;

implementation

//------------------------------------------------------------------------------
// TScrollItemOI methods ...
//------------------------------------------------------------------------------

constructor TScrollItemOI.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;
//------------------------------------------------------------------------------

destructor TScrollItemOI.Destroy;
begin
  FItems.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollItemOI.Paint;
var
  divOff: integer;
  objInsp: TObjInspectorBase;
begin
  inherited Paint;
  objInsp := TObjInspectorBase(ScrollingCtrl);
  divOff := objInsp.DividerOffset;
  with objInsp.Canvas do
  begin
    //draw a dotted bottom edge ...
    pen.Color := clBtnHighlight;
    pen.Style := psDot;
    moveto(0, Top+Height);
    lineto(objInsp.ClientWidth, Top+Height);
    pen.Style := psSolid;

    //draw the column divider ...
    pen.Color := clBtnShadow;
    moveto(divOff, Top);
    lineto(divOff,Top +Height);
    pen.Color := clBtnHighlight;
    moveto(divOff+1, Top);
    lineto(divOff+1,Top +Height);
  end;
end;

//------------------------------------------------------------------------------
// TComboboxEx methods ...
//------------------------------------------------------------------------------

procedure TComboboxEx.CreateWnd;
begin
  inherited;
  if HeightDx < 0 then sendmessage(Handle, CB_SETITEMHEIGHT, -1, Height +HeightDx);
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.KeyDown(var Key: Word; Shift: TShiftState);
var
  itm: TScrollItem;
begin
  case Key of
    VK_RETURN:
      if (Shift * [ssAlt,ssCtrl] <> []) then
         if TScrollItemOI(ObjInspector.FocusedItem).editType = etEllipsis then
           ObjInspector.BtnCtrlClick(ObjInspector)
      else
        inherited;
    VK_DELETE:
      if (TScrollItemOI(ObjInspector.FocusedItem).editType in
        [etReadOnly, etEllipsisRO, etComboList]) then
          key := 0 else
          inherited;
    VK_DOWN:
      if DroppedDown and (Style <> csSimple) then
        inherited
      else if (Shift = [ssCtrl]) and (Style <> csSimple) then
      begin
        fValidateInChange := true;
        //will change to next item in combobox list ...
        inherited;
      end else if Shift = [ssAlt] then
        //nothing => will either dropsdown or closes combobox list
      else
      begin
        key := 0;
        with ObjInspector do
          itm := NextVisibleItem(FocusedItem);
        if assigned(itm) then itm.SetFocus;
      end;
    VK_UP:
      if DroppedDown and (Style <> csSimple) then
        inherited
      else if (Shift = [ssAlt]) or (Shift = [ssCtrl]) then
      begin
        if (Shift = [ssCtrl]) then fValidateInChange := true;
        inherited;
      end else
      begin
        key := 0;
        with ObjInspector do
          itm := PriorVisibleItem(FocusedItem);
        if assigned(itm) then itm.SetFocus;
      end;
    VK_LEFT:
      if (Shift = [ssCtrl]) and ObjInspector.FocusedItem.HasChildren then
      begin
        with ObjInspector.FocusedItem do
          if HasChildren and not Collapsed then Collapsed := true;
        Key := 0;
        ObjInspector.EditItem;
      end;
    VK_RIGHT:
      if (Shift = [ssCtrl]) and ObjInspector.FocusedItem.HasChildren then
      begin
        with ObjInspector.FocusedItem do
          if HasChildren and Collapsed then Collapsed := false;
        Key := 0;
        ObjInspector.EditItem;
      end;
  else
    inherited;
  end;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.KeyPress(var Key: Char);

  procedure LookupItem;
  var
    i,idx: integer;
  begin
    //simulate the DropDownList style lookup ...
    with TScrollItemOI(ObjInspector.FocusedItem).ValueOptions do
    begin
      idx := IndexOf(self.text);
      i := Idx +1;
      while (i < count) do
        if (length(strings[i]) = 0) or
          (AnsiCompareText(strings[i][1], Key) <> 0) then inc(i)
        else break;
      if i = count then
      begin
        i := 0;
        while (i < Idx) do
          if (length(strings[i]) = 0) or
            (AnsiCompareText(strings[i][1], Key) <> 0) then inc(i)
          else break;
      end;
      ItemIndex := i;
    end;
    Key := #0;
    ObjInspector.DoValidation;
  end;

begin
  with TScrollItemOI(ObjInspector.FocusedItem) do
    if editType in [etReadOnly, etEllipsisRO] then
      Key := #0
    else if (Key = #27) then
    begin
      if DroppedDown then CloseUp;
      text := propValue;
      SelectAll;
      Key := #0;
      inherited;
    end
    else if (Key = #13) then
    begin
      if not self.DroppedDown then Key := #0;
      ObjInspector.DoValidation;
      inherited;
    end
    else if (editType = etComboList) and not (Key in [#13,#27]) then
      LookupItem
    else
      inherited;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.DblClick;
var
  i: integer;
begin
  inherited;
  if (Style = csSimple) or (Items.Count = 0) then exit;
  //select next item in list ...
  i := Items.IndexOf(text)+1;
  if i = items.count then i := 0;
    itemindex := i;
  ObjInspector.DoValidation;
end;
//------------------------------------------------------------------------------

function TComboboxEx.GetItemHt: Integer;
begin
  if HeightDx < 0 then
    result := Height else
    result := inherited GetItemHt;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.Change;
begin
  inherited;
  //close up doesn't always immediately update combobox.text so
  //the validation is delayed until Change() is called ...
  if fValidateInChange then ObjInspector.DoValidation;
  fValidateInChange := false;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.DropDown;
begin
  inherited;
  fValidateInChange := false;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.CloseUp;
begin
  inherited;
  //it seems that the combobox height is readjusted when the list closed, so ...
  if HeightDx < 0 then sendmessage(Handle, CB_SETITEMHEIGHT, -1, Height+HeightDx);
  fValidateInChange := true;
end;
//------------------------------------------------------------------------------

procedure TComboboxEx.DoEnter;
begin
  inherited;
  fValidateInChange := false;
end;

//------------------------------------------------------------------------------
// TObjInspectorBase methods ...
//------------------------------------------------------------------------------

constructor TObjInspectorBase.Create(aOwner: TComponent);
begin
  inherited;
  ScrollItemClass           := TScrollItemOI;
  DefaultStyle              := 3;
  AutoShowEditor            := true;
  DefaultItemColor          := clBtnFace;
  DefaultItemHeight         := 20;
  Font.Name                 := 'Arial';
  Font.Size                 := 8;
  HideSelection             := false;
  VertSpace                 := 0;
  FDividerOffset            := 120;
  FMinDividerOffset         := 60;
  FValueLeftMargin          := 1;
  TopOffset                 := 0;

  FItemEditor               := TItemEditor.Create(nil);
  FItemEditor.Visible       := false;
  FItemEditor.Parent        := self;
  FItemEditor.Height        := DefaultItemHeight;
  FItemEditor.HideOnDblClick:= false;
  FItemEditor.OnPaint       := EditorPaint;
  FItemEditor.OnBeforeEnter := EditorBeforeEnter;
  FItemEditor.OnExit        := EditorExit;

  self.ItemEditor           := FItemEditor;

  FComboCtrl                := TComboboxEx.Create(nil);
  FComboCtrl.Parent         := FItemEditor;
  FComboCtrl.Top            := 0;
  FComboCtrl.Left           := FDividerOffset;
  FComboCtrl.Height         := DefaultItemHeight;
  FComboCtrl.HeightDx       := -7;
  FComboCtrl.ObjInspector := self;
  FComboCtrl.Width          := 100;
  FComboCtrl.Style          := csSimple;

  FBtnCtrl                  := TButton.Create(nil);
  FBtnCtrl.Parent           := FItemEditor;
  FBtnCtrl.Top              := 2;
  FBtnCtrl.Height           := FItemEditor.Height -3;
  FBtnCtrl.Width            := FBtnCtrl.Height;
  FBtnCtrl.Caption          := '...';
  FBtnCtrl.OnClick          := BtnCtrlClick;
  FBtnCtrl.TabStop          := false;
  FBtnCtrl.Font.Assign(font);
end;
//------------------------------------------------------------------------------

destructor TObjInspectorBase.Destroy;
begin
  FComboCtrl.Free;
  FBtnCtrl.Free;

  FItemEditor.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.DoEnter;
begin
  inherited;
  if AutoShowEditor then EditItem;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //disable mouse-down events if the mouse is over the divider ...
  FMovingDivider := (Shift = [ssLeft]) and (abs(X-FDividerOffset) < 3);
  if FMovingDivider then
  begin
    Cursor := crHSplit;
    ItemEditor.Hide;
  end
  else inherited;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMovingDivider then
  begin
    FDividerOffset := min(max(X,FMinDividerOffset), ClientWidth - 20);
    Invalidate;
  end
  else if abs(X-FDividerOffset) < 3 then
    Cursor := crHSplit else
    Cursor := crDefault;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMovingDivider then
  begin
    FDividerOffset := min(max(X,FMinDividerOffset), ClientWidth - 20);
    FMovingDivider := false;
    EditItem;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.EditorPaint(Sender: TObject);
begin
  with TItemEditor(Sender), Canvas, TScrollItemOI(Item) do
  begin
    textout(4+ BOX_SIZE+BOX_GAP*2 + TreeLevel*(BOX_SIZE +BOX_GAP), 3, propName);

    pen.Color := clGray;
    moveto(0, Height);
    lineto(0,1);
    lineto(DividerOffset,1);

    //draw the divider ...
    pen.Color := clBtnShadow;
    moveto(DividerOffset, 0);
    lineto(DividerOffset,Height);
    pen.Color := clBtnHighlight;
    moveto(DividerOffset+1, 0);
    lineto(DividerOffset+1,Height);
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.EditorBeforeEnter(Sender: TObject);
var
  hdl: HWND;
  //ComboBoxInfo: TComboBoxInfo;
begin
  FComboCtrl.Clear;
  case TScrollItemOI(FocusedItem).editType of
    etReadOnly:
      begin
        FComboCtrl.enabled := true;
        FComboCtrl.Style := csSimple;
        FComboCtrl.Color := clBtnFace;
        FComboCtrl.Left := dividerOffset;
        FComboCtrl.width := FItemEditor.width - DividerOffset+2;
        FBtnCtrl.Visible := false;
        hdl := FindWindowEx(FComboCtrl.Handle, 0, 'EDIT', nil);
        sendmessage(hdl, EM_SETMARGINS, EC_LEFTMARGIN, FValueLeftMargin);
      end;
    etEdit:
      begin
        FComboCtrl.enabled := true;
        FComboCtrl.Style := csSimple;
        FComboCtrl.Color := clWindow;
        FComboCtrl.Left := dividerOffset;
        FComboCtrl.width := FItemEditor.width - DividerOffset+2;
        FBtnCtrl.Visible := false;
        hdl := FindWindowEx(FComboCtrl.Handle, 0, 'EDIT', nil);
        sendmessage(hdl, EM_SETMARGINS, EC_LEFTMARGIN, FValueLeftMargin);
      end;
    etEllipsis:
      begin
        FComboCtrl.Visible := true;
        FComboCtrl.Left := dividerOffset;
        FComboCtrl.enabled := true;
        FComboCtrl.Style := csSimple;
        FComboCtrl.Color := clWindow;
        FComboCtrl.width := FItemEditor.width - DividerOffset+2;
        FBtnCtrl.Visible := true;
        FBtnCtrl.Left := FItemEditor.width - FBtnCtrl.Width -2;
        hdl := FindWindowEx(FComboCtrl.Handle, 0, 'EDIT', nil);
        sendmessage(hdl, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN,
          FBtnCtrl.Width SHL 16 + FValueLeftMargin);
      end;
    etEllipsisRO:
      begin
        FComboCtrl.enabled := true;
        FComboCtrl.Style := csSimple;
        FComboCtrl.Color := clBtnFace;
        FComboCtrl.Left := dividerOffset;
        FComboCtrl.width := FItemEditor.width - DividerOffset+2;
        FBtnCtrl.Visible := true;
        FBtnCtrl.Left := FItemEditor.width - FBtnCtrl.Width -2;
        hdl := FindWindowEx(FComboCtrl.Handle, 0, 'EDIT', nil);
        sendmessage(hdl, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN,
          FBtnCtrl.Width SHL 16 + FValueLeftMargin);
      end;
    etCombo, etComboList:
      begin
        FComboCtrl.Visible := true;
        FComboCtrl.Left := dividerOffset;
        FComboCtrl.enabled := true;
        //sadly, margins don't work for DropDownList styled comboboxes
        //so that combobox style is simulating instead ...
        FComboCtrl.Style := csDropDown;
        FComboCtrl.Color := clWindow;
        FComboCtrl.Items.Assign(TScrollItemOI(FocusedItem).ValueOptions);
        FComboCtrl.width := FItemEditor.width - DividerOffset+2;
        FBtnCtrl.Visible := false;
        hdl := FindWindowEx(FComboCtrl.Handle, 0, 'EDIT', nil);
        sendmessage(hdl, EM_SETMARGINS, EC_LEFTMARGIN, FValueLeftMargin);
      end;
  end;
  if FComboCtrl.Style = csDropDownList then
  begin
    FComboCtrl.ItemIndex :=
      FComboCtrl.Items.IndexOf(TScrollItemOI(FocusedItem).propValue);
  end else
    FComboCtrl.text := TScrollItemOI(FocusedItem).propValue;
  //I'm really not sure why but the following also appears to be necessary
  //whenever the dropdown list has been used to change a combobox value ...
  SendMessage(FComboCtrl.Handle, WM_SETTEXT, 0, integer(PAnsiChar(FComboCtrl.text)));
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.EditorExit(Sender: TObject);
begin
  DoValidation;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.DoValidation;
var
  isValidValue: boolean;
  newText: string;
begin
  newText := FComboCtrl.Text;
  if AnsiSameStr(newText, TScrollItemOI(focusedItem).propValue) then exit;

  //validate changes ...
  isValidValue := true;
  with TScrollItemOI(focusedItem) do
  begin
    case editType of
      etReadOnly, etEllipsisRO: exit;
      etComboList: if newText = '' then isValidValue := false;
    end;
    if Assigned(FValidateEvent) then
      FValidateEvent(self, newText, TScrollItemOI(focusedItem), isValidValue);
  end;
  if isValidValue then
  begin
    TScrollItemOI(FocusedItem).propValue := newText;
    FComboCtrl.Text := newText;
  end else
  begin
    //restore previous value (which is presumably valid) ...
    FComboCtrl.Text := TScrollItemOI(FocusedItem).propValue;
    //??beep on error??
  end;
  FComboCtrl.SelectAll;
  FComboCtrl.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorBase.BtnCtrlClick(Sender: TObject);
begin
  FComboCtrl.SetFocus;
  if assigned(FEllipsisEvent) then
    FEllipsisEvent(self, TScrollItemOI(focusedItem));
end;
//------------------------------------------------------------------------------

end.
