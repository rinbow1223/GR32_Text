unit ScrollingCtrl;

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
 * The Original Code is ScrollingCtrl.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2002-2009 the Initial Developer. All Rights Reserved.
 *
 * Version 1.15 (Last updated 15-Sep-09)
 *
 * END LICENSE BLOCK **********************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, ExtCtrls,
  Graphics, StdCtrls, Themes, uxTheme, TypInfo;

const
  CM_SHOWEDITOR = WM_USER + 100;
  CM_ITEMCHANGED = WM_USER + 101;

  BOX_SIZE = 13;
  BOX_GAP = 1;

type
  TScrollingCtrl = class;
  TScrollItemClass = class of TScrollItem;
  TGlyphType = (gtUnchecked = 9, gtUncheckedHigh, gtUncheckedDis = 12,
    gtChecked, gtCheckedHigh, gtCheckedDis = 16, gtCollapsed, gtCollapsedHigh,
    gtExpanded = 21, gtExpandedHigh);

  TShadowEdge = (seLeft, seTop, seRight, seBottom);
  TShadowEdges = set of TShadowEdge;

  TScrollItem = class
  private
    fIndex: integer;
    fItemStyle: integer;
    fVirtualTop: integer; //ie Top independant of scroll position
    fTreeLevel: integer;
    fCollapsed: boolean;
    fHeight: integer;
    fFocused: boolean;
    fChecked: boolean;
    fReadOnly: boolean;
    fVisible: boolean;
    fColor: TColor;
    fCheckboxVisible: boolean;
    fScrollingCtrl: TScrollingCtrl;
    fBlockRecursion: boolean;
    procedure SetColor(color: TColor);
    procedure SetHeight(height: integer);
    function GetClientTop: integer;
    procedure SetTreeLevel(level: integer);
    procedure SetVisible(visible: boolean);
    function GetParentItem: TScrollItem;
    procedure SetCollapse(collapse: boolean);
    procedure SetItemStyle(style: integer);
  protected
    procedure Paint; virtual;
    procedure SetChecked(checked: boolean); virtual;
    function GetCheckboxRect: TRect; virtual;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DblClick; virtual;
    property VirtualTop: integer read fVirtualTop;
  public
    constructor Create; virtual;
    procedure SetFocus;
    procedure ScrollIntoView;
    function HasChildren: boolean;
    function NextVisibleItem: TScrollItem;
    function PriorVisibleItem: TScrollItem;
    procedure DrawSimpleOutline;

    property ScrollingCtrl: TScrollingCtrl read fScrollingCtrl;
    property Checked: boolean read fChecked write SetChecked;
    property Focused: boolean read fFocused;
    property CheckboxVisible: boolean read fCheckboxVisible write fCheckboxVisible;
    property ReadOnly: boolean read fReadOnly write fReadOnly;
    property Color: TColor read fColor write SetColor;
    property Height: integer read fHeight write SetHeight;
    property Index: integer read fIndex;
    property Top: integer read GetClientTop; //ie in Client coordinates
    property TreeLevel: integer read fTreeLevel write SetTreeLevel;
    property Collapsed: boolean
      read fCollapsed write SetCollapse;
    property ParentItem: TScrollItem read GetParentItem;
    property Style: integer read fItemStyle write SetItemStyle;
    property Visible: boolean read fVisible write SetVisible;
  end;

  TItemEditor = class(TCustomControl)
  private
    fItem: TScrollItem;
    FLastItemIdx: integer;
    //ftmpColor & ftmpParentColor are used to workaround problems with item
    //colors which change when TItemEditor contains controls like TLabels ...
    ftmpColor: TColor;
    ftmpParentColor: boolean;
    fItemHeight: integer;
    fHideOnDblClick: boolean;
    fOnBeforeEnter: TNotifyEvent;
    fOnPaint: TNotifyEvent;
  protected
    procedure DoExit; override;
    procedure Paint; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure DoEdit(item: TScrollItem); virtual;
    property Item: TScrollItem read fItem;
    property Canvas;
  published
    property Font;
    property Color nodefault;
    property HideOnDblClick: boolean read fHideOnDblClick write fHideOnDblClick;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnBeforeEnter: TNotifyEvent read fOnBeforeEnter write fOnBeforeEnter;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TScrollHeader = class(TCustomControl)
  private
    fColDividerList: TList;
    fIsThemed: boolean;
    FShadowEdges: TShadowEdges;
    FOnPaint: TNotifyEvent;
    procedure SetShadowEdges(const Value: TShadowEdges);
    procedure DrawBackground;
    procedure PaintColDividers;
  protected
    procedure Paint; override;
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddColDivider(XOffset: integer);
    procedure ClearColDividers;
    property Canvas;
  published
    property Align;
    property Color nodefault;
    property Font;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowEdges: TShadowEdges read fShadowEdges write SetShadowEdges;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TItemCheckedEvent = procedure(Sender: TObject; ScrollItem: TScrollItem) of Object;
  TScrollCtrlSortFunc = function (Item1, Item2: TScrollItem): Integer;

  TScrollingCtrl = class(TCustomControl)
  private
    fGuiItems: TList;
    fColDividerList: TList;
    fStyle: integer;
    fHorzSpace: integer;
    fVertSpace: integer;
    fTopOffset: integer;
    fDefaultItemHeight: integer;
    fDefaultItemColor: TColor;
    fHideSelection: boolean;
    fFocusedIndex: integer;
    fSurfaceHeight: integer;
    fVertPos: integer;
    fVertExponent: integer;
    fIndexTopItem: integer;
    fFocusedColor: TColor;
    fUpdateCount: integer;
    fIsThemed: boolean;
    fBmp: TBitmap; //checkbox, radiobutton etc images
    fScrollItemClass: TScrollItemClass;
    fItemEditor: TItemEditor;
    fItemChanging: boolean;
    fAutoShowEditor: boolean;
    fOnItemCheck: TItemCheckedEvent;
    fOnItemChange: TNotifyEvent;
    procedure SetStyle(Style: integer);
    procedure SetDefaultItemColor(Color: TColor);
    procedure SetDefaultItemHeight(Height: integer);
    procedure UpdateVScroll;
    procedure SetVertPos(pos: integer);
    procedure SetHorzSpace(horzSpace: integer);
    procedure SetVertSpace(vertSpace: integer);
    procedure SetTopOffset(Offset: integer);
    function GetItem(index: integer): TScrollItem;
    function GetFocusedItem: TScrollItem;
    procedure SetScrollItemClass(ScrollItemClass: TScrollItemClass);
    procedure SetItemEditor(ItemEditor: TItemEditor);
    procedure ResetTopItem;
    procedure DoItemChange;
    procedure DrawBackground;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure WMGetDlgCode(var message: TMessage); message WM_GETDLGCODE;
    procedure WMVSCROLL(var message: TWMScroll); message	WM_VSCROLL;
    procedure WMERASEBKGND(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure CMSHOWEDITOR(var message: TMessage); message CM_SHOWEDITOR;
    procedure CMITEMCHANGED(var message: TMessage); message CM_ITEMCHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
    procedure Resize; override;
    function GetVertPos: integer;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem: TScrollItem;
    procedure EditItem;
    procedure CancelEdit;
    function InsertItem(index: integer): TScrollItem;
    procedure DeleteItem(index: integer);
    procedure ClearItems;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CollapseAll;
    procedure ExpandAll;
    procedure PaintColDividers;
    procedure AddColDivider(XOffset: integer);
    procedure ClearColDividers;
    function GetScrollItem(ClientY: integer): TScrollItem;
    function NextVisibleItem(item: TScrollItem): TScrollItem;
    function PriorVisibleItem(item: TScrollItem): TScrollItem;

    function ScrollbarIsVisible: boolean;
    procedure Sort(SortFunc: TScrollCtrlSortFunc; KeepCurrentFocus: boolean);

    property FocusedItem: TScrollItem read GetFocusedItem;
    property Items[index: integer]: TScrollItem read GetItem; default;
    function ItemsCount: integer;
    property ItemEditor: TItemEditor read fItemEditor write SetItemEditor;
    property ScrollItemClass: TScrollItemClass
      read fScrollItemClass write SetScrollItemClass;
    property Bitmap: TBitmap read fBmp;
    property Canvas;
    property FocusedColor: TColor read fFocusedColor write fFocusedColor;
    property IsThemed: boolean read fIsThemed;
  published
    property Align;
    property Anchors;
    property AutoShowEditor: boolean read fAutoShowEditor write fAutoShowEditor;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property Constraints;
    property DefaultItemColor: TColor
      read fDefaultItemColor write SetDefaultItemColor;
    property DefaultItemHeight: integer
      read fDefaultItemHeight write SetDefaultItemHeight;
    property DefaultStyle: integer read fStyle write SetStyle;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property HideSelection: boolean read fHideSelection write fHideSelection;
    property HorzSpace: integer read fHorzSpace write SetHorzSpace;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SurfaceHeight: integer read fSurfaceHeight;
    property TabOrder;
    property TabStop;
    property TopOffset: integer read fTopOffset write SetTopOffset;
    property VertSpace: integer read fVertSpace write SetVertSpace;
    property Visible;
    property OnCanResize;
    property OnItemChange: TNotifyEvent read fOnItemChange write fOnItemChange;
    property OnItemCheck: TItemCheckedEvent read fOnItemCheck write fOnItemCheck;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

function GlyphRect(GlyphType: TGlyphType): TRect;

procedure Register;

implementation

resourcestring
  rsDoEditError       = 'DoEdit: No control to take focus';
  rsGetItemRangeError = 'GetItem: Range error.';
  rsSetItemClassError = 'SetScrollItemClass: item list must be empty.';
  rsAddItemError      = 'No ScrollItemClass defined.';
  rsIndexRangeError   = 'DeleteItem: index out of range.';
  rsDeleteItemError   = 'DeleteItem: Can''t delete items with children.';

{$R ScrollingCtrl.res}

type THackedControl = class(TControl);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Samples', [TItemEditor, TScrollHeader, TScrollingCtrl]);
end;
//------------------------------------------------------------------------------

function HasThemeManifest: boolean;
begin
  result := FindResource(hInstance, makeintresource(1), MakeIntResource(24)) > 0;
end;
//------------------------------------------------------------------------------

procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SortFunc: TScrollCtrlSortFunc);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SortFunc(SortList^[I], P) < 0 do Inc(I);
      while SortFunc(SortList^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SortFunc);
    L := I;
  until I >= R;
end;
//------------------------------------------------------------------------------

procedure MergeInternal(list, temp: PPointerList;
  l_start, r_start, r_end: integer; SortFunc: TScrollCtrlSortFunc);
var
  i, l_end, l_start_saved: integer;
begin
  l_end := r_start - 1;
  i := l_start;
  l_start_saved := l_start;

  while ((l_start <= l_end) and (r_start <= r_end)) do
  begin
    if SortFunc(list^[l_start], list^[r_start]) <= 0 then
    begin
      temp[i] := list[l_start];
      inc(l_start);
    end else
    begin
      temp[i] := list[r_start];
      inc(r_start);
    end;
    inc(i);
  end;

  while (l_start <= l_end) do
  begin
    temp^[i] := list^[l_start];
    inc(l_start);
    inc(i);
  end;

  while (r_start <= r_end) do
  begin
    temp^[i] := list^[r_start];
    inc(r_start);
    inc(i);
  end;
  for i := l_start_saved to r_end do list^[i] := temp^[i];
end;

//------------------------------------------------------------------------------

procedure MergeSortInternal(list, temp: PPointerList;
  l, r: integer; SortFunc: TScrollCtrlSortFunc);
var
  l_end: integer;
begin
  if (r = l) then exit;
  l_end := (r + l) shr 1;
  MergeSortInternal(list, temp, l, l_end, SortFunc);
  MergeSortInternal(list, temp, l_end +1, r, SortFunc);
  MergeInternal(list, temp, l, l_end +1, r, SortFunc);
end;
//------------------------------------------------------------------------------

// MergeSort() is about 50% slower than QuickSort() but at least it's 'stable'.
procedure MergeSort(SortList: PPointerList; l, r: Integer;
  SortFunc: TScrollCtrlSortFunc);
var
  tmpList: PPointerList;
begin
  if r = l then exit;
  GetMem(tmpList, SizeOf(pointer)* (r - l + 1));
  try
    mergeSortInternal(sortList, tmpList, l, r, SortFunc);
  finally
    FreeMem(tmpList);
  end;
end;

//------------------------------------------------------------------------------

function min(int1, int2: integer): integer;
begin
  if int1 < int2 then result := int1 else result := int2;
end;
//------------------------------------------------------------------------------

function max(int1, int2: integer): integer;
begin
  if int1 > int2 then result := int1 else result := int2;
end;
//------------------------------------------------------------------------------

function GlyphRect(GlyphType: TGlyphType): TRect;
begin
  result := Rect(ord(GlyphType) *16 +1,1,ord(GlyphType)*16+14,14)
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TItemEditor.Create(aOwner: TComponent);
begin
  inherited;
  ParentBackground := false;
  ControlStyle := ControlStyle +[csAcceptsControls];
  visible := false;
  fHideOnDblClick := true;
  FLastItemIdx := -1;
end;
//------------------------------------------------------------------------------

procedure TItemEditor.Paint;
var
  rec: TRect;
begin
  if (csDesigning in ComponentState) or
    not (parent is TScrollingCtrl) or
    not assigned(Item) then
      with canvas do
      begin
        brush.Color := self.Color;
        rec := self.ClientRect;
        Pen.Color := clBtnHighlight;
        rectangle(rec);
        inflateRect(rec, - 1, - 1);
        Pen.Color := clBtnShadow;
        rectangle(rec);
        exit;
      end;

  with TScrollingCtrl(parent), self.canvas do
  begin
    Font.Assign(Item.ScrollingCtrl.Font);
    rec := self.ClientRect;

    brush.Color := TScrollingCtrl(parent).Color;
    FillRect(rec);
    inflateRect(rec, -fHorzSpace, 0);
    if not self.ParentColor then brush.Color := self.Color;

    case Item.Style of
      1:
        begin
          inflateRect(rec, 0, - 1);
          Pen.Color := fFocusedColor;
          rectangle(rec);
        end;
      2: ;
      3:
        begin
          Pen.Color := clBtnShadow;
          moveto(0,rec.Top); lineto(self.ClientWidth,rec.Top);
          Pen.Color := clBtnHighlight;
          moveto(0,rec.Bottom); lineto(self.ClientWidth,rec.Bottom);
        end;
      else
        begin
          Pen.Color := clBtnHighlight;
          rectangle(rec);
          inflateRect(rec, - 1, - 1);
          Pen.Color := fFocusedColor;
          rectangle(rec);
        end;
    end;

    if Item.fCheckboxVisible then
    begin
      rec := Item.GetCheckboxRect;
      rec.Top := (self.Height - BOX_SIZE) div 2 +1;
      rec.Bottom := rec.Top + BOX_SIZE;
      if FocusedItem.Checked then
        CopyRect(rec, fBmp.Canvas, GlyphRect(gtCheckedDis)) else
        CopyRect(rec, fBmp.Canvas, GlyphRect(gtUncheckedDis));
    end
    else if FocusedItem.HasChildren then
    begin
      rec := Item.GetCheckboxRect;
      rec.Top := (self.Height - BOX_SIZE) div 2 +1;
      rec.Bottom := rec.Top + BOX_SIZE;
      if FocusedItem.fCollapsed then
        CopyRect(rec, fBmp.Canvas, GlyphRect(gtCollapsed)) else
        CopyRect(rec, fBmp.Canvas, GlyphRect(gtExpanded));
    end;

    if Assigned(fOnPaint) then FOnPaint(self);
  end;
end;
//------------------------------------------------------------------------------

procedure TItemEditor.DoExit;
var
  sc: TScrollingCtrl;
begin
  if assigned(item) then
    sc := Item.fScrollingCtrl else
    sc := nil;
  try
    inherited;
  finally
    //restore original item height if it hasn't been deleted ...
    if assigned(Item) then
    begin
      fItem.Height := fItemHeight;
      FLastItemIdx := Item.Index;
    end;
    hide;
    fItem := nil;
    Color := ftmpColor;
    ParentColor := ftmpParentColor;
  end;
  if assigned(sc) and not sc.AutoShowEditor then sc.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TItemEditor.DblClick;
begin
  inherited;
  if fHideOnDblClick then hide;
end;
//------------------------------------------------------------------------------

procedure TItemEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if not visible then exit; //ie when DblClicked
  pt := ClientToScreen(Point(X,Y));
  pt := item.ScrollingCtrl.ScreenToClient(pt);
  with Item do
    if PtInRect(GetCheckboxRect, pt) then
    begin
      if CheckboxVisible then SetChecked(not Checked)
      else if HasChildren then SetCollapse(not Collapsed)
      else exit;
      ScrollingCtrl.invalidate;
      ScrollingCtrl.EditItem;
    end;
end;
//------------------------------------------------------------------------------

procedure TItemEditor.DoEdit(item: TScrollItem);
var
  i, tabOrd, lowestTabOrder: integer;
begin
  if visible and (FLastItemIdx = item.Index) then exit;
  visible := false;
  fItem := item;
  if (ControlCount = 0) or not item.fVisible then exit;
  item.SetFocus;
  Parent := item.fScrollingCtrl;
  Width := Parent.ClientWidth;

  ftmpParentColor := ParentColor;
  ftmpColor := Color;
  if ParentColor then Color := item.Color;

  //event to facilitate final customizing of edit control(s) before showing...
  if assigned(fOnBeforeEnter) then fOnBeforeEnter(self);

  tabOrd := -1;
  lowestTabOrder := $FFFF;
  //find the ItemEditor child control (if any) to receive focus ...
  for i := 0 to ControlCount -1 do
    if (Controls[i] is TWinControl) then
      with TWinControl(Controls[i]) do
      begin
        if (TabStop = true) and visible and
          enabled and (TabOrder < lowestTabOrder) then
        begin
          lowestTabOrder := TabOrder;
          tabOrd := i;
        end;
        //also, make sure no child controls extend beyond right edge...
        if Left + Width > self.ClientWidth then
          Width := self.ClientWidth - Left;
      end;
  if tabOrd < 0 then
    raise Exception.Create(rsDoEditError);

  //resize the item height to the ItemEditor height ...
  fItemHeight := fItem.Height;
  fItem.Height := self.Height;
  fItem.ScrollIntoView;

  setbounds(0, item.Top, parent.ClientWidth, Height);

  visible := true;
  TWinControl(Controls[tabOrd]).SetFocus;
  //do everything else in descendant classes
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TScrollHeader.Create(aOwner: TComponent);
begin
  inherited;
  fIsThemed := HasThemeManifest and ThemeServices.ThemesEnabled;
  ControlStyle := ControlStyle +[csAcceptsControls];
  FShadowEdges := [seLeft, seTop, seRight, seBottom];
  fColDividerList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TScrollHeader.Destroy;
begin
  FreeAndNil(fColDividerList);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.AddColDivider(XOffset: integer);
begin
  fColDividerList.Add(pointer(XOffset));
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.ClearColDividers;
begin
  fColDividerList.Clear;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.PaintColDividers;
var
  i,b: integer;
begin
  if seBottom in FShadowEdges then b := height-1 else b := height;

  for i := 0 to fColDividerList.Count -1 do
  begin
    canvas.Pen.Color := clBtnShadow;
    canvas.moveto(integer(fColDividerList[i]),0);
    canvas.Lineto(integer(fColDividerList[i]),b);
    canvas.Pen.Color := clBtnHighlight;
    canvas.moveto(integer(fColDividerList[i])+1,0);
    canvas.Lineto(integer(fColDividerList[i])+1,b);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.SetShadowEdges(const Value: TShadowEdges);
begin
  FShadowEdges := Value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.DrawBackground;
var
  Details: TThemedElementDetails;
  rec: TRect;
begin
  rec := ClientRect;
  if ParentBackground and fIsThemed and assigned(DrawThemeParentBackground) and
    (DrawThemeParentBackground(handle, canvas.Handle, @Rec) = S_OK) then
  begin
    //workaround since the above brakes several canvas values ...
    canvas.Brush.Style := bsClear;
    canvas.Brush.Style := bsSolid;
    canvas.Pen.Style := psClear;
    canvas.Pen.Style := psSolid;
  end else if fIsThemed and ParentColor then
  begin
    ThemeServices.DrawParentBackground(Handle, canvas.Handle, nil, False, @rec);
  end else if fIsThemed and (Color = clBtnFace) then
  begin
    //nb: DrawElement will always shade Rec as if it's the element's clientrect
    Details := ThemeServices.GetElementDetails(thHeaderRoot);
    ThemeServices.DrawElement(canvas.Handle, Details, Rec);
  end else
  begin
    canvas.Brush.Color := self.Color;
    canvas.FillRect(rec);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.Paint;
begin
  with canvas do
  begin
    DrawBackground;
    brush.Color := self.Color;
    font := self.Font;
    SetBkMode(handle,TRANSPARENT);

    Pen.Color := clBtnHighlight;
    moveto(0,0);
    if seLeft in FShadowEdges then lineto(0, height);
    moveto(width-1, 0);
    if seTop in FShadowEdges then lineto(-1, 0);

    Pen.Color := clBtnShadow;
    moveto(0,height -1);
    if seBottom in FShadowEdges then lineto(width, height -1);
    moveto(width-1, height -1);
    if seRight in FShadowEdges then lineto(width-1, -1);
  end;
  if Assigned(FOnPaint) then FOnPaint(Self);
  PaintColDividers;
end;
//------------------------------------------------------------------------------

procedure TScrollHeader.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 0;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TScrollItem.Create;
begin
  inherited Create;
  fVisible := true;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetChecked(checked: boolean);
begin
  if not fCheckboxVisible or (fchecked = checked) then exit;
  fchecked := checked;
  if assigned(fScrollingCtrl.fOnItemCheck) and not fBlockRecursion then
  begin
    fBlockRecursion := true;
    try
      fScrollingCtrl.fOnItemCheck(fScrollingCtrl, self);
    finally
      fBlockRecursion := false;
    end;
  end;
  ScrollingCtrl.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.ScrollIntoView;
var
  ch: integer;
begin
  ch := ScrollingCtrl.clientHeight;
  if Top {ie VirtualTop - ScrollingCtrl.GetVertPos} + Height > ch then
  begin
    //ie at least part of the item will extend below the bottom of the control

    if Height + ScrollingCtrl.fVertSpace < ch then
      ScrollingCtrl.SetVertPos(VirtualTop + Height + ScrollingCtrl.fVertSpace - ch)
    else
      ScrollingCtrl.SetVertPos(VirtualTop - ScrollingCtrl.fVertSpace);
  end else if (Top < 0) then
    with ScrollingCtrl do SetVertPos(VirtualTop - fVertSpace - fTopOffset)
  else exit;
  ScrollingCtrl.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetFocus;
var
  parentItm: TScrollItem;
begin
  if fFocused or not fVisible then exit;
  with ScrollingCtrl do
  begin
    if assigned(fItemEditor) and (fItemEditor.Parent = ScrollingCtrl) and
      fItemEditor.Visible then fItemEditor.Hide;
    items[fFocusedIndex].fFocused := false;
    fFocusedIndex := fIndex;
  end;
  parentItm := GetParentItem;
  if assigned(parentItm) and parentItm.Collapsed then
    parentItm.Collapsed := false;
  fFocused := true;
  ScrollIntoView;
  with ScrollingCtrl do
  begin
    Invalidate;
    if AutoShowEditor then ItemEditor.DoEdit(self);
    DoItemChange;
  end;
end;
//------------------------------------------------------------------------------

function TScrollItem.GetCheckboxRect: TRect;
var
  x, t: integer;
begin
  x := ScrollingCtrl.fHorzSpace + BOX_GAP;
  //nb: result is in ScrollingCtrl client coordinates, not item coordinates
  t := Top +1;
  result := Rect(x + (fTreeLevel * (BOX_SIZE +BOX_GAP)),
    t + (height - BOX_SIZE) div 2,
    x+ BOX_SIZE + (fTreeLevel * (BOX_SIZE +BOX_GAP)),
    t + (height - BOX_SIZE) div 2 + BOX_SIZE);
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetCollapse(collapse: boolean);
var
  i: integer;
  editorWasVisible: boolean;
begin
  if fCollapsed = collapse then exit;
  with ScrollingCtrl do
  begin
    BeginUpdate;
    try
      editorWasVisible := assigned(fItemEditor) and
        (fItemEditor.Parent = ScrollingCtrl) and fItemEditor.Visible;
      if editorWasVisible then
      begin
        fItemEditor.FLastItemIdx := Index;
        fItemEditor.Hide;
      end;
      for i := fIndex +1 to itemsCount -1 do
      begin
        if items[i].fTreeLevel <= fTreeLevel then break;
        if collapse and items[i].fFocused then self.SetFocus;
        //recursively collapse but don't recursively expand ...
        if collapse or (items[i].TreeLevel = fTreeLevel +1) then
        begin
          items[i].SetVisible(not collapse);
          if not collapse and items[i].HasChildren then
            items[i].fCollapsed := true;
        end;
      end;
      fCollapsed := collapse;
      if editorWasVisible then fItemEditor.DoEdit(FocusedItem);
      invalidate;
    finally
      EndUpdate;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetItemStyle(style: integer);
begin
  if fItemStyle = style then exit;
  fItemStyle := style;
  ScrollingCtrl.invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetColor(color: TColor);
begin
  if color = fColor then exit;
  fColor := color;
  ScrollingCtrl.invalidate;
end;
//------------------------------------------------------------------------------

function TScrollItem.HasChildren: boolean;
begin
  with ScrollingCtrl do
    result := (fIndex < ItemsCount -1) and
      (items[fIndex+1].fTreeLevel > fTreeLevel);
end;
//------------------------------------------------------------------------------

function TScrollItem.NextVisibleItem: TScrollItem;
var
  i,cnt: integer;
begin
  i := Index +1;
  with ScrollingCtrl do
  begin
    cnt := ItemsCount;
    while (i < cnt) and not items[i].Visible do inc(i);
    if i = cnt then result := nil else result := items[i];
  end;
end;
//------------------------------------------------------------------------------

function TScrollItem.PriorVisibleItem: TScrollItem;
var
  i: integer;
begin
  i := Index -1;
  with ScrollingCtrl do
  begin
    while (i >= 0) and not items[i].Visible do dec(i);
    if i < 0 then result := nil else result := items[i];
  end;
end;
//------------------------------------------------------------------------------

function TScrollItem.GetParentItem: TScrollItem;
var
  i: integer;
begin
  result := nil;
  if (fTreeLevel = 0) then exit;
  with ScrollingCtrl do
    for i := fIndex -1 downto 0 do
      if items[i].fTreeLevel < fTreeLevel then
      begin
        result := items[i];
        break;
      end;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetHeight(height: integer);
var
  i, diff: integer;
begin
  //nb: never set Height := 0 externally, use the Visible property
  if height < 0 then height := 0;
  if fHeight = height then exit;
  diff := height - fHeight;
  if height = 0 then
    dec(diff, fScrollingCtrl.fVertSpace)
  else if fHeight = 0 then inc(diff, fScrollingCtrl.fVertSpace);
  fHeight := height;
  //adjust all following item positions & fSurfaceHeight ...
  with fScrollingCtrl do
  begin
    for i := fIndex +1 to ItemsCount -1 do
      TScrollItem(items[i]).fVirtualTop := TScrollItem(items[i]).VirtualTop + diff;
    inc(fSurfaceHeight, diff);
    if handleAllocated then UpdateVScroll;

    //finally if the height has been reduced and exposed blank area below ...
    if (GetVertPos  > 0) and
      (fSurfaceHeight - GetVertPos < ClientHeight) then
        SetVertPos(fSurfaceHeight - ClientHeight);
  end;
  ScrollingCtrl.invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetVisible(visible: boolean);
var
  tmpHeight: integer;
  nextItem: TScrollItem;
begin
  if (visible = fVisible) then exit;
  if not fVisible then
  begin
    //simple workaround to make visible ...
    tmpHeight := fHeight;
    fHeight := 0;
    SetHeight(tmpHeight);
  end;
  fVisible := visible;
  if not fVisible then
  begin
    if fFocused then
    begin
      nextItem := ScrollingCtrl.NextVisibleItem(self);
      if nextItem.GetParentItem = GetParentItem then
        nextItem.SetFocus else
        ScrollingCtrl.PriorVisibleItem(self).SetFocus;
    end;
    //simple workaround to make invisible ...
    tmpHeight := fHeight;
    SetHeight(0);
    fHeight := tmpHeight;
  end
end;
//------------------------------------------------------------------------------

function TScrollItem.GetClientTop: integer;
begin
  result := fVirtualTop - ScrollingCtrl.GetVertPos;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.SetTreeLevel(level: integer);
begin
  if level = fTreeLevel then exit;
  fTreeLevel := level;
  if (fTreeLevel > 0) and (GetParentItem <> nil) and GetParentItem.fCollapsed then
    SetVisible(false);
  ScrollingCtrl.invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.Paint;
var
  Details: TThemedElementDetails;
  rec: TRect;
  DrawFocusRec: boolean;
begin
  if Height = 0 then exit;
  with ScrollingCtrl, canvas do
  begin
    font := fScrollingCtrl.Font;
    rec := Rect(fHorzSpace, self.top, clientWidth -fHorzSpace*2, self.top + self.Height);
    DrawFocusRec := fFocused and (not fHideSelection or focused);
    Brush.Color := self.Color;
    case self.fItemStyle of
      1:
        begin
          if DrawFocusRec then
          begin
            if not focused then
              Pen.Color := clBtnShadow else
              Pen.Color := fFocusedColor;
          end else Pen.Color := clBtnShadow;

          DrawSimpleOutline;
        end;
      2:
        begin
          if DrawFocusRec then
          begin
            if ScrollingCtrl.focused then
              Brush.Color := clActiveCaption else
              Brush.Color := clInactiveCaption;
            fillrect(rec);
            if ScrollingCtrl.focused then
              Pen.Color := cl3DDkShadow else
              Pen.Color := clBtnShadow;
            moveto(rec.Left,rec.Bottom); lineto(rec.Right,rec.Bottom);
          end else if not ParentColor and not ParentBackground then
          begin
            Brush.Color := self.Color;
            fillrect(rec);
          end;
          //prepare for (overriden) painting of text ...
          if Brush.Color = clActiveCaption then
            font.Color := clCaptionText else
            font.Color := ScrollingCtrl.font.Color;
          SetBkMode(handle,TRANSPARENT);
        end;
      3:
        begin
          if not ParentColor and not ParentBackground then fillrect(rec);
          if DrawFocusRec then
          begin
            pen.Color := clBtnShadow;
            moveto(0,self.Top); lineto(ClientWidth,self.Top);
            pen.Color := clBtnHighlight;
            moveto(0,rec.Bottom); lineto(ClientWidth,rec.Bottom);
          end;
          font.Color := ScrollingCtrl.font.Color;
          SetBkMode(handle,TRANSPARENT);
        end;

      else //self.fItemStyle = 0
        if fIsThemed then
        begin
          if not DrawFocusRec then
            Details := ThemeServices.GetElementDetails(tbPushButtonDisabled)
          else if ScrollingCtrl.focused then
            Details := ThemeServices.GetElementDetails(tbPushButtonHot)
          else
              Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
          ThemeServices.DrawElement(Handle, Details, Rec);
          SetBkMode(handle,TRANSPARENT);
        end else
        begin
          if DrawFocusRec then
          begin
            if not ScrollingCtrl.focused then
              Pen.Color := clBtnShadow else
              Pen.Color := fFocusedColor
          end else Pen.Color := clBtnHighlight;
          Rectangle(rec);
          inflateRect(rec, -1, -1);
          if DrawFocusRec then
          begin
            if not ScrollingCtrl.focused then
              Pen.Color := clBtnShadow else
              Pen.Color := fFocusedColor
          end else Pen.Color := clBtnShadow;
          Rectangle(rec);
        end;
      end;

    if not fCheckboxVisible then
    begin
      if not HasChildren then //do nothing
      else if fCollapsed then
        CopyRect(GetCheckboxRect,fBmp.Canvas, GlyphRect(gtCollapsed))
      else
        CopyRect(GetCheckboxRect,fBmp.Canvas, GlyphRect(gtExpanded));
    end else if fChecked then
      CopyRect(GetCheckboxRect,fBmp.Canvas, GlyphRect(gtChecked))
    else
      CopyRect(GetCheckboxRect,fBmp.Canvas, GlyphRect(gtUnchecked));
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.DrawSimpleOutline;
var
  rec: TRect;
begin
  //this enables custom outline colors (ie call after setting pen.color)
  with ScrollingCtrl, canvas do
  begin
    rec := Rect(fHorzSpace, self.top, clientWidth -fHorzSpace*2, self.top + self.Height);
    Rectangle(rec);
    Pixels[rec.Left,rec.Top] := ScrollingCtrl.Color;
    Pixels[rec.Right-1,rec.Top] := ScrollingCtrl.Color;
    Pixels[rec.Left,rec.Bottom-1] := ScrollingCtrl.Color;
    Pixels[rec.Right-1,rec.Bottom-1] := ScrollingCtrl.Color;
    Pixels[rec.Left+1,rec.Top+1] := clBtnShadow;
    Pixels[rec.Right-2,rec.Top+1] := clBtnShadow;
    Pixels[rec.Left+1,rec.Bottom-2] := clBtnShadow;
    Pixels[rec.Right-2,rec.Bottom-2] := clBtnShadow;
    if fFocused and (not fHideSelection or focused) then
    begin
      InflateRect(rec,-1,-1);
      Rectangle(rec);
      Pixels[rec.Left+1,rec.Top+1] := clBtnShadow;
      Pixels[rec.Right-2,rec.Top+1] := clBtnShadow;
      Pixels[rec.Left+1,rec.Bottom-2] := clBtnShadow;
      Pixels[rec.Right-2,rec.Bottom-2] := clBtnShadow;
    end;
    SetBkMode(handle,TRANSPARENT);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not fFocused then fFocused := true;
  if PtInRect(GetCheckboxRect, Point(X,Y)) then
  begin
    if fCheckboxVisible then SetChecked(not fChecked)
    else if HasChildren then SetCollapse(not fCollapsed)
    else exit;
    fScrollingCtrl.invalidate;
  end;
  ScrollIntoView;
end;
//------------------------------------------------------------------------------

procedure TScrollItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  //override this
end;
//------------------------------------------------------------------------------

procedure TScrollItem.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //override this
end;
//------------------------------------------------------------------------------

procedure TScrollItem.DblClick;
begin
  Postmessage(fScrollingCtrl.handle, CM_SHOWEDITOR, 0, 0);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TScrollingCtrl.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  //ControlStyle := ControlStyle +[csParentBackground];
  height := 100;
  width := 100;
  ParentBackground := true;
  ParentColor := true;
  fSurfaceHeight := 0;
  tabStop := true;
  fBmp := TBitmap.Create;
  fBmp.LoadFromResourceName(hInstance,'XP');
  fGuiItems := TList.Create;
  fColDividerList := TList.Create;
  fHorzSpace := 2;
  fVertSpace := 2;
  fTopOffset := 2;
  fDefaultItemHeight := 25;
  fDefaultItemColor := clBtnFace;
  fFocusedColor := clHighlight;
  fHideSelection := true;
  fScrollItemClass := TScrollItem;
  fIsThemed := HasThemeManifest and ThemeServices.ThemesEnabled;
  DoubleBuffered := true;
end;
//------------------------------------------------------------------------------

destructor TScrollingCtrl.Destroy;
begin
  fBmp.Free;
  ClearItems;
  FreeAndNil(fGuiItems);
  FreeAndNil(fColDividerList);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Params.Style := Params.Style or WS_VSCROLL;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CMMouseWheel(var Message: TCMMouseWheel);
var
  ScrollCount : Integer;
  ScrollLines : Integer;
  oldVertPos: integer;
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1
    else
    begin
      if assigned(ItemEditor) and ItemEditor.Visible then ItemEditor.Hide;
      SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
      ScrollCount := -fDefaultItemHeight *ScrollLines *WheelDelta div WHEEL_DELTA;
      oldVertPos := fVertPos;
      SetVertPos(oldVertPos + ScrollCount);
      if fVertPos <> oldVertPos then invalidate;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Resize;
begin
  inherited;
  if (csDesigning in ComponentState) then exit;
  UpdateVScroll;
  if itemsCount > 0 then items[fFocusedIndex].ScrollIntoView;
  if assigned(fItemEditor) and
    (fItemEditor.Parent = self) and fItemEditor.Visible then fItemEditor.Hide;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.UpdateVScroll;
var
  scrollInfo: TScrollInfo;
begin
  if (fUpdateCount > 0) then exit; ////////////////////// 25 Jun 2006

  scrollInfo.cbSize := sizeof(scrollInfo);
  scrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  scrollInfo.nMin   := 0;
  scrollInfo.nTrackPos := 0;

  scrollInfo.nMax   := fSurfaceHeight-1;
  fVertPos := GetScrollPos(handle, SB_VERT) shl fVertExponent;

  fVertExponent := 0;
  while scrollInfo.nMax >= $7FFF do //ie: nMax <= 32,768
  begin
    scrollInfo.nMax := scrollInfo.nMax shr 1;
    Inc(fVertExponent);
  end;
  scrollInfo.nPage  := ClientHeight shr fVertExponent;
  scrollInfo.nPos := fVertPos shr fVertExponent;
  if HandleAllocated then
    windows.SetScrollInfo(handle, SB_VERT, scrollInfo, visible);
  ResetTopItem;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetVertPos(pos: integer);
begin
  fVertPos := max(0, pos);
  fVertPos := min(fVertPos, max(0, fSurfaceHeight - clientHeight));
  SetScrollPos(handle, SB_VERT, fVertPos shr fVertExponent, true);
  ResetTopItem;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetVertPos: integer;
begin
  result := fVertPos;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.ScrollbarIsVisible: boolean;
begin
  result := handleAllocated and
    (GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Sort(SortFunc: TScrollCtrlSortFunc;
  KeepCurrentFocus: boolean);
var
  i: integer;
begin
  if ItemsCount < 2 then exit;

  if not KeepCurrentFocus then
    TScrollItem(items[fFocusedIndex]).fFocused := false;

  //QuickSort(fGuiItems.List, 0, ItemsCount - 1, SortFunc);
  MergeSort(fGuiItems.List, 0, ItemsCount - 1, SortFunc);

  //fixup FocusedIndex ...
  if KeepCurrentFocus then
    for i := 0 to ItemsCount -1 do
      if TScrollItem(fGuiItems[i]).fFocused then
      begin
        fFocusedIndex := i;
        break;
      end;

  //now, fixup the position of all items ...
  fSurfaceHeight := 0;
  with TScrollItem(fGuiItems[0]) do
  begin
    fIndex := 0;
    fVirtualTop := fVertSpace + fTopOffset;
    fVisible := true;
    fCollapsed := false;
    fSurfaceHeight := fVirtualTop + Height + fVertSpace;
  end;
  for i := 1 to ItemsCount -1 do
    with TScrollItem(fGuiItems[i]) do
    begin
      fIndex := i;
      fVirtualTop := fSurfaceHeight;
      fSurfaceHeight := fVirtualTop + Height + fVertSpace;
      //for safety and simplicity ...
      fVisible := true;
      fCollapsed := false;
    end;
  if KeepCurrentFocus then
    items[fFocusedIndex].ScrollIntoView
  else
    TScrollItem(fGuiItems[0]).setfocus;
  //finally, in case an item's visibility has changed ...
  if HandleAllocated then UpdateVScroll;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMVSCROLL(var message: TWMScroll);
var
  oldPos: integer;
begin
  inherited;
  if assigned(fItemEditor) and
    (fItemEditor.Parent = self) and fItemEditor.Visible then fItemEditor.Hide;
  oldPos := GetVertPos;
  case message.ScrollCode of
    SB_LINEDOWN: SetVertPos(fVertPos + fDefaultItemHeight);
    SB_LINEUP: SetVertPos(fVertPos - fDefaultItemHeight);
    SB_PAGEDOWN: SetVertPos(fVertPos+clientHeight);
    SB_PAGEUP: SetVertPos(fVertPos-clientHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK: SetVertPos(message.Pos shl fVertExponent);
    SB_ENDSCROLL: exit;
    else beep;
  end;
  if GetVertPos <> oldPos then invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMGetDlgCode(var message: TMessage);
begin
  message.result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoEnter;
begin
  inherited;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoExit;
begin
  inherited;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CMITEMCHANGED(var message: TMessage);
begin
  fItemChanging := false;
  if assigned(fOnItemChange) then fOnItemChange(Self);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoItemChange;
begin
  //check if a CM_ITEMCHANGED message is already in the queue ...
  if (csDestroying in ComponentState) or fItemChanging then exit;
  Postmessage(handle, CM_ITEMCHANGED, 0, 0);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.ResetTopItem;
begin
  if fGuiItems.Count <= 1 then
  begin
    fIndexTopItem := 0;
    exit;
  end;
  //start with focused item and look from there ...
  fIndexTopItem := fFocusedIndex;
  while (fIndexTopItem < fGuiItems.Count) and
    ((TScrollItem(fGuiItems[fIndexTopItem]).Top +
      TScrollItem(fGuiItems[fIndexTopItem]).Height < VertSpace) or
    not TScrollItem(fGuiItems[fIndexTopItem]).fVisible) do inc(fIndexTopItem);
  if fIndexTopItem >= fGuiItems.Count then fIndexTopItem := fGuiItems.Count -1;
  while (fIndexTopItem > 0) and
    (TScrollItem(fGuiItems[fIndexTopItem]).Top > VertSpace) do dec(fIndexTopItem);
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.NextVisibleItem(item: TScrollItem): TScrollItem;
var
  i: integer;
begin
  for i := item.Index +1 to itemsCount -1 do
    if TScrollItem(items[i]).fVisible then
    begin
      result := TScrollItem(items[i]);
      exit;
    end;
  for i := item.Index downto 0 do
    if TScrollItem(items[i]).fVisible then
    begin
      result := TScrollItem(items[i]);
      exit;
    end;
  //if all else fails ...
  result := item;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.PriorVisibleItem(item: TScrollItem): TScrollItem;
var
  i: integer;
begin
  for i := item.Index -1 downto 0 do
    if TScrollItem(items[i]).fVisible then
    begin
      result := TScrollItem(items[i]);
      exit;
    end;
  for i := item.Index to itemsCount -1 do
    if TScrollItem(items[i]).fVisible then
    begin
      result := TScrollItem(items[i]);
      exit;
    end;
  //if all else fails ...
  result := item;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.KeyDown(var Key: Word; Shift: TShiftState);
var
  item: TScrollItem;
  ch: integer;
  msg: TWMScroll;
begin
  inherited;
  if (ssAlt in Shift) or (itemsCount = 0) then exit;

  case Key of
    VK_SPACE:
      begin
        with items[fFocusedIndex] do
          if fCheckboxVisible then SetChecked(not fChecked);
        items[fFocusedIndex].ScrollIntoView;
      end;
    VK_DOWN:
      if Shift <> [] then
      begin
        msg.ScrollCode := SB_LINEDOWN;
        WMVSCROLL(msg);
      end else
        NextVisibleItem(items[fFocusedIndex]).SetFocus;
    VK_UP:
      if Shift <> [] then
      begin
        msg.ScrollCode := SB_LINEUP;
        WMVSCROLL(msg);
      end else
        PriorVisibleItem(items[fFocusedIndex]).SetFocus;
    VK_LEFT:
      with items[fFocusedIndex] do
      begin
        if HasChildren then
        begin
          SetCollapse(true);
        end else
        begin
          item := GetParentItem;
          if assigned(item) then item.SetCollapse(true);
        end;
        ScrollIntoView;
      end;
    VK_RIGHT:
      with items[fFocusedIndex] do
      begin
        if HasChildren then SetCollapse(false);
        ScrollIntoView;
      end;
    VK_NEXT:
      begin
        ch := ClientHeight;
        with GetFocusedItem do
          if Top + Height < ch - fVertSpace then
            item := GetScrollItem(ch) else
            item := GetScrollItem(ch *2 - fVertSpace);
        if not assigned(item) then item := items[itemsCount -1];
        if not item.fVisible then
          NextVisibleItem(item).SetFocus else
          item.SetFocus;
      end;
    VK_PRIOR:
      begin
        ch := ClientHeight;
        with GetFocusedItem do
          if Top > fVertSpace + fTopOffset then
            item := GetScrollItem(0) else
            item := GetScrollItem(- ch + fVertSpace);
        if not item.fVisible then
          PriorVisibleItem(item).SetFocus else
          item.SetFocus;
      end;
    VK_HOME:
      begin
        item := items[0];
        if not item.Visible then NextVisibleItem(item).SetFocus
        else item.SetFocus;
      end;
    VK_END:
      begin
        item := items[itemsCount -1];
        if not item.Visible then PriorVisibleItem(item).SetFocus
        else item.SetFocus;
      end;
    VK_RETURN, VK_F2:
      begin
        items[fFocusedIndex].ScrollIntoView;
        if assigned(fItemEditor) and not items[fFocusedIndex].ReadOnly then
          fItemEditor.DoEdit(items[fFocusedIndex]);
      end;
  end;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetScrollItem(ClientY: integer): TScrollItem;
var
  i, idxLast: integer;
begin
  //ClientY = client coordinate
  result := nil;
  idxLast := itemsCount -1;
  if (idxLast < 0) or (Items[idxLast].Top + Items[idxLast].Height < ClientY) then
    exit;
  i := fIndexTopItem;
  while (i > 0) and ((Items[i].Top > ClientY) or not Items[i].fVisible) do
    dec(i);
  while (i < idxLast) and
    ((Items[i].Top + Items[i].Height < ClientY) or not Items[i].fVisible) do
    inc(i);
  result := Items[i];
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  item: TScrollItem;
begin
  //find the item before setting focus otherwise may get wrong item
  //eg if ItemEditor is visible and an item resizes
  item := GetScrollItem(Y);

  if not focused and canFocus then
  begin
    SetFocus;
    invalidate;
  end;

  // nb: assign new item before calling inherited otherwise in OnMousedown
  // events, FocusedItem may be out of date.
  if assigned(item) then
  begin
    item.SetFocus;
    item.MouseDown(Button, Shift, X, Y);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  item: TScrollItem;
begin
  inherited;
  item := GetScrollItem(Y);
  if not assigned(item) then exit;
  item.MouseMove(Shift, X, Y);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  item: TScrollItem;
begin
  inherited;
  item := GetScrollItem(Y);
  if not assigned(item) then exit;
  item.MouseUp(Button, Shift, X, Y);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DblClick;
var
  pt: TPoint;
  item: TScrollItem;
begin
  inherited;
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  item := GetScrollItem(pt.y);
  if assigned(item) then item.DblClick;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMERASEBKGND(var message: TWMEraseBkgnd);
begin
  message.Result := 0;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WMThemeChanged(var Message: TMessage);
begin
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CMSHOWEDITOR(var message: TMessage);
begin
  //this message has to be posted from dblclicked items otherwise
  //the editor will immediately loose focus and hide.
  if (itemsCount > 0) and assigned(fItemEditor) and
    not items[fFocusedIndex].ReadOnly then
  begin
    items[fFocusedIndex].ScrollIntoView;
    fItemEditor.DoEdit(GetFocusedItem);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DrawBackground;
var
  Details: TThemedElementDetails;
  rec: TRect;
begin
  rec := ClientRect;
  if ParentBackground and fIsThemed and assigned(DrawThemeParentBackground) and
    (DrawThemeParentBackground(handle, canvas.Handle, @Rec) = S_OK) then
  begin
    //workaround since the above brakes several canvas values ...
    canvas.Brush.Style := bsClear;
    canvas.Brush.Style := bsSolid;
    canvas.Pen.Style := psClear;
    canvas.Pen.Style := psSolid;
  end else if fIsThemed and ParentColor then
  begin
    ThemeServices.DrawParentBackground(Handle, canvas.Handle, nil, False, @rec);
  end else if fIsThemed and (Color = clBtnFace) then
  begin
    //nb: DrawElement will always shade Rec as if it's the element's clientrect
    Details := ThemeServices.GetElementDetails(ttBody);
    ThemeServices.DrawElement(canvas.Handle, Details, Rec);
  end else
  begin
    canvas.Brush.Color := self.Color;
    canvas.FillRect(rec);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Paint;
var
  i, ch: integer;
begin
  DrawBackground;

  if (csDesigning in ComponentState) then
  begin
    //when designing, display a few dummy items with Default values
    if ItemsCount = 0 then for i := 0 to 4 do AddItem;
    Items[1].SetFocus;
    ch := fVertSpace + fTopOffset;
    for i := 0 to 4 do with Items[i] do
    begin
      fHeight := fDefaultItemHeight;
      fColor := fDefaultItemColor;
      fItemStyle := fStyle;
      fVirtualTop := ch + i*(Height + fVertSpace);
    end;
    fSurfaceHeight := ch + 5*(fDefaultItemHeight + fVertSpace);
  end;

  canvas.Font.Assign(font);
  ch := clientHeight;
  for i := fIndexTopItem to ItemsCount -1 do
    with items[i] do if fVisible then
    begin
      Paint;
      if Top + Height > ch then break;
    end;

  PaintColDividers;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetStyle(Style: integer);
begin
  if not (Style in [0..3]) or (Style = fStyle) then exit;
  fStyle := Style;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetDefaultItemColor(Color: TColor);
begin
  if (fDefaultItemColor = Color) then exit;
  fDefaultItemColor := Color;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetDefaultItemHeight(Height: integer);
begin
  if (Height < 4) or (fDefaultItemHeight = Height) then exit;
  fDefaultItemHeight := Height;
  invalidate;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetItem(index: integer): TScrollItem;
begin
  if (index < 0) or (index >= fGuiItems.Count) then
    raise Exception.Create(rsGetItemRangeError);
  result := TScrollItem(fGuiItems[index]);
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetFocusedItem: TScrollItem;
begin
  if ItemsCount = 0 then
    result := nil else
    result := items[fFocusedIndex];
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetScrollItemClass(ScrollItemClass: TScrollItemClass);
begin
  if (ItemsCount > 0) then
    raise Exception.Create(rsSetItemClassError);
  fScrollItemClass := ScrollItemClass;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetItemEditor(ItemEditor: TItemEditor);
begin
  fItemEditor := ItemEditor;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.ClearItems;
var
  i: integer;
begin
  if assigned(fItemEditor) and
    (fItemEditor.Parent = self) and fItemEditor.Visible then fItemEditor.Hide;
  if fGuiItems.Count > 0 then
  begin
    for i := 0 to fGuiItems.Count -1 do TScrollItem(fGuiItems[i]).Free;
    fGuiItems.Clear;
    DoItemChange; //post message of changed (ie lost) item focus
  end;
  fFocusedIndex := 0;
  fSurfaceHeight := 0;
  fIndexTopItem := 0;
  Invalidate;
  if HandleAllocated then UpdateVScroll;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.BeginUpdate;
begin
  inc(fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.EndUpdate;
begin
  dec(fUpdateCount);
  if fUpdateCount > 0 then exit;
  UpdateVScroll;
  Repaint; //not sure why invalidate doesn't always work here
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.AddColDivider(XOffset: integer);
begin
  fColDividerList.Add(pointer(XOffset));
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.ClearColDividers;
begin
  fColDividerList.Clear;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.PaintColDividers;
var
  i: integer;
begin
  for i := 0 to fColDividerList.Count -1 do
  begin
    canvas.Pen.Color := clBtnShadow;
    canvas.moveto(integer(fColDividerList[i]),0);
    canvas.Lineto(integer(fColDividerList[i]),height);
    canvas.Pen.Color := clBtnHighlight;
    canvas.moveto(integer(fColDividerList[i])+1,0);
    canvas.Lineto(integer(fColDividerList[i])+1,height);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CollapseAll;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := 0 to ItemsCount -1 do
      if Items[i].TreeLevel = 0 then Items[i].Collapsed := true;

    if assigned(FocusedItem) then FocusedItem.ScrollIntoView;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.ExpandAll;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := 0 to ItemsCount -1 do
      if Items[i].TreeLevel = 0 then Items[i].Collapsed := false;

    if assigned(FocusedItem) then FocusedItem.ScrollIntoView;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.CancelEdit;
begin
  if assigned(fItemEditor) then fItemEditor.visible := false;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.EditItem;
begin
  Postmessage(handle, CM_SHOWEDITOR, 0, 0);
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.AddItem: TScrollItem;
begin
  if fScrollItemClass = nil then
    raise Exception.Create(rsAddItemError);

  result := fScrollItemClass.Create;
  result.fIndex := fGuiItems.Count;
  result.fHeight := fDefaultItemHeight;
  result.fColor := fDefaultItemColor; //self.Color;
  result.fItemStyle := fStyle;
  result.fScrollingCtrl := self;
  if result.fIndex = 0 then
  begin
    result.fVirtualTop := fVertSpace + fTopOffset;
    result.fFocused := true;
    fFocusedIndex := 0;
  end else
    result.fVirtualTop := fSurfaceHeight;

  fSurfaceHeight := result.fVirtualTop + result.Height + fVertSpace;
  fGuiItems.Add(result);
  if HandleAllocated then UpdateVScroll;
  invalidate;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.InsertItem(index: integer): TScrollItem;
var
  i: integer;
begin
  if fScrollItemClass = nil then
    raise Exception.Create(rsAddItemError);
  if index < 0 then index := 0
  else if index > fGuiItems.Count then index := fGuiItems.Count;
  if index = fGuiItems.Count then
  begin
    result := AddItem;
    exit;
  end;

  result := fScrollItemClass.Create;
  result.fColor := fDefaultItemColor; //self.Color;
  result.fItemStyle := fStyle;
  result.fScrollingCtrl := self;
  if fFocusedIndex >= index then inc(fFocusedIndex);
  for i := index to fGuiItems.Count -1 do
    TScrollItem(fGuiItems[i]).fIndex := i +1;
  result.fHeight := 0; //a simple workaround
  result.fVirtualTop := TScrollItem(fGuiItems[index]).fVirtualTop;
  result.fIndex := index;
  fGuiItems.Insert(index, result);
  result.SetHeight(fDefaultItemHeight);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DeleteItem(index: integer);
var
  i: integer;
begin
  if (index < 0) or (index >= ItemsCount) then
    raise Exception.Create(rsIndexRangeError);
  if items[index].HasChildren then
    raise Exception.Create(rsDeleteItemError);

  if assigned(fItemEditor) and assigned(fItemEditor.Item) and
    (fItemEditor.Item.Index = index) then
      with fItemEditor do
      begin
        //restore item height before deleting the item ...
        fItem.Height := fItemHeight;
        fItem := nil;
        visible := false;
      end;

  items[index].SetVisible(false) ; //a simple workaround
  items[index].Free;
  fGuiItems.Delete(index);
  for i := index to fGuiItems.Count -1 do
    TScrollItem(fGuiItems[i]).fIndex := i;
  if fFocusedIndex > index then dec(fFocusedIndex);
  ResetTopItem;
  invalidate;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.ItemsCount: integer;
begin
  result := fGuiItems.Count;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetHorzSpace(horzSpace: integer);
begin
  if (ItemsCount > 0) and not (csDesigning in ComponentState) then exit;
  fHorzSpace := horzSpace;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetVertSpace(vertSpace: integer);
begin
  if (ItemsCount > 0) and not (csDesigning in ComponentState) then exit;
  fVertSpace := vertSpace;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetTopOffset(Offset: integer);
begin
  if (ItemsCount > 0) and not (csDesigning in ComponentState) then exit;
  fTopOffset := Offset;
  invalidate;
end;
//------------------------------------------------------------------------------

end.
