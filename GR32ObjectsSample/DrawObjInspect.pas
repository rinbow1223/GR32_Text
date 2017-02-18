unit DrawObjInspect;

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
 * The Original Code is DrawObjInspect.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2009 the Initial Developer. All Rights Reserved.
 *
 * Version 1.1 (Last updated 17-Sep-09)
 *
 * END LICENSE BLOCK **********************************************************)

interface

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ScrollingCtrl, StdCtrls, ExtCtrls, TypInfo,
  GR32, GR32_Blend, GR32_Objects, ObjInspect;

type
  TItemKind = (ikNormal, ikColor, ikColor32, ikColor32Array,
    ikFontCharset, ikFixedPointArray, ikPseudoSet);

  TScrollItemDO = class(TScrollItemOI)
  private
    kind: TItemKind;
    ParentPropName: string;
    itemObject: TPersistent;
    intVal: integer; //used for tkInteger type properties
  protected
    procedure Paint; override;
  end;

  //nb: This ObjectInspector is highly customized for TDrawObjLayerBase classes
  TObjInspectorDO = class(TObjInspectorBase)
  private
    FObject: TPersistent;
    bmp32: TBitmap32;
    trueTypeFonts: TStringlist;
  protected
    procedure ValidateChange(Sender: TObject; var NewValue: string;
      item: TScrollItemOI; var IsValid: boolean);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetObject(obj: TPersistent);
    procedure Refresh;
    property DrawObj: TPersistent read FObject;
  end;

implementation

uses Math;

resourcestring
  rsTrue = 'True';
  rsFalse = 'False';
  rsInvalidDynArrayType = 'Unsupported Dynamic Array type.';

const

  FontCharsets: array[0..17] of TIdentMapEntry = (
    (Value: 0; Name: 'ANSI_CHARSET'),
    (Value: 1; Name: 'DEFAULT_CHARSET'),
    (Value: 2; Name: 'SYMBOL_CHARSET'),
    (Value: 77; Name: 'MAC_CHARSET'),
    (Value: 128; Name: 'SHIFTJIS_CHARSET'),
    (Value: 129; Name: 'HANGEUL_CHARSET'),
    (Value: 130; Name: 'JOHAB_CHARSET'),
    (Value: 134; Name: 'GB2312_CHARSET'),
    (Value: 136; Name: 'CHINESEBIG5_CHARSET'),
    (Value: 161; Name: 'GREEK_CHARSET'),
    (Value: 162; Name: 'TURKISH_CHARSET'),
    (Value: 177; Name: 'HEBREW_CHARSET'),
    (Value: 178; Name: 'ARABIC_CHARSET'),
    (Value: 186; Name: 'BALTIC_CHARSET'),
    (Value: 204; Name: 'RUSSIAN_CHARSET'),
    (Value: 222; Name: 'THAI_CHARSET'),
    (Value: 238; Name: 'EASTEUROPE_CHARSET'),
    (Value: 255; Name: 'OEM_CHARSET'));

  colorNames: array[0..47] of string =
    ('clBlack','clMaroon','clGreen','clOlive','clNavy', 'clPurple','clTeal',
    'clGray','clSilver','clRed','clLime','clYellow','clBlue','clFuschia',
    'clAqua','clLtGray','clDkGray','clWhite',
    'clScrollBar','clBackground','clActiveCaption','clInactiveCaption',
    'clMenu','clWindow','clWindowFrame','clMenuText','clWindowText',
    'clCaptionText','clActiveBorder','clInactiveBorder','clAppWorkSpace',
    'clHighlight','clHighlightText','clBtnFace','clBtnShadow','clGrayText',
    'clBtnText','clInactiveCaptionText','clBtnHighlight','cl3DDkShadow',
    'cl3DLight','clInfoText','clInfoBk','clHotLight','clGradientActiveCaption',
    'clGradientInactiveCaption','clMenuHighlight','clMenuBar');

  colorValues: array[0..47] of cardinal =
    ($000000,$000080,$008000,$008080,$800000,$800080,$808000,$808080,$C0C0C0,
    $0000FF,$00FF00,$00FFFF,$FF0000,$FF00FF,$FFFF00,$C0C0C0,$808080,$FFFFFF,
    clSystemColor or COLOR_SCROLLBAR, clSystemColor or COLOR_BACKGROUND,
    clSystemColor or COLOR_ACTIVECAPTION, clSystemColor or COLOR_INACTIVECAPTION,
    clSystemColor or COLOR_MENU, clSystemColor or COLOR_WINDOW,
    clSystemColor or COLOR_WINDOWFRAME, clSystemColor or COLOR_MENUTEXT,
    clSystemColor or COLOR_WINDOWTEXT, clSystemColor or COLOR_CAPTIONTEXT,
    clSystemColor or COLOR_ACTIVEBORDER, clSystemColor or COLOR_INACTIVEBORDER,
    clSystemColor or COLOR_APPWORKSPACE, clSystemColor or COLOR_HIGHLIGHT,
    clSystemColor or COLOR_HIGHLIGHTTEXT, clSystemColor or COLOR_BTNFACE,
    clSystemColor or COLOR_BTNSHADOW, clSystemColor or COLOR_GRAYTEXT,
    clSystemColor or COLOR_BTNTEXT, clSystemColor or COLOR_INACTIVECAPTIONTEXT,
    clSystemColor or COLOR_BTNHIGHLIGHT, clSystemColor or COLOR_3DDKSHADOW,
    clSystemColor or COLOR_3DLIGHT, clSystemColor or COLOR_INFOTEXT,
    clSystemColor or COLOR_INFOBK, clSystemColor or COLOR_HOTLIGHT,
    clSystemColor or COLOR_GRADIENTACTIVECAPTION,
    clSystemColor or COLOR_GRADIENTINACTIVECAPTION,
    clSystemColor or COLOR_MENUHILIGHT, clSystemColor or COLOR_MENUBAR);

  boolStr: array [boolean] of string = (rsFalse, rsTrue);

  ignoreList: array[0..21] of string =
    ('.FillColor', 'TFont.Height', 'TFont.Pitch',
    'TFontStyles.fsStrikeOut', 'TFontStyles.fsUnderline',
    'TDrawObjNonTextBase.Font', 'TDrawObjNonTextBase.TextPadding',
    'TDrawObjNonTextBase.Text', 'TDrawObjArc.Font',
    'TDrawObjArc.TextPadding', 'TDrawObjArc.Text', 'TDrawObjGraphic.Font',
    'TDrawObjGraphic.FillColors','TDrawObjGraphic.FillStyle',
    'TDrawObjGraphic.TextPadding', 'TDrawObjGraphic.Text',
    'TDrawObjGraphic.StrokeColor','TDrawObjGraphic.StrokeWidth',
    'TDrawObjGraphic.StrokeStyle', 'TDrawObjGraphic.ShadowColor',
    'TDrawObjGraphic.ShadowOffsetX', 'TDrawObjGraphic.ShadowOffsetY');

  COLORBOX_SIZE = 13;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function EnumFontFamiliesProc(var elf: TEnumLogFontEx; var ntm: TNewTextMetricExW;
  FontType: Integer; strings: TStrings): Integer; stdcall;
var
  tmp: string;
begin
  if FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE then
    with strings do
    begin
      tmp := elf.elfFullName;
      if (Count = 0) or (AnsiCompareText(strings[Count-1], tmp) <> 0) then
        Add(tmp);
  end;
  Result := 1;
end;
//------------------------------------------------------------------------------

procedure GetTrueTypeFonts(strings: TStrings);
var
  dc: HDC;
  lf: TLogFont;
begin
  dc := GetDC(0);
  try
    strings.BeginUpdate;
    FillChar(lf, sizeof(lf), 0);
    lf.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(dc, lf, @EnumFontFamiliesProc, LongInt(strings),0);
  finally
    ReleaseDC(0, dc);
    strings.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function StrToCardinalDef(const s: string; default: cardinal): cardinal;
var
  code: integer;
begin
  try
    Val(s, result, code);
    if code <> 0 then result := default;
  except
    result := default;
  end;
end;
//------------------------------------------------------------------------------

function DynArraySize(a: Pointer): Integer;
asm
        TEST EAX, EAX
        JZ   @@exit
        MOV  EAX, [EAX-4]
@@exit:
end;
//------------------------------------------------------------------------------

function DynArrayOfCardinalToString(DynArray: Pointer): string;
var
  i: integer;
begin
  result := '';
  i := DynArraySize(DynArray);
  if i = 0 then exit;
  for i := 1 to i-1 do
  begin
    result := result + format('$%8.8x, ',[PCardinal(DynArray)^]);
    inc(pchar(DynArray), 4);
  end;
  result := result + format('$%8.8x',[PCardinal(DynArray)^]);
end;
//------------------------------------------------------------------------------

function DynArrayOfFixedPointToString(DynArray: Pointer): string;
var
  i: integer;
begin
  result := '';
  i := DynArraySize(DynArray);
  if i = 0 then exit;
  for i := 1 to i-1 do
  begin
    result := result + format('(%1.2f,',[PInteger(DynArray)^ *FixedToFloat]);
    inc(pchar(DynArray), 4);
    result := result + format('%1.2f), ',[PInteger(DynArray)^ *FixedToFloat]);
    inc(pchar(DynArray), 4);
  end;
  result := result + format('(%1.2f,',[PInteger(DynArray)^ *FixedToFloat]);
  inc(pchar(DynArray), 4);
  result := result + format('%1.2f)',[PInteger(DynArray)^ *FixedToFloat]);
end;
//------------------------------------------------------------------------------

procedure StrToDynArrayOfCardinal(s: string; var DynArray: pointer);
var
  i, len, oldLen, commaCnt, valStartPos: integer;
  values: array of Cardinal;
  p: PCardinal;
begin
  values := nil;
  s := trim(s);
  len := length(s);
  if len > 0 then
  begin
    if not (s[len] in [',',';']) then
    begin
      s := s + ',';
      inc(len);
    end;
    commaCnt := 0;
    for i := 1 to len do
      if (s[i] in [',',';']) then
        inc(commaCnt);
    setlength(values, commaCnt);
    commaCnt := 0;
    valStartPos := 1;
    for i := 1 to len do
      if s[i] in [',',';'] then
      begin
        values[commaCnt] :=
          StrToCardinalDef(Copy(s,valStartPos, i-valStartPos), 0);
        valStartPos := i +1;
        inc(commaCnt);
      end;
  end;

  len := length(values); //nb: len reassigned
  if assigned(DynArray) then
  begin
    p := DynArray;
    dec(p);
    oldLen := p^;
    dec(p);
    if oldLen <> len then
      ReallocMem(p, sizeof(cardinal)*(len + 2));
    Inc(p);
    p^ := len; //ie new array length
    Inc(p);
  end else
  begin
    GetMem(p,sizeof(cardinal)*(len + 2));
    p^ := 1;   //reference counter
    inc(p);
    p^ := len; //array length
    Inc(p);
  end;
  DynArray := p;
  for i := 0 to len-1 do
  begin
    p^ := values[i];
    Inc(p);
  end;
end;
//------------------------------------------------------------------------------

function ColorNameFromVal(colorVal: integer): string;
var
  i: integer;
begin
  result := inttostr(colorVal);
  for i := low(ColorNames) to high(ColorNames) do
    if integer(colorValues[i]) = colorVal then
      begin
        result := ColorNames[i];
        break;
      end;
end;
//------------------------------------------------------------------------------

//IgnoreCheck: returns true if the property is to be ignored
function IgnoreCheck(const propName, parentPropName: string): boolean; overload;
var
  i,j: integer;
  s: string;
begin
  for i := low(ignoreList) to high(ignoreList) do
  begin
    j := pos('.'+propName, ignoreList[i]);
    result := (j > 0);
    if not result then continue;
    s := copy(ignoreList[i],1,j-1);
    result := (s = '') or AnsiSameStr(s, parentPropName);
    if result then break;
  end;
end;
//------------------------------------------------------------------------------

function IgnoreCheck(const propName: string; obj: TObject): boolean; overload;
var
  i,j: integer;
  typeInfo: PTypeInfo;
  s: string;
begin
  for i := low(ignoreList) to high(ignoreList) do
  begin
    j := pos('.'+propName, ignoreList[i]);
    result := (j > 0);
    if not result then continue;
    s := copy(ignoreList[i],1,j-1);
    if s = '' then exit;
    typeInfo := obj.ClassInfo;
    repeat
      result := AnsiSameStr(s, typeInfo^.Name);
      if result then exit;
      if typeInfo^.Name = 'TDrawObjLayerBase' then break;
      typeInfo := GetTypeData(typeInfo).ParentInfo^;
    until not assigned(typeInfo);
  end;
end;
//------------------------------------------------------------------------------

procedure MergeColorAndRect(bmp32: TBitmap32; color: TColor32);
var
  i: integer;
  c: PColor32;
begin
  with bmp32 do
  begin
    if Empty then exit;
    if AlphaComponent(color) = $FF then
      FillRect(0,0, width, height, color)
    else
    begin
      FillRect(0,0,COLORBOX_SIZE,COLORBOX_SIZE,clWhite32);
      c := @bits[0];
      for i := 0 to Width * Height -1 do
      begin
        MergeMem(color,c^);
        inc(c);
      end;
      EMMS;
    end;
    PenColor := clBlack32;
    MoveTo(0,0);
    LineToS(0,width-1);
    LineToS(height-1,width-1);
    LineToS(height-1,0);
    LineToS(0,0);
  end;
end;
//------------------------------------------------------------------------------

function Color32StrArrayToColor32Array(strClr32Array: string): TArrayOfColor32;
var
  i, len, commaCnt, valStartPos: integer;
begin
  result := nil;
  len := length(strClr32Array);
  if len = 0 then exit;
  if not (strClr32Array[len] in [',',';']) then
  begin
    strClr32Array := strClr32Array + ',';
    inc(len);
  end;
  commaCnt := 0;
  for i := 1 to len do
    if (strClr32Array[i] in [',',';']) then
      inc(commaCnt);
  setlength(result, commaCnt);
  commaCnt := 0;
  valStartPos := 1;
  for i := 1 to len do
    if strClr32Array[i] in [',',';'] then
    begin
      result[commaCnt] :=
        StrToCardinalDef(Copy(strClr32Array, valStartPos, i-valStartPos), 0);
      valStartPos := i +1;
      inc(commaCnt);
    end;
end;

//------------------------------------------------------------------------------
// TScrollItemDO methods
//------------------------------------------------------------------------------

procedure TScrollItemDO.Paint;
var
  i, l, divOff: integer;
  objInsp: TObjInspectorDO;
  rec, rec2: TRect;
  clrArray: TArrayOfColor32;
begin
  inherited Paint;
  objInsp := TObjInspectorDO(ScrollingCtrl);
  divOff := objInsp.DividerOffset;
  with objInsp.Canvas do
  begin
    l := 4+ BOX_SIZE+BOX_GAP*2 + TreeLevel*(BOX_SIZE +BOX_GAP);
    TextRect(Rect(l,Top+1, divOff-2, Top+height), l, Top+3, propName);
    l := divOff +3 + objInsp.ValueLeftMargin;
    if Kind in [ikColor,ikColor32,ikColor32Array] then
    begin
      rec := Rect(l, Top+4, l+COLORBOX_SIZE, Top+4+COLORBOX_SIZE);
      rec2 := Rect(0, 0, COLORBOX_SIZE, COLORBOX_SIZE);
      case Kind of
        ikColor32: MergeColorAndRect(objInsp.bmp32, cardinal(intVal));
        ikColor: MergeColorAndRect(objInsp.bmp32, Color32(intVal));
        ikColor32Array:
          begin
            clrArray := Color32StrArrayToColor32Array(propValue);
            for i := 0 to high(clrArray) do
            begin
              MergeColorAndRect(objInsp.bmp32, clrArray[i]);
              CopyRect(rec, objInsp.bmp32.Canvas, rec2);
              OffsetRect(rec,COLORBOX_SIZE+3,0);
            end;
            exit; //ie just the color boxes, skip the text
          end;
      end;
      CopyRect(rec, objInsp.bmp32.Canvas, rec2);
      inc(l, COLORBOX_SIZE+3);
    end;
    textout(l, Top+3, propValue);
  end;
end;

//------------------------------------------------------------------------------
// TObjInspectorDO methods
//------------------------------------------------------------------------------

constructor TObjInspectorDO.Create(aOwner: TComponent);
begin
  inherited;
  bmp32 := TBitmap32.Create;
  bmp32.SetSize(COLORBOX_SIZE, COLORBOX_SIZE);
  trueTypeFonts := TStringlist.Create;
  GetTrueTypeFonts(trueTypeFonts);
  trueTypeFonts.sorted := true;

  ScrollItemClass            := TScrollItemDO;
  ValueLeftMargin            := 2;
  OnValidateChange           := ValidateChange;
end;
//------------------------------------------------------------------------------

destructor TObjInspectorDO.Destroy;
begin
  trueTypeFonts.Free;
  bmp32.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorDO.SetObject(obj: TPersistent);

  procedure SetObjectInternal(obj: TPersistent; level: integer);
  var
    i,j,k: integer;
    setVal: cardinal;
    td : PTypeData;
    pl : PPropList;
    PropInfo: PPropInfo;
    typeInfo, ti: PTypeInfo;
    s, propInfoName: string;
    subObj: TObject;
    DynArrayTypeInfo: PDynArrayTypeInfo;
    DynArray: pointer;
  begin
    typeInfo := obj.ClassInfo;
    td := GetTypeData(typeInfo);
    GetMem (pl, td^.PropCount*SizeOf(Pointer));
    try
      GetPropInfos(typeInfo, pl);
      for i :=0 to td^.PropCount-1 do
      begin
        PropInfo := pl^[i];
        typeInfo := PropInfo.PropType^;
        propInfoName := PropInfo.name;

        //check for ignored properties ...
        if level = 0 then
        begin
          if IgnoreCheck(propInfoName, obj) then continue
        end else
          if IgnoreCheck(propInfoName, obj.ClassName) then continue;

        case typeInfo.Kind of
          tkEnumeration:
            with TScrollItemDO(AddItem) do
            begin
              itemObject := obj;
              propName := propInfoName;
              propValue := GetEnumProp(obj, PropInfo);
              TreeLevel := level;
              editType := etComboList;
              //now get the list of enum names
              with GetTypeData(typeInfo)^ do
                for j := MinValue to MaxValue do
                  ValueOptions.Add(GetEnumName(typeInfo, j));
            end;
          tkInteger:
            with TScrollItemDO(AddItem) do
            begin
              itemObject := obj;
              propName := propInfoName;
              intVal := GetOrdProp(obj, PropInfo);
              TreeLevel := level;
              editType := etEdit;
              if (obj.ClassName = 'TFont') and (propName = 'Charset') then
                kind := ikFontCharset
              else if pos('Color', propName) > 0 then kind := ikColor;
              if GetTypeData(typeInfo).OrdType = otULong then
              begin
                if kind = ikColor then kind := ikColor32;
                propValue := format('$%8.8x',[cardinal(intVal)]);
              end else
              begin
                propValue := inttostr(intVal);
                if kind = ikColor then
                begin
                  editType := etCombo;
                  for k := low(ColorNames) to high(ColorNames) do
                  begin
                    ValueOptions.Add(ColorNames[k]);
                    if intVal = integer(colorValues[k]) then
                      propValue := ColorNames[k];
                  end;
                end else if kind = ikFontCharset then
                begin
                  editType := etCombo;
                  for k := low(FontCharsets) to high(FontCharsets) do
                  begin
                    ValueOptions.Add(FontCharsets[k].Name);
                    if intVal = FontCharsets[k].Value then
                      propValue := FontCharsets[k].Name;
                  end;
                end;
              end;
            end;
          tkFloat:
            begin
              with TScrollItemDO(AddItem) do
              begin
                itemObject := obj;
                propName := propInfoName;
                propValue := format('%1.4g',[GetFloatProp(obj, PropInfo)]);
                TreeLevel := level;
                editType := etEdit;
              end;
            end;
          tkLString:
            with TScrollItemDO(AddItem) do
            begin
              itemObject := obj;
              propName := propInfoName;
              propValue := GetStrProp(obj, PropInfo);
              TreeLevel := level;
              if (obj.ClassName = 'TFont') and (propName = 'Name') then
              begin
                editType := etComboList;
                ValueOptions.Assign(trueTypeFonts);
              end else
                editType := etEdit;
            end;
          tkWString:
            with TScrollItemDO(AddItem) do
            begin
              itemObject := obj;
              propName := propInfoName;
              propValue := GetWideStrProp(obj, propInfo);
              TreeLevel := level;
              editType := etEdit;
            end;
          tkSet:
            with TScrollItemDO(AddItem) do
            begin
              itemObject := obj;
              propName := propInfoName;
              setVal := GetOrdProp(obj, PropInfo);
              propValue := GetSetProp(obj, PropInfo, true);
              TreeLevel := level;
              editType := etReadOnly;
              //now get the list of enum names
              ti := GetTypeData(typeInfo).CompType^;
              with GetTypeData(ti)^ do
                for j := MinValue to MaxValue do
                begin
                  s := GetEnumName(ti, j);
                  if IgnoreCheck(s, typeInfo.Name) then continue;
                  with TScrollItemDO(AddItem) do
                  begin
                    kind := ikPseudoSet;
                    ParentPropName := propInfoName;
                    itemObject := obj;
                    propName := GetEnumName(ti, j);
                    propValue := BoolStr[(1 shl j) and setVal <> 0];
                    TreeLevel := level +1;
                    editType := etComboList;
                    ValueOptions.Add(rsTrue);
                    ValueOptions.Add(rsFalse);
                  end;
                end;
            end;
          tkClass:
            begin
              subObj := TObject(GetOrdProp(obj, PropInfo));
              if assigned(subObj) then
              begin
                with TScrollItemDO(AddItem) do
                begin
                  itemObject := obj;
                  propName := propInfoName;
                  propValue := '('+subObj.ClassName+')';
                  TreeLevel := level;
                  editType := etReadOnly;
                end;
                if subObj is TComponent then
                  //implement TComponent when objects 'connect' to other objects
                else if (subObj is TPersistent) then
                  SetObjectInternal(TPersistent(subObj), level+1);
              end;
            end;
          tkDynArray:
            begin
              DynArrayTypeInfo := PDynArrayTypeInfo(PropInfo.PropType^);
              Inc(PChar(DynArrayTypeInfo), Length(DynArrayTypeInfo.name));
              //we're only interested in single dimensional arrays of integer
              if (DynArrayTypeInfo^.elType <> nil) then continue;
              DynArray := Pointer(GetOrdProp(obj, PropInfo));
              with TScrollItemDO(AddItem) do
              begin
                itemObject := obj;
                propName := propInfoName;
                if (propName = 'FillColors') and
                  (DynArrayTypeInfo^.varType = varInteger) and
                  (DynArrayTypeInfo^.elSize = 4) then
                begin
                  kind := ikColor32Array;
                  propValue := DynArrayOfCardinalToString(DynArray);
                end
                else if (propName = 'ControlBtns') and
                  (DynArrayTypeInfo^.elSize = 8) then
                begin
                  //shouldn't get here now - ControlBtns is no longer published
                  kind := ikFixedPointArray;
                  propValue := DynArrayOfFixedPointToString(DynArray);
                end
                else raise Exception.Create(rsInvalidDynArrayType);
                TreeLevel := level;
                editType := etEdit;
              end;
            end;
        end;
      end;
    finally
      FreeMem(pl);
    end;
  end;

begin
  if FObject = obj then exit;
  BeginUpdate;
  try
    ClearItems;
    FObject := obj;
    if assigned(obj) then SetObjectInternal(obj, 0);
    CollapseAll;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorDO.Refresh;
var
  i,j: integer;
  PropInfo: PPropInfo;
  typeInfo: PTypeInfo;
  setVal: cardinal;
  DynArrayTypeInfo: PDynArrayTypeInfo;
  DynArray: Pointer;
begin
  if not assigned(FObject) then exit;
  BeginUpdate;
  try
    for i := 0 to ItemsCount -1 do
      with TScrollItemDO(Items[i]) do
      begin

        //first see if this is a set pseudo-property item ...
        if (kind = ikPseudoSet) then
        begin
          PropInfo := GetPropInfo(itemObject, ParentPropName);
          setVal := GetOrdProp(itemObject, PropInfo);
          typeInfo := PropInfo.PropType^;
          typeInfo := GetTypeData(typeInfo).CompType^;
          j := GetEnumValue(typeInfo, propName);
          propValue := BoolStr[(1 shl j) and setVal <> 0];
          continue;
        end;

        PropInfo := GetPropInfo(itemObject, propName);
        typeInfo := PropInfo.PropType^;
        case typeInfo.Kind of
          tkEnumeration: propValue := GetEnumProp(itemObject, PropInfo);
          tkInteger:
            begin
              intVal := GetOrdProp(itemObject, PropInfo);
              if (kind = ikFontCharset) and CharsetToIdent(intVal,propValue) then
                //propValue assigned
              else if kind = ikColor then
                propValue := ColorNameFromVal(intVal)
              else if GetTypeData(typeInfo).OrdType = otULong then
                propValue := format('$%8.8x',[cardinal(intVal)])
              else
                propValue := inttostr(intVal);
            end;
          tkFloat: propValue := format('%1.4g',[GetFloatProp(itemObject, PropInfo)]);
          tkLString: propValue := GetStrProp(itemObject, PropInfo);
          tkWString: propValue := GetWideStrProp(itemObject, propInfo);
          tkSet: propValue := GetSetProp(itemObject, PropInfo, true);
          tkDynArray:
            begin
              DynArrayTypeInfo := PDynArrayTypeInfo(PropInfo.PropType^);
              Inc(PChar(DynArrayTypeInfo), Length(DynArrayTypeInfo.name));
              //nb: only single dimensional arrays needed here
              DynArray := Pointer(GetOrdProp(itemObject, PropInfo));
              if (DynArrayTypeInfo^.elSize = 4) then
                propValue := DynArrayOfCardinalToString(DynArray) else
                propValue := DynArrayOfFixedPointToString(DynArray);
            end;
        end;
      end;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectorDO.ValidateChange(Sender: TObject;
  var NewValue: string; item: TScrollItemOI; var IsValid: boolean);
var
  i,j,oldVal: integer;
  f: extended;
  PropInfo: PPropInfo;
  typeInfo: PTypeInfo;
  setVal: cardinal;
  DynArray: Pointer;
begin
  with TScrollItemDO(item) do
  begin
    //first, check if this is a set pseudo-property item ...
    if (kind = ikPseudoSet) then
    begin
      PropInfo := GetPropInfo(itemObject, ParentPropName);
      setVal := GetOrdProp(itemObject, PropInfo);
      typeInfo := PropInfo.PropType^;
      //get the typeInfo of the enumeration, not the set ...
      typeInfo := GetTypeData(typeInfo).CompType^;
      i := GetEnumValue(typeInfo, propName);
      if NewValue = rsTrue then
        setVal := setVal or (1 shl i) else
        setVal := setVal and not (1 shl i);
      SetOrdProp(itemObject, PropInfo, setVal);
    end
    else
    begin
      PropInfo := GetPropInfo(itemObject, propName);
      typeInfo := PropInfo.PropType^;
      case typeInfo.Kind of
        tkEnumeration: SetEnumProp(itemObject, PropInfo, NewValue);
        tkInteger:
          with GetTypeData(typeInfo)^ do
          begin
            if OrdType = otULong then
            begin
              {$R-}
              //these are TColor32 properties 
              cardinal(oldVal) := StrToCardinalDef(propValue, 0);
              cardinal(intVal) := StrToCardinalDef(NewValue, oldVal);
              {$R+}
            end
            else if (kind = ikFontCharset) and IdentToCharset(newValue, intVal) then
            begin
              if not IdentToCharset(propValue, oldVal) then
                oldVal := StrToIntDef(propValue, 0);
            end
            else if kind = ikColor then
            begin
              //nb: 1. these are TColor properties (not TColor32)
              //    2. intVal will be the stored integer value
              {$R-}
              j := ValueOptions.IndexOf(propValue);
              if j < 0 then
                oldVal := StrToIntDef(propValue, 0) else
                cardinal(oldVal) := colorValues[j];

              j := ValueOptions.IndexOf(newValue);
              if j < 0 then
                intVal := StrToIntDef(newValue, oldVal) else
                cardinal(intVal) := colorValues[j];
              {$R+}
            end else
            begin
              //plain old integers ...
              oldVal := StrToIntDef(propValue, 0);
              intVal := StrToIntDef(NewValue, oldVal);
            end;
            if (intVal = oldVal) then
            begin
              IsValid := false; //quite possibly a dud value
              exit;
            end;
            //OK, at least it's a valid integer but now set and get
            //the value in case NewValue is out of a defined range ...
            SetOrdProp(itemObject,PropInfo, intVal);
            intVal := GetOrdProp(itemObject,PropInfo);

            if (kind = ikFontCharset) and CharsetToIdent(intVal, newValue) then
              //newValue assigned
            else if kind = ikColor then
              NewValue := ColorNameFromVal(intVal)
            else if GetTypeData(typeInfo).OrdType = otULong then
              NewValue := format('$%8.8x',[cardinal(intVal)])
            else
              NewValue := inttostr(intVal);
          end;
        tkFloat:
          if TextToFloat(PChar(newValue), f, fvExtended) then
          begin
            SetFloatProp(itemObject,PropInfo, f);
            newValue := format('%1.4g',[GetFloatProp(itemObject, PropInfo)]);
          end else
            IsValid := false;
        tkLString: SetStrProp(itemObject, PropInfo, NewValue);
        tkWString: SetWideStrProp(itemObject, PropInfo, NewValue);
        tkDynArray:
          begin
            DynArray := Pointer(GetOrdProp(itemObject, PropInfo));
            StrToDynArrayOfCardinal(newValue, DynArray);
            //nb: DynArray may have been realloc'ed, so ...
            SetOrdProp(itemObject, PropInfo, longint(DynArray));
            //update NewValue to reflect any changes ...
            NewValue := DynArrayOfCardinalToString(DynArray);
          end;
      end;
    end;
    if not IsValid then exit;
    TDrawObjLayerBase(DrawObj).RePaint;
  end;
  //and since other properties may also have been updated ...
  Refresh;
end;
//------------------------------------------------------------------------------

end.
