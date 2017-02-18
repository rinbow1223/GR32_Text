unit MainUnit;

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  StdCtrls, ExtCtrls, ComCtrls, GR32_Image, Printers,
  {$IFDEF FPC} LResources, {$ENDIF}
  GR32, GR32_Lines, GR32_Text, GR32_Misc, GR32_Misc2, GR32_Polygons,
  GR32_Math, GR32_Blend, GR32_Transforms, GR32_Layers, GR32_Objects,
  GR32_ObjStore, AppEvnts, Menus, ClipBrd, DrawObjInspect, ActnList,
  XPStyleActnCtrls, ActnMan, ExtDlgs;

type

  TGetValueProc = procedure(value: integer) of object;

  TForm1 = class(TForm)
    PrintDialog1: TPrintDialog;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSaveAs: TMenuItem;
    N3: TMenuItem;
    mnuPrint: TMenuItem;
    N4: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    Zoom1: TMenuItem;
    mnuZoom50: TMenuItem;
    mnuZoom100: TMenuItem;
    mnuZoom200: TMenuItem;
    mnuZoom300: TMenuItem;
    mnuZoomPW: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ePropName: TEdit;
    ActionManager1: TActionManager;
    actBringFront: TAction;
    actSendBack: TAction;
    actExtendLine: TAction;
    BringtoFront2: TMenuItem;
    SendtoBack2: TMenuItem;
    ExtendLine1: TMenuItem;
    actAddLine: TAction;
    actAddBezier: TAction;
    actAddWBezier: TAction;
    actAddRect: TAction;
    actAddEllipse: TAction;
    actAddStar: TAction;
    actAddArrow: TAction;
    actAddArc: TAction;
    actAddGraphic: TAction;
    actAddDiamond: TAction;
    actAddPolygon: TAction;
    actAddPoint: TAction;
    AddLine1: TMenuItem;
    AddBezier1: TMenuItem;
    N7: TMenuItem;
    AddRectangle1: TMenuItem;
    AddEllipse1: TMenuItem;
    AddDiamond1: TMenuItem;
    AddStar1: TMenuItem;
    AddArc1: TMenuItem;
    AddArrow1: TMenuItem;
    AddGraphic1: TMenuItem;
    AddPoint1: TMenuItem;
    AddLine2: TMenuItem;
    AddBezier2: TMenuItem;
    N8: TMenuItem;
    AddRectangle2: TMenuItem;
    AddEllipse2: TMenuItem;
    AddDiamond2: TMenuItem;
    AddStar2: TMenuItem;
    AddArc2: TMenuItem;
    AddArrow2: TMenuItem;
    AddGraphic2: TMenuItem;
    AddPoint2: TMenuItem;
    N9: TMenuItem;
    actDelete: TAction;
    actSelectAll: TAction;
    Action1: TMenuItem;
    BringtoFront1: TMenuItem;
    SendtoBack1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    Delete1: TMenuItem;
    N5: TMenuItem;
    mnuCopyPageImage: TMenuItem;
    N10: TMenuItem;
    pnlMain: TPanel;
    Image: TImgView32;
    actCopyObjects: TAction;
    actPasteObjects: TAction;
    CopyObjects1: TMenuItem;
    PasteObjects1: TMenuItem;
    N6: TMenuItem;
    actRotate: TAction;
    Rotate1: TMenuItem;
    ExtendLine2: TMenuItem;
    actChangeImage: TAction;
    ChangeImage1: TMenuItem;
    Rotate2: TMenuItem;
    ChangeImage2: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImageResize(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuZoomPWClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure actSelectAllClick(Sender: TObject);
    procedure actDeleteClick(Sender: TObject);
    procedure actBringFrontExecute(Sender: TObject);
    procedure actSendBackExecute(Sender: TObject);
    procedure actExtendLineExecute(Sender: TObject);
    procedure actAddLineExecute(Sender: TObject);
    procedure pnlMainEnter(Sender: TObject);
    procedure pnlMainExit(Sender: TObject);
    procedure actCopyObjectsExecute(Sender: TObject);
    procedure actPasteObjectsExecute(Sender: TObject);
    procedure mnuCopyPageImageClick(Sender: TObject);
    procedure actChangeImageExecute(Sender: TObject);
    procedure actRotateExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  
    //GdipGraphic requires USE_GDIPLUS compiler directive set in GR32_Misc2.
    GdipGraphic: TGdipGraphic;

    savedAngle: integer;
    scrollbar_width: integer;
    page_orientation: TPrinterOrientation;
    lastDrawObj: TDrawObjLayerBase;
    SelectionRec: TRect;
    SelectionShape: TShape;
    procedure CountObjects(out count, indexLast: integer);
    procedure CountSelected(out count, indexLast: integer);
    procedure DesignerMoving(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState);
    procedure DesignerBtnMoving(Sender: TObject);
    procedure ValidateDragState(Sender: TObject; var dragState: integer);
    procedure DrawFocusRec(Rec: TRect);
    procedure AngleChange(value: integer);
  public
    popupPos: TPoint;
    OI: TObjInspectorDO;
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

{$R image.res}

//------------------------------------------------------------------------------
// Popup slider input form ...
//------------------------------------------------------------------------------

type
  TSliderForm = class(TForm)
  protected
    OnChangeEvent: TGetValueProc;
    procedure Event(Sender: TObject);
  end;

procedure TSliderForm.Event(Sender: TObject);
var
  s: string;
begin
  if (Sender is TTrackbar) and assigned(OnChangeEvent) then
  with TTrackbar(Sender) do
  begin
    OnChangeEvent(Position);
    s := copy(caption, 1, Pos('(',caption));
    caption := format('%s%d)',[s, Position]);
  end;
end;
//------------------------------------------------------------------------------

function GetSliderInput(const location: TPoint; const caption: string;
  min, current, max: integer; ticks: TArrayOfInteger; onChange: TGetValueProc): boolean;
var
  i: integer;
  form: TSliderForm;
  pnl: TPanel;
  OKbtn, CancelBtn: TButton;
  trackbar: TTrackbar;
begin
  form := TSliderForm.createNew(nil);
  try
    form.caption := format('%s (%d)',[caption, current]);
    form.Font.Name := 'Arial';
    form.Font.Size := 8;
    form.BorderStyle := bsToolWindow;
    form.OnChangeEvent := onChange;

    pnl := TPanel.create(form);
    pnl.parent := form;
    pnl.BevelOuter :=bvNone;
    pnl.align := alTop;
    pnl.height := 26;

    OKbtn := TButton.create(form);
    OKbtn.parent := pnl;
    OKbtn.top := 1;
    OKbtn.height := 22;
    OKbtn.caption := '&OK';
    OKbtn.ModalResult := mrOk;
    OKbtn.Default := true;

    cancelBtn := TButton.create(form);
    cancelBtn.parent := pnl;
    cancelBtn.top := 1;
    cancelBtn.Left := OKbtn.Width + 3;
    cancelBtn.height := 22;
    cancelBtn.caption := '&Cancel';
    cancelBtn.ModalResult := mrCancel;
    cancelBtn.Cancel := true;

    trackBar := TTrackBar.create(form);
    trackBar.parent := form;
    trackBar.align := alClient;
    trackBar.Min := min;
    trackBar.Max := max;
    trackBar.PageSize := math.max(1,(max-min) div 10);
    trackBar.Position := current;
    trackBar.TickStyle := tsManual;
    trackBar.OnChange := form.event;
    for i := 0 to high(ticks) do trackBar.SetTick(ticks[i]);
    form.ActiveControl := trackbar;
    with location do
      form.SetBounds(X,Y,200,86);
    result := form.ShowModal = mrOK;
    if not result and assigned(onChange) then onChange(current);
  finally
    form.Free;
  end;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

procedure SwapInts(var int1, int2: integer);
var
  tmp: integer;
begin
  tmp := int1;
  int1 := int2;
  int2 := tmp;
end;
//------------------------------------------------------------------------------

function NormalizeRect(r: TRect): TRect;
begin
  result := r;
  if r.Left > r.Right then
  begin
    result.Left := r.Right; result.Right := r.Left;
  end;
  if r.Top > r.Bottom then
  begin
    result.Top := r.Bottom; result.Bottom := r.Top;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  rec: TRect;
  pts: TArrayOfFixedPoint;
  rs: TResourceStream;
  printer_ppi: TSize;
  printer_size: TSize;
  printer_margin: TSize;
begin
  //width := 598; //just for the website screenshot

  OI := TObjInspectorDO.Create(self);
  OI.Parent := Panel2;
  OI.Align := alClient;

  scrollbar_width := GetSystemMetrics(SM_CXVSCROLL);

  //get printer page dimensions ...
  printer_ppi.cx := GetDeviceCaps(printer.Handle, LOGPIXELSX);
  printer_ppi.cy := GetDeviceCaps(printer.Handle, LOGPIXELSY);
  printer_size.cx := GetDeviceCaps(printer.Handle, PHYSICALWIDTH);
  printer_size.cy := GetDeviceCaps(printer.Handle, PHYSICALHEIGHT);
  printer_margin.cx := GetDeviceCaps(printer.Handle, PHYSICALOFFSETX);
  printer_margin.cy := GetDeviceCaps(printer.Handle, PHYSICALOFFSETY);
  printer_margin.cx :=
    round(printer_margin.cx * screen.PixelsPerInch/printer_ppi.cx);
  printer_margin.cy :=
    round(printer_margin.cY * screen.PixelsPerInch/printer_ppi.cY);
  if (printer_ppi.cx = 0) or (printer_ppi.cy = 0) or
    (printer_size.cx = 0) or (printer_size.cy = 0) then
  begin
    //probably no printer drivers installed ... defaults to A4
    printer_size.cx := round(210 * screen.PixelsPerInch/25.4);
    printer_size.cy := round(297 * screen.PixelsPerInch/25.4);
  end else
  begin
    printer_size.cx :=
      round(printer_size.cx * screen.PixelsPerInch/printer_ppi.cx);
    printer_size.cy :=
      round(printer_size.cy * screen.PixelsPerInch/printer_ppi.cy);
  end;
  printer.Orientation := poLandscape;
  page_orientation := printer.Orientation;
  if page_orientation = poLandscape then
  begin
    with printer_size do SwapInts(cx, cy);
    with printer_margin do SwapInts(cx, cy);
  end;

  //set the bitmap the the printer's page size ...
  with printer_size do Image.Bitmap.SetSize(cx, cy);
  Image.Bitmap.Clear($FFFFFFFF);

  //in case I want to draw transparent images directly onto Image.Bitmap ...
  Image.Bitmap.DrawMode := dmBlend;

  mnuZoomPWClick(mnuZoom100);

  //draw a pale gray outline of the image bitmap surface ...
  rec := Image.Bitmap.BoundsRect;
  dec(rec.Right);
  dec(rec.Bottom);
  pts := MakeArrayOfFixedPoints(rec);
  PolylineXS(Image.Bitmap, pts, clLightGray32, true);
  //now draw a faint printing margin on the page ...
  InflateRect(rec, -printer_margin.cx,-printer_margin.cy);
  pts := MakeArrayOfFixedPoints(rec);
  PolylineXS(Image.Bitmap, pts, $FFF4F4F4, true);

  //DRAW A NUMBER OF DRAWOBJECTS ...
  //Note: all these objects could instead be loaded from the accompanying
  //sample.dob file but they're loaded here to demo how that's done ...

  with TDrawObjGraphic.Create(Image.Layers) do
  begin
    //AllowResizing := false;      //defaults to true
    //ProportionalSizing := false; //defaults to true

    //nb: GDI+ is needed if you want to load JPG, GIF, PNG, ICO, TIF, EMF files.
    //To enable GDI+ support see GR32_Misc2.pas.
    rs := TResourceStream.Create(hInstance, 'BMP1', RT_RCDATA);
    try Position(FloatRect(20,165,120,310), 0, rs); finally rs.Free; end;
    Transparent := true;

    //nb: painting will only be done following an explicit call to Repaint (or
    //following an EndUpdate). This avoids wasting CPU cycles with unnecessary
    //repainting that might otherwise occur when changing object properties.
    Repaint;
  end;

  with TDrawObjPolygon.Create(Image.Layers) do
  begin
    Regular := true; //ie forces a symmetrical shape
    StrokeColor := $FF009900;
    FillColors := MakeArrayOfColor32([$FFFFFFEE,$CCFFFF88,$FF66FF66]);
    FillStyle := fsRadial;
    StrokeWidth := 4;
    ShadowOffset := 3;
    Position(FloatRect(280,190,380,290),8);
    Repaint;
  end;

  with TDrawObjPoint.Create(Image.Layers) do
  begin
    Radius := 4;
    StrokeColor := clRed32;
    FillColor := clRed32;
    Position(FloatPoint(31,43));
    ShadowOffset := 2;
    Repaint;
  end;

  with TDrawObjPoint.Create(Image.Layers) do
  begin
    Radius := 7;
    Position(FloatPoint(50,70));
    ShadowOffset := 2;
    Repaint;
  end;

  with TDrawObjRectangle.Create(Image.Layers) do
  begin
    StrokeColor := $0;

    //Notes on sub-pixel anti-aliasing ...
    //(see http://en.wikipedia.org/wiki/Subpixel_rendering)
    //When text is drawn onto a full transparent layer, the background color
    //for that text is undefined and makes sub-pixel anti-aliasing impossible.
    //This isn't a problem when printing but text can look a bit fuzzy without
    //sub-pixel anti-aliasing on LCD monitors.) We can hardcode a background
    //color by setting the FillColor almost but not entirely transparent.
    //With a fill color of $02FFFFFF, the background will appear transparent
    //but text will be anti-aliased as if the background was white. This
    //isn't usually a good idea unless you can be confident the end-user
    //won't be dragging the text over colored objects.
    FillColor := $02FFFFFF;

    Text := 'A rectangular object with no stroke or fill';
    Position(FloatRect(500,20,590,120), 0);
    Repaint;
  end;

  with TDrawObjEllipse.Create(Image.Layers) do
  begin
    StrokeColor := $CCAA4400;
    Text := 'an ellipse with'#10'radial filling,'#10'balloon tip and'#10'drop shadow';
    Font.Color := clNavy;
    FillColors := MakeArrayOfColor32([$CCFFFFFF,$CCDDDD44]);
    FillStyle := fsRadial;
    StrokeWidth := 3;
    BalloonPos := bpBottomLeft;
    ShadowOffset := 3;
    Position(FloatRect(100,110,240,200),0);
    Repaint;
  end;

  with TDrawObjDiamond.Create(Image.Layers) do
  begin
    Text := 'Gradient'#10'filling';
    Font.Name := 'Verdana';
    Font.Color := clTeal;
    Font.Size := 11;
    StrokeColor := $FF009900;
    FillColors := MakeArrayOfColor32([$CCFFFF88,$CCCCFF88,$CC66FF66]);
    FillStyle := fsGradiant;
    StrokeWidth := 4;
    ShadowOffset := 3;
    Position(FloatRect(220,30,380,130),0);
    Repaint;
  end;


  with TDrawObjArc.Create(Image.Layers) do
  begin
    StrokeColor := $CC000099;
    FillColors := MakeArrayOfColor32([$FFFFCCCC,$CCFFFF99,$80AAAAEE]);
    FillStyle := fsRadial;
    StrokeWidth := 3;
    ShadowOffset := 3;
    AngleStart := 45;
    AngleEnd := 0;
    Position(FloatRect(470,240,570,380),0);
    Repaint;
  end;

  with TDrawObjRectangle.Create(Image.Layers) do
  begin
    StrokeColor := $FF0000CC;
    StrokeStyle := psDash;
    Text := 'A semi- transparent rectangle with rounded corners';
    Rounded := true;
    //Regular := true; //ie force to a square whenever the designer buttons moved
    FillColor := $CCDDDDFF;
    StrokeWidth := 3;
    TextPadding := 4;
    Position(FloatRect(410,70,490,220), 30);
    Repaint;
  end;

  with TDrawObjLine.Create(Image.Layers) do
  begin
    ArrowStart.Style := asCircle;
    ArrowStart.Color := $30990000;
    ArrowEnd.Style := asThreePoint;
    ArrowEnd.Color := $30990000;
    Text := 'try this';
    StrokeColor := $FF990000;
    StrokeStyle := psDot;

    //see my comments on sub-pixel anti-aliasing above and try uncommenting the
    //FillColor statement below. (Also test this by moving the line over a
    //colored object.) With and without FillColor both have merits: if white,
    //the text usually looks a little clearer over white backgrounds but not so
    //good over colored objects; if the FillColor is fully transparent, the text
    //usually doesn't look quite a crisp on LCD monitors (though will still be
    //fine when printed).
    //FillColor := $FFFFFFFF;

    Font.Color := clMaroon;
    StrokeWidth := 3;
    Position(MakeArrayOfFloatPoints([260,150, 390,180]));
    Repaint;
  end;

  with TDrawObjStar.Create(Image.Layers) do
  begin
    StrokeColor := $FFAA0000;
    StrokeWidth := 3;
    Regular := true;
    FillColors := MakeArrayOfColor32([$80FFFF00, $FFFF6600]);
    FillStyle := fsRadial;
    Position(FloatPoint(200,270),10,60,7);
    ShadowOffset := 3;
    Repaint;
  end;

  with TDrawObjArrow.Create(Image.Layers) do
  begin
    StrokeColor := $FF006600;
    StrokeWidth := 3;
    FillColor := $3000FF00;
    Position(FloatRect(90,315,170,415), 60);
    ShadowOffset := 3;
    Repaint;
  end;

  with TDrawObjBezier.Create(Image.Layers) do
  begin
    ArrowEnd.Style := asThreePoint;
    ArrowEnd.Color := $30000066;
    Text := 'This is really cool';
    StrokeWidth := 3;
    StrokeColor := $CC000066;

    //see my comments on sub-pixel anti-aliasing above and try uncommenting the
    //FillColor statement below. (Also test this by moving the line over a
    //colored object.) With and without FillColor both have merits: if white,
    //the text usually looks a little clearer over white backgrounds but not so
    //good over colored objects; if the FillColor is fully transparent, the text
    //usually doesn't look quite a crisp on LCD monitors (though will still be
    //fine when printed).
    //FillColor := $FFFFFFFF;

    Position(MakeArrayOfFloatPoints([300,340, 370,300, 370,380, 450,350]));
    Repaint;
  end;

  with TDrawObjWideBezier.Create(Image.Layers) do
  begin
    StrokeWidth := 15;
    FillWidth := 9;
    StrokeColor := $CCAA3300;
    FillColors := MakeArrayOfColor32([$AAFFFF00, $99FF6600]);
    FillStyle := fsGradiant;
    ShadowOffset := 3;
    Position(MakeArrayOfFloatPoints([200,380, 270,300, 270,400, 320,390]));
    //Smooth := false;
    Repaint;
  end;

  //now for a bit of fun, let's load an animated gif ...
  GdipGraphic := TGdipGraphic.Create;
  rs := TResourceStream.Create(hInstance, 'GIF1', RT_RCDATA);
  GdipGraphic.LoadFromStream(rs);
  rs.Free;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GdipGraphic.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
const
  l = 25; t = 25;
begin
  //animate the GdipGraphic ...
  with GdipGraphic do
  begin
    //erase the background where the image is to be (re)drawn ...
    Image.Bitmap.FillRect(l,t,l+Width,t+Height, $FFFFFFFF);
    //draw the gif image ...
    Image.Bitmap.Draw(l,t,CurrentBitmap);
    //reset the timer interval ...
    Timer1.Interval := CurrentDelayInterval;
    //get ready for the next image in the loop ...
    NextBitmap;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainEnter(Sender: TObject);
begin
  pnlMain.Color := clActiveCaption;
  actDelete.ShortCut := ShortCut(VK_DELETE,[]);
  actCopyObjects.ShortCut := ShortCut(ord('C'),[ssCtrl]);
  actPasteObjects.ShortCut := ShortCut(ord('V'),[ssCtrl]);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainExit(Sender: TObject);
begin
  pnlMain.Color := clBtnFace;
  actDelete.ShortCut := 0;
  actCopyObjects.ShortCut := 0;
  actPasteObjects.ShortCut := 0;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageResize(Sender: TObject);
begin
  if mnuZoomPW.Checked then
  begin
    Image.Scale := (image.ClientWidth - scrollbar_width -20)/Image.Bitmap.Width;
    Image.ScrollToCenter(0,0);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuZoomPWClick(Sender: TObject);
begin
  if Sender = mnuZoom50 then
    Image.Scale := 0.5
  else if Sender = mnuZoom100 then
    Image.Scale := 1.0
  else if Sender = mnuZoom200 then
    Image.Scale := 2.0
  else if Sender = mnuZoom300 then
    Image.Scale := 3.0
  else if Sender = mnuZoomPW then
    Image.Scale := (image.ClientWidth - scrollbar_width -20)/Image.Bitmap.Width
  else
    exit; //oops

  mnuZoom50.Checked := false;
  mnuZoom100.Checked := false;
  mnuZoom200.Checked := false;
  mnuZoom300.Checked := false;
  mnuZoomPW.Checked := false;
  TMenuItem(Sender).Checked := true;

  Image.ScrollToCenter(0,0);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuOpenClick(Sender: TObject);
var
  i,cnt,last: integer;
  s: TFileStream;
begin
  //check if deleting existing objects ...
  CountObjects(cnt,last);
  if (cnt > 0) and
    (MessageBox(handle, 'Do you wish to delete existing objects first?',
      PChar(application.Title),
      MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES) then
        for i := Image.Layers.Count -1 downto 0 do
          image.Layers[i].Free;
  lastDrawObj := nil;
  OI.SetObject(nil);
  if not OpenDialog1.Execute then exit;

  //load new objects ...
  s := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
  try
    LoadObjectsFromStream(Image.Layers, s);
  finally
    s.free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSaveAsClick(Sender: TObject);
var
  i,cnt,last: integer;
  s: TFileStream;
begin
  CountObjects(cnt,last);
  if (cnt = 0) or not SaveDialog1.Execute then exit;
  s := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  try
    for i := 0 to Image.Layers.Count -1 do
      if (image.Layers[i] is TDrawObjLayerBase) then
      begin
        SaveObjectToStream(TDrawObjLayerBase(image.Layers[i]), s);
      end;
  finally
    s.free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuCopyPageImageClick(Sender: TObject);
var
  i: integer;
  bmp32: TBitmap32;
  bmp: TBitmap;
  size: TSize;
begin
  bmp32 := TBitmap32.Create;
  bmp := TBitmap.Create;
  try
    //get the dimensions of the new bitmap to place on the clipboard ...
    size.cx := 0; size.cy := 0;
    for i := 0 to Image.Layers.Count -1 do
      if (image.Layers[i] is TDrawObjLayerBase) then
        with MakeRect(TDrawObjLayerBase(image.Layers[i]).Location) do
        begin
          if Right > size.cx then size.cx := Right;
          if Bottom > size.cy then size.cy := Bottom;
        end;

    bmp32.SetSize(size.cx, size.cy);
    bmp32.Clear($FFFFFFFF);

    //draw the objects on a temporary bitmap32 ...
    for i := 0 to Image.Layers.Count -1 do
      if (image.Layers[i] is TDrawObjLayerBase) then
        with TDrawObjLayerBase(image.Layers[i]) do
          drawto(bmp32, FloatPoint(0,0), 1);

    //finally draw to the clipboard via a TBitmap ...
    bmp.Assign(bmp32);
    Clipboard.Open;
    Clipboard.Assign(bmp);
    Clipboard.Close;
  finally
    bmp32.Free;
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuPrintClick(Sender: TObject);
var
  i,printer_ppi: integer;
  bmp32: TBitmap32;
  bmp: TBitmap;
  scaleFactor: single;
begin
  Printer.Title := 'demo';
  if not PrintDialog1.Execute then exit;

  bmp32 := TBitmap32.Create;
  bmp := TBitmap.Create;
  try
    printer.BeginDoc;

    //get the scaling factor ...
    printer_ppi := GetDeviceCaps(printer.Canvas.Handle, LOGPIXELSX);
    scaleFactor := printer_ppi/Screen.PixelsPerInch;

    bmp32.SetSize(round(Image.Bitmap.Width*scaleFactor),
      round(Image.Bitmap.Height*scaleFactor));
    bmp32.Clear($00FFFFFF);

    //draw the objects ...
    for i := 0 to Image.Layers.Count -1 do
      if (image.Layers[i] is TDrawObjLayerBase) then
        with TDrawObjLayerBase(image.Layers[i]) do
          drawto(bmp32, FloatPoint(0,0), scaleFactor);

    //draw to the printer ...
    bmp.Assign(bmp32);
    Printer.Canvas.Draw(0,0,bmp);
    printer.EndDoc;
  finally
    bmp32.Free;
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Image.Layers.Count -1 do
    if (Image.Layers[i] is TDrawObjLayerBase)
      and not assigned(TDrawObjLayerBase(Image.Layers[i]).Designer) then
        with TDesignerLayer.Create(image.Layers) do
        begin
          OnMoving := DesignerMoving;
          OnButtonMoving := DesignerBtnMoving;
          OnValidateDragState := ValidateDragState;
          ChildLayer := TDrawObjLayerBase(Image.Layers[i]);
        end;
end;
//------------------------------------------------------------------------------

procedure TForm1.DrawFocusRec(Rec: TRect);
begin
  if not Assigned(SelectionShape) then
  begin
    //simple selection 'rubber-banding' ...
    SelectionShape := TShape.create(self);
    SelectionShape.Parent := Image;
    SelectionShape.Brush.Style := bsClear;
    SelectionShape.Pen.Style := psDot;
  end;
  with Rec do
    SelectionShape.SetBounds(left,top,right-left, bottom-top);
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

  procedure ClearDesigners;
  var
    i: integer;
  begin
    for i := Image.Layers.Count -1 downto 0 do
      if image.Layers[i] is TDesignerLayer then
        image.Layers[i].Free;
  end;

var
  cnt, last: integer;
begin
  if Layer is TDesignerLayer then
  begin
    if (ssCtrl in Shift) then Layer.Free;

    CountSelected(cnt, last);
    if cnt = 1 then
    begin
      OI.SetObject(image.Layers[last]);
      ePropName.Text := OI.DrawObj.ClassName;
    end else
    begin
      OI.SetObject(nil);
      ePropName.Text := '';
    end;
    exit;
  end
  else if Layer is TDrawObjLayerBase then
  begin
    if ([ssShift, ssCtrl] * Shift = []) then ClearDesigners;
    with TDesignerLayer.Create(image.Layers) do
    begin
      OnMoving := DesignerMoving;
      OnButtonMoving := DesignerBtnMoving;
      OnValidateDragState := ValidateDragState;
      ChildLayer := TDrawObjLayerBase(Layer);
    end;
    CountSelected(cnt, last);
    if cnt = 1 then
    begin
      OI.SetObject(image.Layers[last]);
      ePropName.Text := OI.DrawObj.ClassName;
    end else
    begin
      OI.SetObject(nil);
      ePropName.Text := '';
    end;
  end else
  begin
    ClearDesigners;
    //start selection rubber-banding ...
    SelectionRec := Rect(X,Y,X,Y);
    if (ssLeft in Shift) then DrawFocusRec(SelectionRec);
    OI.SetObject(nil);
    ePropName.Text := '';
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  if not (ssLeft in Shift) or not Assigned(SelectionShape) then exit;
  SelectionRec.BottomRight := Point(X,Y);
  DrawFocusRec(NormalizeRect(SelectionRec));
end;
//------------------------------------------------------------------------------

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  rec2,rec3: TRect;
  i, cnt, last: integer;
  selectionFixedRect: TFixedRect;
begin
  if not Assigned(SelectionShape) then exit;
  FreeAndNil(SelectionShape);
  with SelectionRec do if (abs(right-left) < 5) and (abs(bottom-top) < 5) then
    exit; //ie assume it's a simple form click.

  //create Designer layers for each DrawObject that resides inside
  //the selection rectangle ...
  selectionFixedRect := FixedRect(NormalizeRect(SelectionRec));
  ScreenToBitmap(Image.Layers, selectionFixedRect.TopLeft);
  ScreenToBitmap(Image.Layers, selectionFixedRect.BottomRight);
  rec2 := MakeRect(selectionFixedRect);
  for i := 0 to Image.Layers.Count -1 do
    if (Image.Layers[i] is TDrawObjLayerBase) then
    begin
      rec3 := MakeRect(TDrawObjLayerBase(Image.Layers[i]).Location);
      if IntersectRect(rec3, rec2, rec3) then
        with TDesignerLayer.Create(image.Layers) do
        begin
          OnMoving := DesignerMoving;
          OnButtonMoving := DesignerBtnMoving;
          OnValidateDragState := ValidateDragState;
          ChildLayer := TDrawObjLayerBase(Image.Layers[i]);
        end;
    end;
  CountSelected(cnt, last);
  if cnt = 1 then
  begin
    OI.SetObject(image.Layers[last]);
    ePropName.Text := image.Layers[last].ClassName;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.DesignerMoving(Sender: TObject; const OldLocation: TFloatRect;
  var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState);
var
  i: integer;
  loc: TFloatRect;
begin
  //move all the selected objects to their new locations ...
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDesignerLayer) and (Sender <> image.Layers[i]) then
      with TDesignerLayer(image.Layers[i]) do
      begin
        loc := Location;
        OffsetFloatRect(loc,
          round(newLocation.Left-OldLocation.Left),
          round(newLocation.top-OldLocation.Top));
        Location := loc;
      end;
  OI.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.DesignerBtnMoving(Sender: TObject);
begin
  OI.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.ValidateDragState(Sender: TObject; var dragState: integer);
var
  cnt,last: integer;
begin
  //if the mouse is over an object control button, make sure only one object is
  //selected before allowing the designer to proceed with button moves ...
  CountSelected(cnt, last);
  //nb: dragState >= 0 means the mouse is over a control button ...
  if (dragState >= 0) and (cnt <> 1) then dragState := DRAG_MOVE;
end;
//------------------------------------------------------------------------------

procedure TForm1.CountObjects(out count, indexLast: integer);
var
  i: integer;
begin
  count := 0;
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDrawObjLayerBase) then
    begin
      inc(count);
      indexLast := i;
    end;
end;
//------------------------------------------------------------------------------

procedure TForm1.CountSelected(out count, indexLast: integer);
var
  i: integer;
begin
  count := 0;
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDesignerLayer) then
    begin
      inc(count);
      indexLast := TDesignerLayer(image.Layers[i]).ChildLayer.Index;
    end;
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  cnt, selectedCnt, last: integer;
begin
  //get the point where new objects will be located ...
  if Sender = PopupMenu1 then
  begin
    GetCursorPos(popupPos);
    popupPos := Image.ScreenToClient(popupPos);
  end else
    popupPos := Point(20,20);
  ScreenToBitmap(Image.Layers, popupPos);

  CountObjects(cnt, last);
  mnuCopyPageImage.Enabled := cnt > 0;
  CountSelected(selectedCnt, last);
  if selectedCnt = 1 then
    lastDrawObj := TDrawObjLayerBase(image.Layers[last]) else
    lastDrawObj := nil;
  actSendBack.enabled := selectedCnt > 0;
  actBringFront.enabled := selectedCnt > 0;
  actExtendLine.Enabled := assigned(lastDrawObj) and
    ((lastDrawObj is TDrawObjLine) or (lastDrawObj is TDrawObjWideBezier));
  actCopyObjects.Enabled := selectedCnt > 0;
  actRotate.Enabled := selectedCnt = 1;
  actChangeImage.Enabled :=
    (selectedCnt = 1) and (lastDrawObj is TDrawObjGraphic);
end;
//------------------------------------------------------------------------------

procedure TForm1.actCopyObjectsExecute(Sender: TObject);
var
  i, cnt, last: integer;
  ss: TStringStream;
begin
  CountSelected(cnt, last);
  if cnt = 0 then exit;
  ss := TStringStream.Create('');
  try
    for i := 0 to Image.Layers.Count -1 do
      if (image.Layers[i] is TDesignerLayer) then
        SaveObjectToStream(TDesignerLayer(image.Layers[i]).ChildLayer, ss);
    Clipboard.Open;
    Clipboard.AsText := ss.DataString;
    Clipboard.Close;
  finally
    ss.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actPasteObjectsExecute(Sender: TObject);
var
  ss: TStringStream;
begin
  Clipboard.Open;
  try
    if not Clipboard.HasFormat(CF_TEXT) then exit;
    ss := TStringStream.Create(Clipboard.AsText);
    try
      LoadObjectsFromStream(Image.Layers, ss);
    finally
      ss.Free;
    end;
  finally
    Clipboard.Close;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actBringFrontExecute(Sender: TObject);
begin
  if assigned(lastDrawObj) then
  begin
    lastDrawObj.BringToFront;
    lastDrawObj.Designer.BringToFront;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actSendBackExecute(Sender: TObject);
begin
  if assigned(lastDrawObj) then
  begin
    lastDrawObj.Designer.SendToBack;
    lastDrawObj.SendToBack;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actExtendLineExecute(Sender: TObject);
begin
  if not assigned(lastDrawObj) then exit;
  if (lastDrawObj is TDrawObjLine) then
    TDrawObjLine(lastDrawObj).Extend(true)
  else if (lastDrawObj is TDrawObjWideBezier) then
    TDrawObjWideBezier(lastDrawObj).Extend(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.actDeleteClick(Sender: TObject);
var
  i: integer;
begin
  for i := Image.Layers.Count -1 downto 0 do
    if (image.Layers[i] is TDrawObjLayerBase) then
      with TDrawObjLayerBase(image.Layers[i]) do
      begin
        if OI.DrawObj = image.Layers[i] then
        begin
          OI.SetObject(nil);
          ePropName.Text := '';
        end;
        if not assigned(Designer) then continue;
        Designer.free;
        free;
      end;
end;
//------------------------------------------------------------------------------

procedure TForm1.actAddLineExecute(Sender: TObject);
var
  i: integer;
  obj: TDrawObjLayerBase;
  fs: TFileStream;
begin
  if Sender = actAddLine then
    obj := TDrawObjLine.create(image.Layers)
  else if Sender = actAddBezier then
    obj := TDrawObjBezier.create(image.Layers)
  else if Sender = actAddWBezier then
    obj := TDrawObjWideBezier.create(image.Layers)
  else if Sender = actAddRect then
    obj := TDrawObjRectangle.create(image.Layers)
  else if Sender = actAddEllipse then
    obj := TDrawObjEllipse.create(image.Layers)
  else if Sender = actAddStar then
    obj := TDrawObjStar.create(image.Layers)
  else if Sender = actAddArrow then
    obj := TDrawObjArrow.create(image.Layers)
  else if Sender = actAddArc then
    obj := TDrawObjArc.create(image.Layers)
  else if Sender = actAddGraphic then
  begin

    if not OpenPictureDialog1.Execute then exit;
    obj := TDrawObjGraphic.create(image.Layers);
    fs := TFileStream.Create(OpenPictureDialog1.FileName, fmOpenRead);
    try
      with popupPos do
        TDrawObjGraphic(obj).Position(FloatRect(X,Y,X,Y),0,fs);
    finally
      fs.Free;
    end;
  end
  else if Sender = actAddDiamond then
    obj := TDrawObjDiamond.create(image.Layers)
  else if Sender = actAddPolygon then
    obj := TDrawObjPolygon.create(image.Layers)
  else if Sender = actAddPoint then
  begin
    obj := TDrawObjPoint.create(image.Layers);
    with popupPos do
      TDrawObjPoint(obj).Position(FloatPoint(X,Y)); //otherwise too close to corner
  end
  else exit;
  if not (obj is TDrawObjLine) then obj.FillColor := $8030FF88;
  obj.Left := popupPos.X;
  obj.Top := popupPos.Y;
  obj.RePaint;

  //clear current selection ...
  for i := Image.Layers.Count -1 downto 0 do
    if image.Layers[i] is TDesignerLayer then
      image.Layers[i].Free;

  //make new control the selection ...
  with TDesignerLayer.Create(image.Layers) do
  begin
    OnMoving := DesignerMoving;
    OnButtonMoving := DesignerBtnMoving;
    OnValidateDragState := ValidateDragState;
    ChildLayer := obj;
  end;
  OI.SetObject(obj);
  ePropName.Text := obj.ClassName;

end;
//------------------------------------------------------------------------------

procedure TForm1.actChangeImageExecute(Sender: TObject);
var
  fs: TFileStream;
begin
  if not (lastDrawObj is TDrawObjGraphic) or not OpenPictureDialog1.Execute then exit;
  fs := TFileStream.Create(OpenPictureDialog1.FileName, fmOpenRead);
  try
    TDrawObjGraphic(lastDrawObj).SetImage(fs);
    lastDrawObj.RePaint;
  finally
    fs.Free;
  end;
  OI.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.actRotateExecute(Sender: TObject);
var
  pt: TPoint;
begin
  if assigned(lastDrawObj) then
    if (lastDrawObj is TDrawObjTextBase) then
      with TDrawObjTextBase(lastDrawObj) do
      begin
        GetCursorPos(pt);
        GetSliderInput(pt,'Set Angle ...',-180,
          round(angle), 180, nil, AngleChange);
      end
    else if (lastDrawObj is TDrawObjGraphic) then
      with TDrawObjGraphic(lastDrawObj) do
      begin
        GetCursorPos(pt);
        GetSliderInput(pt,'Set Angle ...',-180,
          round(angle), 180, nil, AngleChange);
      end
    else with lastDrawObj do
    begin
      GetCursorPos(pt);
      GetSliderInput(pt,'Rotate ...',-180, 0, 180, nil, AngleChange);
      savedAngle := 0;
    end;
end;
//------------------------------------------------------------------------------

procedure TForm1.AngleChange(value: integer);
begin
  if not assigned(lastDrawObj) then exit;

  if lastDrawObj is TDrawObjTextBase then
    TDrawObjTextBase(lastDrawObj).Angle := value
  else if lastDrawObj is TDrawObjGraphic then
    TDrawObjGraphic(lastDrawObj).Angle := value
  else
  begin
    lastDrawObj.Rotate(value-savedAngle);
    savedAngle := value;
  end;
  lastDrawObj.RePaint;
  OI.Refresh;
end;
//------------------------------------------------------------------------------

end.
