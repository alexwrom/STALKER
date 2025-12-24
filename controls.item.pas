unit controls.item;

interface

uses System.SysUtils, System.Types, System.UITypes, System.classes,
  System.Variants, FMX.Objects, FMX.StdCtrls, FMX.Layouts,
  FMX.Types, FMX.controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.Effects;

type
  TOnClickRightButton = procedure(AObject: TObject) of object;
  TOnItemClick = procedure(FTagObject: TObject; FItem: TFMXObject = nil) of object;

  TControlItem = class(TLayout)
    rcBackground: TRectangle;
    recIcon: TCircle;
    recIconBackground: TCircle;
    recIconBackgroundTransp: TCircle;
    layClient: TLayout;
    labLeadingText: TLabel;
    labCenterText: TLabel;
    layTop: TLayout;
    labDetailText: TLabel;
    Effect: TShadowEffect;
    crButtonRight: TCircle;
    btnButtonRight: TSpeedButton;
    Timer: TTimer;
    AnimIcon: TAniIndicator;
    Inner: TInnerGlowEffect;
  private
    FTime: integer;
    FCenterText: string;
    FOnClickRightButton: TOnClickRightButton;
    FOnItemClick: TOnItemClick;
    FObjectItem: TObject;
    procedure SetCenterText(AText: string);
    procedure SelfHeightUpdate;
    procedure OnClickRightButton(Sender: TObject);
    procedure OnDownItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnTimer(Sender: TObject);
    procedure OnUpItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnLeave(Sender: TObject);
    procedure SetObjectItem(const Value: TObject);
  public
    labFooterText: TLabel;

    function GetImage: TBitmap;

    property OnClickButton: TOnClickRightButton read FOnClickRightButton write FOnClickRightButton;
    property OnItemClick: TOnItemClick read FOnItemClick write FOnItemClick;
    property CenterText: string read FCenterText write SetCenterText;
    constructor Create(AOwner: TFMXObject; AddRightButtom: Boolean = false); overload;

  end;

implementation

{ TCartOfReservation }

constructor TControlItem.Create(AOwner: TFMXObject; AddRightButtom: Boolean = false);
begin
  inherited Create(AOwner);

  with Self do
  begin
    Parent := AOwner;
    Align := TAlignLayout.Top;
    Height := 50;
    Width := (AOwner as TVertScrollBox).Width;
    Position.Y := 100000;
    Padding.Top := 15;
    Padding.Left := 2;
    Padding.Bottom := 5;
    Padding.Right := 10;
    ShowHint := false;
  end;
  // Background
  rcBackground := TRectangle.Create(nil);
  with rcBackground do
  begin
    Align := TAlignLayout.Contents;
    Parent := Self;
    Fill.Color := $01000000;
    XRadius := 10;
    YRadius := 10;
    ShowHint := false;
    Padding.Top := 5;
    Padding.Bottom := 5;
    Padding.Left := 55;
    Padding.Right := 5;
    Margins.Top := 25;
    Margins.Bottom := 5;
    Margins.Left := 50;
    Margins.Right := 15;
    Stroke.Kind := TBrushKind.None;
    OnMouseDown := OnDownItem;
    OnMouseUp := OnUpItem;
    OnMouseLeave := OnLeave;
    OnMouseMove := OnMove;

  end;


  // Icon

  layTop := TLayout.Create(nil);
  with layTop do
  begin
    Parent := Self;
    Align := TAlignLayout.Top;
    Height := 75;
    Margins.Left := 15;
    Margins.Top := -10;
    HitTest := false;
  end;

  recIconBackgroundTransp := TCircle.Create(nil);
  with recIconBackgroundTransp do
  begin
    Parent := layTop;
    Align := TAlignLayout.MostLeft;
    Stroke.Kind := TBrushKind.None;
    Fill.Kind := TBrushKind.None;
    Fill.Color := $FF282828;
    Width := Height;
    HitTest := false;
  end;

  recIconBackground := TCircle.Create(nil);
  with recIconBackground do
  begin
    Parent := recIconBackgroundTransp;
    Align := TAlignLayout.Client;
    Stroke.Kind := TBrushKind.None;
    Fill.Kind := TBrushKind.None;
    Width := Height;
    Margins.Right := 5;
    Margins.Top := 5;
    Margins.Bottom := 5;
    Margins.Left := 5;
    HitTest := false;
  end;

  Effect := TShadowEffect.Create(nil);
  Effect.Parent := recIconBackground;

  recIcon := TCircle.Create(nil);
  with recIcon do
  begin
    Parent := recIconBackground;
    Align := TAlignLayout.Client;
    Stroke.Kind := TBrushKind.None;
    Margins.Right := 3;
    Margins.Top := 3;
    Margins.Bottom := 3;
    Margins.Left := 3;
    Fill.Kind := TBrushKind.Bitmap;
    Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    HitTest := false;
  end;

  Inner := TInnerGlowEffect.Create(nil);
  with Inner do
  begin
    Parent := AnimIcon;
    GlowColor := $FFFF4E45;
    Opacity := 1;
    Softness := 8;
  end;

  layClient := TLayout.Create(nil);
  with layClient do
  begin
    Parent := rcBackground;
    Align := TAlignLayout.Client;
    HitTest := false;
  end;

  // Center Text
  labCenterText := TLabel.Create(nil);
  with labCenterText do
  begin
    Parent := layClient;
    Align := TAlignLayout.Client;
    StyledSettings := [];
    TextSettings.FontColor := TAlphacolors.Aliceblue;
    TextSettings.Font.Style := [TFontStyle.fsBold];
    TextSettings.Font.Family := 'YouTube Sans Dark';
    Font.Size := 14;
    AutoSize := true;
    WordWrap := true;
    Height := 0;
    HitTest := false;
  end;

end;


procedure TControlItem.SetObjectItem(const Value: TObject);
begin
  FObjectItem := Value;
end;

procedure TControlItem.SelfHeightUpdate;
var
  lHeight: Single;
begin
  lHeight := labCenterText.Height + Self.Padding.Top + Self.Padding.Bottom + rcBackground.Padding.Top + rcBackground.Padding.Bottom;
  if lHeight < 80 then
    Self.Height := 80
  else
    Self.Height := lHeight;
end;

procedure TControlItem.SetCenterText(AText: string);
begin
  labCenterText.Text := AText;
  SelfHeightUpdate;
end;

function TControlItem.GetImage: TBitmap;
begin
  Result := recIcon.Fill.Bitmap.Bitmap;
end;

procedure TControlItem.OnClickRightButton(Sender: TObject);
begin
  FOnClickRightButton(Self.TagObject);
end;

procedure TControlItem.OnDownItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  (Self.Owner as TVertScrollBox).HelpKeyword := (Self.ConvertLocalPointTo((Self.Owner as TVertScrollBox), TPointF.Create(X, Y)).Y + 5).ToString;
  rcBackground.Fill.Color := $FFFF8707;
  FTime := 0;
  Timer := TTimer.Create(nil);
  Timer.Interval := 20;
  Timer.OnTimer := OnTimer;
  Timer.Enabled := true;

end;

procedure TControlItem.OnUpItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  rcBackground.Fill.Color := $01000000;
  Timer.Enabled := false;
  Timer.DisposeOf;
  FOnItemClick(Self.TagObject, Self);
end;

procedure TControlItem.OnMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  lObject: TFMXObject;
begin
  rcBackground.Fill.Color := $FFFF8707;

  if FTime >= 200 then
    if (ssLeft in Shift) then
    begin
      for lObject in (Self.Parent as TScrollContent).Children do
        if lObject is TControlItem then
          if (lObject as TControlItem).rcBackground.HitTest then
            (lObject as TControlItem).rcBackground.HitTest := false;
      (Self.Owner as TVertScrollBox).HelpKeyword := (Self.ConvertLocalPointTo((Self.Owner as TVertScrollBox), TPointF.Create(X, Y)).Y + 5).ToString;
    end;
end;

procedure TControlItem.OnLeave(Sender: TObject);
begin
  rcBackground.Fill.Color := $01000000;
end;

procedure TControlItem.OnTimer(Sender: TObject);
begin
  FTime := FTime + 20;
end;

end.
