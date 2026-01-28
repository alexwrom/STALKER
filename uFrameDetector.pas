unit uFrameDetector;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Ani, FMX.Media, uScanerWiFi,
  FMX.Controls.Presentation, System.IOUtils, uGlobal, FMX.Layouts, Generics.Collections, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  System.Sensors, System.ImageList, FMX.ImgList;

type

  TFrameDetector = class(TFrame)
    imgDetector: TImage;
    recSignalOtklik: TRectangle;
    animDetectorOtklik: TFloatAnimation;
    TimerSensor: TTimer;
    MediaPlayer: TMediaPlayer;
    recBackForIndicator: TRectangle;
    animLightOtklick: TFloatAnimation;
    timerScannerArtefacts: TTimer;
    GlowEffect1: TGlowEffect;
    layOtklik: TLayout;
    layBear: TLayout;
    Image1: TImage;
    GlowEffect2: TGlowEffect;
    pieBearSignal: TPie;
    cicleBackBear: TCircle;
    animDetectorBear: TFloatAnimation;
    animLightBear: TFloatAnimation;
    layVilka: TLayout;
    imgDetectorVilka: TImage;
    recBackForIndicatorVilka: TRectangle;
    labDisplayVilka: TLabel;
    recSkin: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    ShadowEffect1: TShadowEffect;
    layVeles: TLayout;
    ShadowEffect2: TShadowEffect;
    Image2: TImage;
    layVelesDisplay: TLayout;
    MapLayout: TLayout;
    ImageList: TImageList;
    animVeles: TFloatAnimation;
    LocationMarker: TLayout;
    OrientationMarker: TImage;
    ShadowEffect3: TShadowEffect;
    procedure TimerSensorTimer(Sender: TObject);
    procedure timerScannerArtefactsTimer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FArtDistance: integer;
    FKoeff: TPointF;
    FCoords: TLocationCoord2D;
    FMarkerList: TList<TMarkerData>;

    function ScanDistanceToArtefacts: double;

    procedure CreateMarker(AMarker: TMarkerData);
    procedure SetMarker(AMarker: TImage; Lat, Lon: double);
    function CoordinatesToPixels(Lat, Lon: double): TPointF;
    procedure MyLocationCenter;


    { Private declarations }
  public
    FCurrentScale: double;
    procedure LoadDetector;
    procedure UpdateAnomalies;
    procedure UpdateArtefacts;
    constructor Create(AObject: TFmxObject);
    { Public declarations }
  end;

implementation

uses
  System.Permissions;
{$R *.fmx}

constructor TFrameDetector.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  FMarkerList := TList<TMarkerData>.Create;

  FKoeff.Y := 0.2566;
  FKoeff.X := 0.5210;
  LoadDetector;
  MediaPlayer.FileName := TPath.Combine(GetUserAppPath, 'detector_art.mp3');
  labDisplayVilka.TextSettings.Font.Family := 'lcd';
end;

procedure TFrameDetector.LoadDetector;
begin
   case Person.Detector.Level of
    1:
      begin
        layOtklik.Visible := true;
        layBear.Visible := false;
        layVilka.Visible := false;
        layVeles.Visible := false;
      end;
    2:
      begin
        layOtklik.Visible := false;
        layBear.Visible := true;
        layVilka.Visible := false;
        layVeles.Visible := false;
      end;
    3:
      begin
        layVilka.Visible := true;
        layBear.Visible := false;
        layOtklik.Visible := false;
        layVeles.Visible := false;
      end;
    4:
      begin
        layVilka.Visible := false;
        layBear.Visible := false;
        layOtklik.Visible := false;
        layVeles.Visible := true;
      end;
  end;
end;

procedure TFrameDetector.TimerSensorTimer(Sender: TObject);
begin
  if Not FIsDead then
  begin
{$IF Defined(ANDROID)}
    Vibration(100);
{$ENDIF}
    if Person.Detector.Level = 4 then
      animVeles.Start;

    if FArtDistance <= Person.Detector.Radius then
    begin
      case Person.Detector.Level of
        1:
          begin
            MediaPlayer.CurrentTime := 0;
            animDetectorOtklik.Start;
            animLightOtklick.Start;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
        2:
          begin

            case FArtDistance of
              0 .. 2:
                begin
                  pieBearSignal.EndAngle := 360;
                  pieBearSignal.StartAngle := 0;
                end;
              3 .. 5:
                begin
                  pieBearSignal.EndAngle := -222;
                  pieBearSignal.StartAngle := 42;
                end;
              6 .. 9:
                begin
                  pieBearSignal.EndAngle := -186;
                  pieBearSignal.StartAngle := 6;
                end;
              10 .. 15:
                begin
                  pieBearSignal.EndAngle := -134;
                  pieBearSignal.StartAngle := -45;
                end;
            else
              begin
                pieBearSignal.EndAngle := -80;
                pieBearSignal.StartAngle := -99;
              end;
            end;

            MediaPlayer.CurrentTime := 0;
            animDetectorBear.Start;
            animLightBear.Start;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
        3:
          begin
            labDisplayVilka.Text := FArtDistance.ToString.PadLeft(4, '0');
            MediaPlayer.CurrentTime := 0;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
        4:
          begin
            labDisplayVilka.Text := FArtDistance.ToString.PadLeft(4, '0');
            MediaPlayer.CurrentTime := 0;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
      end;
    end;
  end;
end;

procedure TFrameDetector.FrameResize(Sender: TObject);
begin
  pieBearSignal.Height := Self.Height - 40;
  pieBearSignal.Width := pieBearSignal.Height;
  recBackForIndicatorVilka.Height := Self.Height * FKoeff.Y;
  recBackForIndicatorVilka.Width := recBackForIndicatorVilka.Height / FKoeff.X;
  recBackForIndicatorVilka.Margins.Left := 171 / (136 / recBackForIndicatorVilka.Height);
  recBackForIndicatorVilka.Margins.Bottom := 74 / (136 / recBackForIndicatorVilka.Height);
  labDisplayVilka.TextSettings.Font.Size := 120 / (136 / recBackForIndicatorVilka.Height);

  layVelesDisplay.Height := 215 / (420 / Self.Height);
  layVelesDisplay.Width := layVelesDisplay.Height / (215 / 400);
  layVelesDisplay.Margins.Bottom := 80 / (420 / Self.Height);
end;

procedure TFrameDetector.UpdateAnomalies;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := 0 to FAnomalyList.Count - 1 do
  begin
    FCoords := FAnomalyList[I].Coords;
    AMarker.Coords := FCoords;

    if FAnomalyList[I].AnomalyType = atRadiation then
      AMarker.MarkerType := mtRadiation
    else
      AMarker.MarkerType := mtAnomaly;

    AMarker.Index := I;
    CreateMarker(AMarker);
  end;
end;

procedure TFrameDetector.UpdateArtefacts;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := 0 to FArtefactsList.Count - 1 do
  begin
    FCoords := FArtefactsList[I].Coords;
    AMarker.Coords := FCoords;

    AMarker.MarkerType := mtArtefact;

    CreateMarker(AMarker);
  end;
end;

function TFrameDetector.ScanDistanceToArtefacts: double;
var
  I: integer;
  vDistance: double;
  vMinDistance: double;
begin
  vMinDistance := Person.Detector.Radius + 1;

  if FLocation.Latitude <> 0 then
    for I := 0 to FArtefactsList.Count - 1 do
    begin
      if FArtefactsList[I].Level <= Person.Detector.Level then
      begin
        vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FArtefactsList[I].Coords.Latitude, FArtefactsList[I].Coords.Longitude);

        if (vDistance <= vMinDistance) then
          vMinDistance := vDistance;
      end;
    end;

  result := vMinDistance;
end;

procedure TFrameDetector.CreateMarker(AMarker: TMarkerData);
  procedure CreateBackground;
  begin
    with TCircle.Create(AMarker.Marker) do
    begin
      Parent := AMarker.Marker;
      Align := TAlignLayout.Client;
      fill.Kind := TBrushKind.Solid;
      fill.Color := TAlphaColors.Alpha;
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := TAlphaColors.White;
      HitTest := false;
      Opacity := 0.5;
    end;
  end;

  procedure CreateIcon(ABitmap: TBitmap; AWidth: integer = 30);
  begin
    with TImage.Create(AMarker.Marker) do
    begin
      Parent := AMarker.Marker;
      Align := TAlignLayout.Center;
      Opacity := 1;
      Bitmap.Assign(ABitmap);
      Width := AWidth;
      Height := Width;
      HitTest := false;
    end;
  end;

begin
  AMarker.Marker := TImage.Create(MapLayout);
  AMarker.Marker.Parent := MapLayout;
  AMarker.Marker.Width := 20;
  AMarker.Marker.Height := AMarker.Marker.Width;

  case AMarker.MarkerType of
    mtArtefact:
      begin
        AMarker.Marker.Bitmap.Assign(ImageList.Source[6].MultiResBitmap[0].Bitmap);
      end;
    mtAnomaly, mtRadiation:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Height := AMarker.Marker.Width;

        CreateBackground;

        case FAnomalyList[AMarker.Index].AnomalyType of
          atRadiation:
            CreateIcon(ImageList.Source[1].MultiResBitmap[0].Bitmap);
          atElectro:
            CreateIcon(ImageList.Source[2].MultiResBitmap[0].Bitmap);
          atFire:
            CreateIcon(ImageList.Source[4].MultiResBitmap[0].Bitmap);
          atPhisic:
            CreateIcon(ImageList.Source[5].MultiResBitmap[0].Bitmap);
          atChimishe:
            CreateIcon(ImageList.Source[3].MultiResBitmap[0].Bitmap);
          atPSI:
            CreateIcon(ImageList.Source[0].MultiResBitmap[0].Bitmap);
        end;
      end;
  end;

  SetMarker(AMarker.Marker, FCoords.Latitude, FCoords.Longitude);

  FMarkerList.Add(AMarker);
end;

procedure TFrameDetector.SetMarker(AMarker: TImage; Lat, Lon: double);
var
  Point: TPointF;
begin
  Point := CoordinatesToPixels(Lat, Lon);

  // Учитываем масштаб при позиционировании маркера
  Point.X := Point.X * FCurrentScale;
  Point.Y := Point.Y * FCurrentScale;

  // Позиционируем маркер
  AMarker.Position.X := Point.X - AMarker.Width / 2;
  AMarker.Position.Y := Point.Y - AMarker.Height / 2;
  AMarker.BringToFront;
end;

procedure TFrameDetector.MyLocationCenter;
var
  Point: TPointF;
begin
  Point := CoordinatesToPixels(FLocation.Latitude, FLocation.Longitude);
  Point.X := Point.X * FCurrentScale;
  Point.Y := Point.Y * FCurrentScale;

  MapLayout.Position.X := layVelesDisplay.Width / 2 - Point.X;
  MapLayout.Position.Y := layVelesDisplay.Height / 2 - Point.Y - 20;

  OrientationMarker.RotationAngle := GetOrientation;
end;

function TFrameDetector.CoordinatesToPixels(Lat, Lon: double): TPointF;
var
  X, Y: double;
begin

  // Преобразование долготы в X координату
  X := ((Lon - FTopLeftLon) / (FBottomRightLon - FTopLeftLon)) * FOriginalMapWidth;

  // Преобразование широты в Y координату (инвертируем, т.к. координаты идут сверху)
  Y := ((Lat - FTopLeftLat) / (FBottomRightLat - FTopLeftLat)) * FOriginalMapHeight;

  result := TPointF.Create(X, Y);
end;

procedure TFrameDetector.timerScannerArtefactsTimer(Sender: TObject);
begin
  // Запрос разрешений для Android
{$IFDEF ANDROID}
  if Not FIsDead then
  begin
    MyLocationCenter;

    TThread.CreateAnonymousThread(
      procedure
      begin
        // ScanNetworks;    // по WIFI

        TThread.Synchronize(nil,
          procedure
          begin
            FArtDistance := Round(ScanDistanceToArtefacts);

            if FArtDistance <= Person.Detector.Radius then
            begin
              if Person.Detector.Level = 4 then
                TimerSensor.Interval := 1000
              else
              begin
                case FArtDistance of
                  0 .. 2:
                    TimerSensor.Interval := 200;
                  3 .. 5:
                    TimerSensor.Interval := 800;
                  6 .. 9:
                    TimerSensor.Interval := 1500;
                  10 .. 15:
                    TimerSensor.Interval := 2200;
                else
                  TimerSensor.Interval := 5000;
                end;
              end;
              TimerSensor.Enabled := true;
            end
            else
              TimerSensor.Enabled := false;
          end);
      end).Start;
  end;
{$ENDIF}
end;

end.
