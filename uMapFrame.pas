unit uMapFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, System.Math.Vectors,
  FMX.Edit, FMX.Ani, FMX.Gestures, FMX.VirtualKeyboard, FMX.Platform,
  System.Math, System.Sensors, System.Sensors.Components, System.Permissions,
  FMX.Effects, Generics.Collections, System.ImageList, FMX.ImgList,
  System.Actions, FMX.ActnList, uGlobal, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Media, System.IOUtils;

type
  TMapFrame = class(TFrame)
    ScrollBox: TScrollBox;
    MapLayout: TLayout;
    MapImage: TImage;
    ManMarker: TCircle;
    ZoomLayout: TLayout;
    btnZoomIn: TButton;
    btnZoomOut: TButton;
    btnResetZoom: TButton;
    GestureManager: TGestureManager;
    LocationMarker: TLayout;
    LayMapControls: TLayout;
    TimerSensor: TTimer;
    LocationSensor: TLocationSensor;
    OrientationMarker: TImage;
    btnMyLocation: TButton;
    lblZoom: TLabel;
    MarkersPanel: TImage;
    ImageList: TImageList;
    Layout1: TLayout;
    labMarkerCount: TLabel;
    btnAddMarker: TSpeedButton;
    btnAddMarkerRad: TSpeedButton;
    btnAddMarkerAnomaly: TSpeedButton;
    btnAddMarkerBag: TSpeedButton;
    ActionList: TActionList;
    ActAddMarker: TAction;
    layDetailIssue: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    labIssueText: TLabel;
    labIssueDetail: TLabel;
    LayDetailMarker: TLayout;
    Rectangle3: TRectangle;
    labMarkerText: TLabel;
    btnDelMarker: TSpeedButton;
    Image2: TImage;
    gplDeleteYesNo: TGridPanelLayout;
    btnDeleteNo: TSpeedButton;
    btnDeleteYes: TSpeedButton;
    Rectangle4: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    InnerGlowEffect2: TInnerGlowEffect;
    InnerGlowEffect3: TInnerGlowEffect;
    MediaPlayerRad: TMediaPlayer;
    MediaPlayerAnomaly: TMediaPlayer;
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure btnMyLocationClick(Sender: TObject);
    procedure MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ActAddMarkerExecute(Sender: TObject);
    procedure btnDelMarkerClick(Sender: TObject);
    procedure btnDeleteNoClick(Sender: TObject);
    procedure btnDeleteYesClick(Sender: TObject);
    procedure TimerSensorTimer(Sender: TObject);
  private
    FMapLoaded: Boolean;
    FOriginalMapWidth: Double;
    FOriginalMapHeight: Double;
    FCurrentScale: Double;
    FMinScale: Double;
    FMaxScale: Double;
    FZoomStep: Double;
    FTopLeftLat: Double;
    FTopLeftLon: Double;
    FBottomRightLat: Double;
    FBottomRightLon: Double;
    FLastDistance: integer;
    FIsZooming: Boolean;
    FLongTap: TPointF;
    FCoords: TLocationCoord2D;
    FMarkerList: TList<TMarkerData>;
    FMarkerIssue: TList<TMarkerData>;
    FAnomalyList: TList<TAnomalyData>;
    procedure LoadMapFromFile(const AFileName: string);
    procedure SetLocationMarker(Lat, Lon: Double);
    function CoordinatesToPixels(Lat, Lon: Double): TPointF;
    function PixelsToCoordinates(X, Y: Single): TLocationCoord2D;
    procedure UpdateMapBounds;
    procedure UpdateZoomControls;
    procedure ApplyZoom(ACenterX: Single = -1; ACenterY: Single = -1);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure SetZoom(AScale: Double; ACenterX: Single = -1; ACenterY: Single = -1);
    procedure SetupAndroidSpecifics;
    function CalculateBearing(const StartPoint, EndPoint: TLocationCoord2D): Double;
    procedure SetMarker(AMarker: TImage; Lat, Lon: Double);
    procedure CreateMarker(AMarker: TMarkerData);
    procedure ResetLocationMarkers;
    procedure OnMarkerClick(Sender: TObject);
    procedure UpdateIssue;
    procedure OnMarkerIssueClick(Sender: TObject);
    function GetNumberMarker(AMarker: TImage): integer;
    procedure ScrollToImageAdvanced(ScrollBox: TScrollBox; Image: TImage; Center: Boolean = True; Animate: Boolean = False);
    procedure ScanAnomalies;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Установка границ карты (координаты углов)
    procedure SetMapBounds(TopLeftLat, TopLeftLon, BottomRightLat, BottomRightLon: Double);

    // Масштабирование
    procedure ZoomToPoint(APoint: TPointF; AScale: Double);

    // Свойства только для чтения
    property MapLoaded: Boolean read FMapLoaded;
    property CurrentScale: Double read FCurrentScale;
  end;

implementation

{$R *.fmx}
{$IFDEF ANDROID}

uses
  FMX.Platform.Android;
{$ENDIF}

constructor TMapFrame.Create(AOwner: TComponent);
begin
  inherited;

  // Настройки для Android
  SetupAndroidSpecifics;

  FMapLoaded := False;
  FCurrentScale := 1.0;
  FMinScale := 0.1;
  FMaxScale := 5.0;
  FZoomStep := 0.3; // Увеличиваем шаг для тач-интерфейса

  LocationMarker.Visible := False;

  // Настройка жестов
  ScrollBox.Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan, TInteractiveGesture.DoubleTap];
  ScrollBox.Touch.GestureManager := GestureManager;

  UpdateZoomControls;

  FMarkerList := TList<TMarkerData>.Create;
  FMarkerIssue := TList<TMarkerData>.Create;
  FAnomalyList := TList<TAnomalyData>.Create;

  LoadMapFromFile(''); // Переписать

  UpdateIssue;
  ResetLocationMarkers;

{$IFDEF ANDROID}
  MediaPlayerRad.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zvuk-radiacii.mp3');
  MediaPlayerAnomaly.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'detector.mp3');
{$ENDIF}
end;

procedure TMapFrame.UpdateIssue;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := 0 to FIssueList.Count - 1 do
  begin
    FCoords := FIssueList[I].Coords;
    AMarker.Coords := FCoords;
    AMarker.MarkerType := mtIssue;

    AMarker.LabelText := FIssueList[I].Name;
    AMarker.LabelDetail := FIssueList[I].Detail;

    CreateMarker(AMarker);
  end;
end;

destructor TMapFrame.Destroy;
begin
  inherited;
end;

procedure TMapFrame.SetupAndroidSpecifics;
begin
  // Увеличиваем размеры элементов для тач-интерфейса
  if TOSVersion.Platform = pfAndroid then
  begin
    // Настройка для лучшей производительности
    ScrollBox.EnableDragHighlight := False;
    MapImage.HitTest := True;
  end;
end;

procedure TMapFrame.ActAddMarkerExecute(Sender: TObject);
var
  AMarker: TMarkerData;
begin
  AMarker.Coords := FCoords;

  case (Sender as TSpeedButton).Tag of
    0:
      begin
        AMarker.MarkerType := mtPoint;
        AMarker.LabelText := 'Моя точка';
      end;
    1:
      begin
        AMarker.MarkerType := mtRad;
        AMarker.LabelText := 'Радиация';
      end;
    2:
      begin
        AMarker.MarkerType := mtAnomaly;
        AMarker.LabelText := 'Аномалия';
      end;
    3:
      begin
        AMarker.MarkerType := mtBag;
        AMarker.LabelText := 'Схрон';
      end;
  end;

  CreateMarker(AMarker);
  MarkersPanel.Visible := False;
end;

procedure TMapFrame.LoadMapFromFile(const AFileName: string);
begin
  try
    // MapImage.Bitmap.LoadFromFile(AFileName);
    FMapLoaded := True;

    // Сохраняем оригинальные размеры
    FOriginalMapWidth := MapImage.Bitmap.Width;
    FOriginalMapHeight := MapImage.Bitmap.Height;

    // Устанавливаем размер Image под размер карты
    MapImage.Width := FOriginalMapWidth;
    MapImage.Height := FOriginalMapHeight;
    MapLayout.Width := FOriginalMapWidth;
    MapLayout.Height := FOriginalMapHeight;

    // Сбрасываем масштаб
    SetZoom(1.0);

    // Обновляем границы карты
    UpdateMapBounds;

  except
    on E: Exception do
    begin
      ShowMessage('Ошибка загрузки карты: ' + E.Message);
      FMapLoaded := False;
    end;
  end;
end;

procedure TMapFrame.LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
begin
  if NewLocation.Latitude <> 0 then
  begin
    FLocation := NewLocation;
    LocationMarker.Visible := True;

    SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
    OrientationMarker.RotationAngle := 135 + CalculateBearing(OldLocation, FLocation);
  end;
end;

procedure TMapFrame.ScanAnomalies;
var
  I: integer;
  vDistance: Double;
begin

  for I := 0 to FAnomalyList.Count - 1 do
  begin
    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[I].Coords.Latitude, FAnomalyList[I].Coords.Longitude);

    if vDistance <= FAnomalyList[I].Radius then
    begin
      if FAnomalyList[I].AnomalyType = atRadiation then
      begin
        MediaPlayerRad.CurrentTime := 0;
        MediaPlayerRad.Volume := vDistance / FAnomalyList[I].Radius * 100;
        MediaPlayerRad.Play;
      end
      else
      begin
        MediaPlayerAnomaly.CurrentTime := 0;
        MediaPlayerAnomaly.Volume := vDistance / FAnomalyList[I].Radius * 100;
        MediaPlayerAnomaly.Play;
      end;

      Person.Health := Person.Health - FAnomalyList[I].Power * (vDistance / FAnomalyList[I].Radius);
    end;
  end;
end;

function TMapFrame.CalculateBearing(const StartPoint, EndPoint: TLocationCoord2D): Double;
var
  Lat1, Lon1, Lat2, Lon2: Double;
  dLon, X, Y: Double;
begin
  // Конвертируем градусы в радианы
  Lat1 := DegToRad(StartPoint.Latitude);
  Lon1 := DegToRad(StartPoint.Longitude);
  Lat2 := DegToRad(EndPoint.Latitude);
  Lon2 := DegToRad(EndPoint.Longitude);

  // Разница долгот
  dLon := Lon2 - Lon1;

  // Расчет азимута
  Y := Sin(dLon) * Cos(Lat2);
  X := Cos(Lat1) * Sin(Lat2) - Sin(Lat1) * Cos(Lat2) * Cos(dLon);

  Result := RadToDeg(ArcTan2(Y, X));

  // Нормализуем результат в диапазон 0-360
  Result := (Result + 360.0);
  if Result >= 360.0 then
    Result := Result - 360.0;
end;

procedure TMapFrame.SetLocationMarker(Lat, Lon: Double);
var
  Point: TPointF;
begin
  if not FMapLoaded then
    Exit;

  Point := CoordinatesToPixels(Lat, Lon);

  // Учитываем масштаб при позиционировании маркера
  Point.X := Point.X * FCurrentScale;
  Point.Y := Point.Y * FCurrentScale;

  // Позиционируем маркер
  LocationMarker.Position.X := Point.X - LocationMarker.Width / 2;
  LocationMarker.Position.Y := Point.Y - LocationMarker.Height / 2;
  LocationMarker.Visible := True;
  LocationMarker.BringToFront;
end;

function TMapFrame.CoordinatesToPixels(Lat, Lon: Double): TPointF;
var
  X, Y: Double;
begin
  if not FMapLoaded then
    Exit(TPointF.Zero);

  // Преобразование долготы в X координату
  X := ((Lon - FTopLeftLon) / (FBottomRightLon - FTopLeftLon)) * FOriginalMapWidth;

  // Преобразование широты в Y координату (инвертируем, т.к. координаты идут сверху)
  Y := ((Lat - FTopLeftLat) / (FBottomRightLat - FTopLeftLat)) * FOriginalMapHeight;

  Result := TPointF.Create(X, Y);
end;

function TMapFrame.PixelsToCoordinates(X, Y: Single): TLocationCoord2D;
var
  Lat, Lon: Double;
begin
  if not FMapLoaded then
    Exit;

  // Учитываем масштаб
  X := X / FCurrentScale;
  Y := Y / FCurrentScale;

  // Преобразование X координаты в долготу
  Lon := FTopLeftLon + (X / FOriginalMapWidth) * (FBottomRightLon - FTopLeftLon);

  // Преобразование Y координаты в широту
  Lat := FTopLeftLat + (Y / FOriginalMapHeight) * (FBottomRightLat - FTopLeftLat);

  Result := TLocationCoord2D.Create(Lat, Lon);
end;

procedure TMapFrame.MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // if Button = TMouseButton.mbLeft then
  // begin
  // Получаем координаты по клику
  FCoords := PixelsToCoordinates(FLongTap.X, FLongTap.Y);
  btnAddMarker.Enabled := FMarkerList.Count < 10;
  btnAddMarkerRad.Enabled := FMarkerList.Count < 10;
  btnAddMarkerBag.Enabled := FMarkerList.Count < 10;
  btnAddMarkerAnomaly.Enabled := FMarkerList.Count < 10;
  labMarkerCount.Text := FMarkerList.Count.ToString + '/10';
  // Устанавливаем маркер
  SetMarker(MarkersPanel, FCoords.Latitude, FCoords.Longitude);
  MarkersPanel.BringToFront;
end;

procedure TMapFrame.SetMarker(AMarker: TImage; Lat, Lon: Double);
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

  AMarker.Visible := True;
end;

procedure TMapFrame.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MarkersPanel.Visible := False;
  LayDetailMarker.Visible := False;
  layDetailIssue.Visible := False;
  FLongTap.X := X;
  FLongTap.Y := Y;
end;

procedure TMapFrame.btnZoomInClick(Sender: TObject);
var
  vOldViewportPositionX: Single;
  vOldViewportPositionY: Single;
begin

  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale + FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale + FZoomStep);
  ZoomIn;
  ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
  // ApplyZoom(vOldViewportPositionX, vOldViewportPositionY);
  ResetLocationMarkers;
end;

procedure TMapFrame.btnZoomOutClick(Sender: TObject);
var
  vOldViewportPositionX: Single;
  vOldViewportPositionY: Single;
begin
  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale - FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale - FZoomStep);
  ZoomOut;
  ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
  // ApplyZoom(vOldViewportPositionX, vOldViewportPositionY);
  ResetLocationMarkers;
end;

procedure TMapFrame.btnDelMarkerClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := True;
  btnDelMarker.Visible := False;
end;

procedure TMapFrame.btnMyLocationClick(Sender: TObject);
var
  MarkerCenter: TPointF;
  ViewportPos: TPointF;
  TargetX: Single;
  TargetY: Single;
begin
  if LocationMarker.Visible then
  begin
    ScrollBox.ViewportPosition := TPointF.Create(0, 0);

    // Получаем позицию центра маркера относительно MapLayout
    MarkerCenter := (TPointF.Create((LocationMarker.LocalToAbsolute(TPointF.Zero).X + LocationMarker.Width / 2) * MapLayout.Scale.X,
      (LocationMarker.LocalToAbsolute(TPointF.Zero).Y + LocationMarker.Height / 2)) * MapLayout.Scale.Y);
    MarkerCenter := ScrollBox.AbsoluteToLocal(MarkerCenter);

    // Вычисляем целевую позицию прокрутки для центрирования маркера
    TargetX := MarkerCenter.X - ScrollBox.Width / 2;
    TargetY := MarkerCenter.Y - ScrollBox.Height / 2;

    // Устанавливаем позицию прокрутки
    ScrollBox.ViewportPosition := TPointF.Create(TargetX, TargetY);
  end;
end;

procedure TMapFrame.OnMarkerClick(Sender: TObject);
begin
  LayDetailMarker.Parent := (Sender as TImage);
  LayDetailMarker.Position.X := (Sender as TImage).Width;
  LayDetailMarker.Position.Y := 0;
  labMarkerText.Text := FMarkerList[GetNumberMarker(Sender as TImage)].LabelText;
  btnDeleteYes.TagObject := (Sender as TImage);
  LayDetailMarker.Visible := True;
  layDetailIssue.Visible := False;
  btnDelMarker.Visible := True;
  gplDeleteYesNo.Visible := False;
  (Sender as TImage).BringToFront;
end;

procedure TMapFrame.ScrollToImageAdvanced(ScrollBox: TScrollBox; Image: TImage; Center: Boolean = True; Animate: Boolean = False);
var
  ImageAbsolutePos: TPointF;
  ImageViewportPos: TPointF;
  NewViewportX, NewViewportY: Single;
begin
  // Получаем абсолютную позицию изображения
  ImageAbsolutePos := Image.LocalToAbsolute(TPointF.Zero);

  // Преобразуем в координаты Viewport
  ImageViewportPos := ScrollBox.AbsoluteToLocal(ImageAbsolutePos);

  if Center then
  begin
    // Центрируем изображение в ScrollBox
    NewViewportX := ImageViewportPos.X - (ScrollBox.Width - Image.Width) / 2;
    NewViewportY := ImageViewportPos.Y - (ScrollBox.Height - Image.Height) / 2;
  end
  else
  begin
    // Просто прокручиваем чтобы изображение было видно
    NewViewportX := ImageViewportPos.X;
    NewViewportY := ImageViewportPos.Y;
  end;

  // Ограничиваем значения в пределах допустимой прокрутки
  NewViewportX := Max(0, Min(NewViewportX, ScrollBox.ContentBounds.Width - ScrollBox.Width));
  NewViewportY := Max(0, Min(NewViewportY, ScrollBox.ContentBounds.Height - ScrollBox.Height));

  if Animate then
  begin
    // Анимированная прокрутка (если нужна анимация)
    // AnimateScrollTo(ScrollBox, NewViewportX, NewViewportY);
  end
  else
  begin
    // Мгновенная прокрутка
    ScrollBox.ViewportPosition := TPointF.Create(NewViewportX, NewViewportY);
  end;
end;

procedure TMapFrame.OnMarkerIssueClick(Sender: TObject);
begin
  layDetailIssue.Parent := (Sender as TImage);
  layDetailIssue.Position.X := (Sender as TImage).Width;
  layDetailIssue.Position.Y := 0;
  labIssueText.Text := FMarkerIssue[GetNumberMarker(Sender as TImage)].LabelText;
  labIssueDetail.Text := FMarkerIssue[GetNumberMarker(Sender as TImage)].LabelDetail;
  layDetailIssue.Visible := True;
  LayDetailMarker.Visible := False;
  (Sender as TImage).BringToFront;
end;

function TMapFrame.GetNumberMarker(AMarker: TImage): integer;
var
  I: integer;
begin
  Result := -1;

  for I := 0 to FMarkerList.Count - 1 do
    if FMarkerList[I].Marker = AMarker then
    begin
      Result := I;
      Exit;
    end;

  for I := 0 to FMarkerIssue.Count - 1 do
    if FMarkerIssue[I].Marker = AMarker then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TMapFrame.CreateMarker(AMarker: TMarkerData);
begin
  AMarker.Marker := TImage.Create(MapLayout);
  AMarker.Marker.Parent := MapLayout;
  AMarker.Marker.Width := 50;
  AMarker.Marker.Height := AMarker.Marker.Width;
  AMarker.Marker.OnClick := OnMarkerClick;

  case AMarker.MarkerType of
    mtPoint:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[0].MultiResBitmap[0].Bitmap);
    mtRad:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[1].MultiResBitmap[0].Bitmap);
    mtAnomaly:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[2].MultiResBitmap[0].Bitmap);
    mtBag:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[3].MultiResBitmap[0].Bitmap);
    mtIssue:
      begin
        AMarker.Marker.Bitmap.Assign(ImageList.Source[4].MultiResBitmap[0].Bitmap);
        AMarker.Marker.OnClick := OnMarkerIssueClick;
      end;
  end;

  AMarker.Marker.Visible := True;
  SetMarker(AMarker.Marker, FCoords.Latitude, FCoords.Longitude);

  if AMarker.MarkerType = mtIssue then
    FMarkerIssue.Add(AMarker)
  else
    FMarkerList.Add(AMarker);
end;

procedure TMapFrame.ResetLocationMarkers;
var
  I: integer;
begin

  for I := 0 to FMarkerList.Count - 1 do
    SetMarker(FMarkerList[I].Marker, FMarkerList[I].Coords.Latitude, FMarkerList[I].Coords.Longitude);

  for I := 0 to FMarkerIssue.Count - 1 do
    SetMarker(FMarkerIssue[I].Marker, FMarkerIssue[I].Coords.Latitude, FMarkerIssue[I].Coords.Longitude);

  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);

  if MarkersPanel.Visible then
    SetMarker(MarkersPanel, FCoords.Latitude, FCoords.Longitude);
end;

procedure TMapFrame.ZoomIn;
begin
  SetZoom(FCurrentScale + FZoomStep);
end;

procedure TMapFrame.ZoomOut;
begin
  SetZoom(FCurrentScale - FZoomStep);
end;

procedure TMapFrame.SetZoom(AScale: Double; ACenterX: Single = -1; ACenterY: Single = -1);
begin
  if not FMapLoaded then
    Exit;

  // Ограничиваем масштаб
  AScale := Max(FMinScale, Min(FMaxScale, AScale));

  if AScale <> FCurrentScale then
  begin
    if (FOriginalMapHeight * AScale >= ScrollBox.Height) then
    begin
      FCurrentScale := AScale;
      // Обновляем размеры карты
      MapImage.Width := FOriginalMapWidth * FCurrentScale;
      MapImage.Height := FOriginalMapHeight * FCurrentScale;
      MapLayout.Width := FOriginalMapWidth * FCurrentScale;
      MapLayout.Height := FOriginalMapHeight * FCurrentScale;

      UpdateZoomControls;

      // Если указана точка центра, корректируем позицию прокрутки
      if (ACenterX >= 0) and (ACenterY >= 0) then
      begin
        ApplyZoom(ACenterX, ACenterY);
      end;
    end;
  end;
end;

procedure TMapFrame.TimerSensorTimer(Sender: TObject);
begin
  ScanAnomalies;
end;

procedure TMapFrame.btnDeleteNoClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := False;
  btnDelMarker.Visible := True;
end;

procedure TMapFrame.btnDeleteYesClick(Sender: TObject);
var
  AMarker: TImage;
begin
  AMarker := (Sender as TSpeedButton).TagObject as TImage;
  AMarker.Visible := False;
  FMarkerList.Delete(GetNumberMarker(AMarker));
  gplDeleteYesNo.Visible := False;
  btnDelMarker.Visible := True;
end;

procedure TMapFrame.ApplyZoom(ACenterX: Single = -1; ACenterY: Single = -1);
var
  ViewportX, ViewportY: Single;
begin
  if (ACenterX >= 0) and (ACenterY >= 0) then
  begin
    // Ограничиваем позицию прокрутки
    ViewportX := Max(0, Min(ACenterX, MapLayout.Width - ScrollBox.Width));
    ViewportY := Max(0, Min(ACenterY, MapLayout.Height - ScrollBox.Height));

    ScrollBox.ViewportPosition := PointF(ViewportX, ViewportY);
  end;
end;

procedure TMapFrame.ZoomToPoint(APoint: TPointF; AScale: Double);
begin
  SetZoom(AScale, APoint.X, APoint.Y);
end;

procedure TMapFrame.UpdateZoomControls;
begin
  lblZoom.Text := Format('%.0f%%', [FCurrentScale * 100]);

  // Обновляем состояние кнопок
  btnZoomIn.Enabled := FCurrentScale < FMaxScale;
  btnZoomOut.Enabled := FCurrentScale > FMinScale;
  btnResetZoom.Enabled := Abs(FCurrentScale - 1.0) > 0.01;
end;

procedure TMapFrame.UpdateMapBounds;
begin
  // Устанавливаем границы по умолчанию
  FTopLeftLat := 52.093602;
  FTopLeftLon := 23.697432;
  FBottomRightLat := 52.085325;
  FBottomRightLon := 23.721153;
end;

procedure TMapFrame.SetMapBounds(TopLeftLat, TopLeftLon, BottomRightLat, BottomRightLon: Double);
begin
  FTopLeftLat := TopLeftLat;
  FTopLeftLon := TopLeftLon;
  FBottomRightLat := BottomRightLat;
  FBottomRightLon := BottomRightLon;
end;

end.
