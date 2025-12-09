unit uMapFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, System.Math.Vectors,
  FMX.Edit, FMX.Ani, FMX.Gestures, FMX.VirtualKeyboard, FMX.Platform,
  System.Math, System.Sensors, System.Sensors.Components, System.Permissions,
  FMX.Effects, Generics.Collections, System.ImageList, FMX.ImgList,
  System.Actions, FMX.ActnList;

type
  TMarkerType = (mtPoint, mtRad, mtAnomaly, mtBag, mtIssue);
  TAnomalyType = (atElectro, atFire, atGravity, atRadiation, atChemical);

  TMarkerData = record
    Marker: TImage;
    Coords: TLocationCoord2D;
    LabelText: string;
    LabelDetail: string;
    MarkerType: TMarkerType;
  end;

  TAnomalyData = record
    Coords: TLocationCoord2D;
    Radius: integer;
    Power: integer;
    AnomalyType: TAnomalyType;
  end;

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
    Marker: TImage;
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
    Label1: TLabel;
    LocationSensorSecond: TLocationSensor;
    LocationSensorThird: TLocationSensor;
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnResetZoomClick(Sender: TObject);
    procedure FrameResized(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure btnMyLocationClick(Sender: TObject);
    procedure MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ActAddMarkerExecute(Sender: TObject);
    procedure btnDelMarkerClick(Sender: TObject);
    procedure btnDeleteNoClick(Sender: TObject);
    procedure btnDeleteYesClick(Sender: TObject);
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
    FLocation: TLocationCoord2D;
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
    procedure HideVirtualKeyboard;
    procedure AdjustControlsForMobile;
    function GetAndroidScaleFactor: Single;
    function CalculateBearing(const StartPoint, EndPoint: TLocationCoord2D): Double;
    procedure SetMarker(AMarker: TImage; Lat, Lon: Double);
    procedure CreateMarker(AMarker: TMarkerData);
    procedure ResetLocationMarkers;
    procedure OnMarkerClick(Sender: TObject);
    procedure CreateIssueTest;
    procedure OnMarkerIssueClick(Sender: TObject);
    function GetNumberMarker(AMarker: TImage): integer;
    procedure CreateAnomalyTest;
    function CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: Double): Double;
    function GetPositionRelativeTo(SourceControl, TargetParent: TControl): TPointF;
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

uses
{$IFDEF ANDROID}
  FMX.Platform.Android,
{$ENDIF}
  uGlobal;

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
  LocationMarker.BringToFront;

  // Настройка жестов
  ScrollBox.Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan, TInteractiveGesture.DoubleTap];
  ScrollBox.Touch.GestureManager := GestureManager;

  UpdateZoomControls;
  AdjustControlsForMobile;

  FMarkerList := TList<TMarkerData>.Create;
  FMarkerIssue := TList<TMarkerData>.Create;
  FAnomalyList := TList<TAnomalyData>.Create;

  LoadMapFromFile(''); // Переписать

  // CreateIssueTest;   // Тестовая задача
  CreateAnomalyTest
end;

procedure TMapFrame.CreateIssueTest;
var
  AMarker: TMarkerData;
begin
  FCoords.Latitude := 52.153046;
  FCoords.Longitude := 23.586531;

  AMarker.Coords := FCoords;

  AMarker.MarkerType := mtIssue;

  AMarker.LabelText := 'Первая уловка';
  AMarker.LabelDetail := 'Найти зацепки пропавшего сталкера';

  CreateMarker(AMarker);
end;

procedure TMapFrame.CreateAnomalyTest;
var
  AAnomaly: TAnomalyData;
begin
  FCoords.Latitude := 52.153046;
  FCoords.Longitude := 23.586531;

  AAnomaly.Coords := FCoords;
  AAnomaly.AnomalyType := atRadiation;
  AAnomaly.Radius := 5;
  AAnomaly.Power := 10;

  FAnomalyList.Add(AAnomaly);

  FCoords.Latitude := 52.153022;
  FCoords.Longitude := 23.586815;

  AAnomaly.Coords := FCoords;
  AAnomaly.AnomalyType := atRadiation;
  AAnomaly.Radius := 7;
  AAnomaly.Power := 20;

  FAnomalyList.Add(AAnomaly);
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

    // Подписка на события ресайза
    OnResized := FrameResized;
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
  Marker.Visible := False;
end;

procedure TMapFrame.AdjustControlsForMobile;
var
  ScaleFactor: Single;
begin
  if TOSVersion.Platform = pfAndroid then
  begin
    ScaleFactor := GetAndroidScaleFactor;

    // Увеличиваем размер маркера
    // LocationMarker.Width := 30 * ScaleFactor;
    // LocationMarker.Height := 30 * ScaleFactor;
  end;
end;

function TMapFrame.GetAndroidScaleFactor: Single;
begin
  if TOSVersion.Platform = pfAndroid then
    Result := 1.5 // Базовая масштабируемость для Android
  else
    Result := 1.0;
end;

procedure TMapFrame.FrameResized(Sender: TObject);
begin
  // Адаптация интерфейса при изменении ориентации
  AdjustControlsForMobile;
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
  FLocation := NewLocation;
  LocationMarker.Visible := True;

  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
  OrientationMarker.RotationAngle := 135 + CalculateBearing(OldLocation, FLocation);
end;

function TMapFrame.CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: Double): Double;
const
  R = 6371000; // Радиус Земли в метрах
  DegToRad = Pi / 180;
var
  X, Y: Double;
begin
  // Приближенный расчет для небольших расстояний
  X := (Lon2 - Lon1) * Cos(DegToRad * (Lat1 + Lat2) / 2);
  Y := (Lat2 - Lat1);

  Result := R * Sqrt(X * X + Y * Y) * DegToRad;
end;

procedure TMapFrame.ScanAnomalies;
var
  I: integer;
  vDistance: Double;
begin
  Label1.Text := '';

  for I := 0 to FAnomalyList.Count - 1 do
  begin
    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[I].Coords.Latitude, FAnomalyList[I].Coords.Longitude);

    if vDistance <= FAnomalyList[I].Radius then
      Person.Health := Person.Health - FAnomalyList[I].Power * (vDistance / FAnomalyList[I].Radius);
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
  HideVirtualKeyboard;
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
  SetMarker(Marker, FCoords.Latitude, FCoords.Longitude);
  Marker.BringToFront;
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

  if AMarker = Marker then
    AMarker.Position.Y := Point.Y - AMarker.Height / 2
  else
    AMarker.Position.Y := Point.Y - AMarker.Height;

  AMarker.Visible := True;
end;

procedure TMapFrame.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Marker.Visible := False;
  LayDetailMarker.Visible := False;
  layDetailIssue.Visible := False;
  FLongTap.X := X;
  FLongTap.Y := Y;
end;

procedure TMapFrame.btnZoomInClick(Sender: TObject);
begin
  HideVirtualKeyboard;
  ZoomIn;
  ApplyZoom(ScrollBox.ViewportPosition.X + ScrollBox.Width / 2, ScrollBox.ViewportPosition.Y + ScrollBox.Height / 2);
  ResetLocationMarkers;
end;

procedure TMapFrame.btnZoomOutClick(Sender: TObject);
begin
  HideVirtualKeyboard;
  ZoomOut;
  ApplyZoom(ScrollBox.ViewportPosition.X + ScrollBox.Width / 2, ScrollBox.ViewportPosition.Y + ScrollBox.Height / 2);
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
if not LocationMarker.Visible then
    Exit;

  // Получаем позицию центра маркера относительно MapLayout
  MarkerCenter := TPointF.Create(
    LocationMarker.Position.X + LocationMarker.Width / 2,
    LocationMarker.Position.Y + LocationMarker.Height / 2
  );

  // Вычисляем целевую позицию прокрутки для центрирования маркера
  TargetX := MarkerCenter.X - ScrollBox.Width / 2;
  TargetY := MarkerCenter.Y - ScrollBox.Height / 2;

  // Ограничиваем позицию в пределах карты
  TargetX := Max(0, Min(TargetX, MapLayout.Width - ScrollBox.Width));
  TargetY := Max(0, Min(TargetY, MapLayout.Height - ScrollBox.Height));

  // Устанавливаем позицию прокрутки
  ScrollBox.ViewportPosition := TPointF.Create(TargetX, TargetY);

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

function TMapFrame.GetPositionRelativeTo(SourceControl, TargetParent: TControl): TPointF;
begin

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
  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);

  if Marker.Visible then
    SetMarker(Marker, FCoords.Latitude, FCoords.Longitude);

  for I := 0 to FMarkerList.Count - 1 do
    SetMarker(FMarkerList[I].Marker, FMarkerList[I].Coords.Latitude, FMarkerList[I].Coords.Longitude);

  for I := 0 to FMarkerIssue.Count - 1 do
    SetMarker(FMarkerIssue[I].Marker, FMarkerIssue[I].Coords.Latitude, FMarkerIssue[I].Coords.Longitude);
end;

procedure TMapFrame.btnResetZoomClick(Sender: TObject);
begin
  HideVirtualKeyboard;
  SetZoom(1.0);
end;

procedure TMapFrame.HideVirtualKeyboard;
var
  FService: IFMXVirtualKeyboardService;
begin
  if TOSVersion.Platform = pfAndroid then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, FService) then
    begin
      FService.HideVirtualKeyboard;
    end;
  end;
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
    // Корректируем позицию прокрутки для сохранения точки в центре
    ViewportX := ACenterX - ScrollBox.Width / 2;
    ViewportY := ACenterY - ScrollBox.Height / 2;

    // Ограничиваем позицию прокрутки
    ViewportX := Max(0, Min(ViewportX, MapLayout.Width - ScrollBox.Width));
    ViewportY := Max(0, Min(ViewportY, MapLayout.Height - ScrollBox.Height));

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
  FTopLeftLat := 52.154782;
  FTopLeftLon := 23.581871;
  FBottomRightLat := 52.151184;
  FBottomRightLon := 23.597090;
end;

procedure TMapFrame.SetMapBounds(TopLeftLat, TopLeftLon, BottomRightLat, BottomRightLon: Double);
begin
  FTopLeftLat := TopLeftLat;
  FTopLeftLon := TopLeftLon;
  FBottomRightLat := BottomRightLat;
  FBottomRightLon := BottomRightLon;
end;

end.
