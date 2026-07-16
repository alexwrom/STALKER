unit uFrameMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, System.Math.Vectors,
  FMX.Edit, FMX.Ani, FMX.Gestures, FMX.VirtualKeyboard, FMX.Platform,
  System.Math, System.Sensors, System.Sensors.Components, System.Permissions,
  FMX.Effects, Generics.Collections, System.ImageList, FMX.ImgList,
  System.Actions, FMX.ActnList, uGlobal, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Media, System.IOUtils, FireDAC.Comp.Client, System.Threading,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.Location,
  Androidapi.JNI.Net,
{$ENDIF}
  uLocationListener;

type
  TFrameMap = class(TFrame)
    ScrollBox: TScrollBox;
    MapLayout: TLayout;
    MapImage: TImage;
    ManMarker: TCircle;
    ZoomLayout: TLayout;
    btnZoomIn: TButton;
    btnZoomOut: TButton;
    GestureManager: TGestureManager;
    LocationMarker: TLayout;
    LayMapControls: TLayout;
    OrientationMarker: TImage;
    btnMyLocation: TButton;
    lblZoom: TLabel;
    ImageList: TImageList;
    ActionList: TActionList;
    ActAddMarker: TAction;
    LayDetailMarker: TLayout;
    Rectangle3: TRectangle;
    labMarkerText: TLabel;
    btnDelMarker: TSpeedButton;
    Image2: TImage;
    gplDeleteYesNo: TGridPanelLayout;
    btnDeleteNo: TSpeedButton;
    btnDeleteYes: TSpeedButton;
    recPanelDeleteMarker: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    InnerGlowEffect2: TInnerGlowEffect;
    InnerGlowEffect3: TInnerGlowEffect;
    InnerGlowEffect4: TInnerGlowEffect;
    LayClient: TLayout;
    layCritical: TLayout;
    Image1: TImage;
    Rectangle4: TRectangle;
    labCritical: TLabel;
    imgArrowMan: TImage;
    Layout7: TLayout;
    ShadowEffect1: TShadowEffect;
    btnEdit: TSpeedButton;
    Image3: TImage;
    gplDelEdit: TGridPanelLayout;
    InnerGlowEffect5: TInnerGlowEffect;
    Rectangle1: TRectangle;
    Image26: TImage;
    btnAddArt: TCornerButton;
    Image4: TImage;
    btnAddPlace: TCornerButton;
    Image5: TImage;
    btnAddAnomaly: TCornerButton;
    imgAddPanel: TImage;
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnMyLocationClick(Sender: TObject);
    procedure btnDelMarkerClick(Sender: TObject);
    procedure btnDeleteNoClick(Sender: TObject);
    procedure btnDeleteYesClick(Sender: TObject);
    procedure ScrollBoxViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure btnEditClick(Sender: TObject);
    procedure MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure btnAddArtClick(Sender: TObject);
    procedure btnAddPlaceClick(Sender: TObject);
    procedure btnAddAnomalyClick(Sender: TObject);
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
    FLongTap: TPointF;
    FMapRealWidth: integer;
    FLoad: Boolean;
    procedure LoadMap;
    procedure SetLocationMarker(Lat, Lon: Double);
    function CoordinatesToPixels(Lat, Lon: Double): TPointF;
    function PixelsToCoordinates(X, Y: Single): TLocationCoord2D;
    procedure UpdateZoomControls;
    procedure ApplyZoom(ACenterX: Single = -1; ACenterY: Single = -1);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure SetZoom(AScale: Double; ACenterX: Single = -1; ACenterY: Single = -1);
    procedure SetupAndroidSpecifics;
    function CalculateBearing(const StartPoint, EndPoint: TLocationCoord2D): Double;
    procedure SetMarker(AMarker: TImage; Lat, Lon: Double);
    procedure CreateMarker(AMarker: TMarkerData);
    procedure OnMarkerClick(Sender: TObject);
    function GetNumberMarker(AMarker: TImage): integer;

    procedure SetArrows(AArrow: TImage; ATarget: TControl);
    procedure SetLocation;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ResetLocationMarkers;
    procedure LoadArtefactsToMap;
    procedure UpdateArts;
    procedure LoadAnomalies;
    procedure UpdateAnomalies;
    procedure LoadPlaces;
    procedure UpdateBaseSafeDead;
{$IFDEF ANDROID}
    procedure LocationServiceChanged;
    procedure LocationisChanged(Location: JLocation); overload;
    procedure LocationisChanged(Locations: JList);  overload;
{$ENDIF}
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

constructor TFrameMap.Create(AOwner: TComponent);
begin
  inherited;
  FLoad := true;

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
  FAnomalyList := TList<TAnomalyData>.Create;

  LoadMap;

  LoadArtefactsToMap;
  LoadPlaces;
  LoadAnomalies;

  UpdateBaseSafeDead;
  UpdateArts;
  UpdateAnomalies;
end;

procedure TFrameMap.LoadAnomalies;
var
  vQuery: TFDQuery;
  vAnomalyItem: TAnomalyData;
  AFormatSettings: TFormatSettings;
begin
  AFormatSettings.DateSeparator := '-';
  AFormatSettings.TimeSeparator := ':';
  AFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  AFormatSettings.LongTimeFormat := 'hh:nn:ss';

  if Assigned(FAnomalyList) then
    FAnomalyList.Clear
  else
    FAnomalyList := TList<TAnomalyData>.Create;

  ExeExec('select * from anomalies;', exActive, vQuery);
  try
    vQuery.First;

    while Not vQuery.Eof do
    begin
      vAnomalyItem.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
      vAnomalyItem.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
      vAnomalyItem.Radius := vQuery.FieldByName('radius').AsInteger;
      vAnomalyItem.Power := vQuery.FieldByName('power').AsInteger;
      vAnomalyItem.ID := vQuery.FieldByName('anomaly_id').AsInteger;
      vAnomalyItem.IsHide := vQuery.FieldByName('hide_in_datetime').AsBoolean;

      if vQuery.FieldByName('datetime_hide').AsString <> ''  then
        vAnomalyItem.DateTimeHide := StrToDateTime(vQuery.FieldByName('datetime_hide').AsString, AFormatSettings);

      case vQuery.FieldByName('anomaly_type_id').AsInteger of
        1:
          vAnomalyItem.AnomalyType := atElectro;
        2:
          vAnomalyItem.AnomalyType := atFire;
        3:
          vAnomalyItem.AnomalyType := atPhisic;
        4:
          vAnomalyItem.AnomalyType := atRadiation;
        5:
          vAnomalyItem.AnomalyType := atChimishe;
        6:
          vAnomalyItem.AnomalyType := atPSI;
      end;

      FAnomalyList.Add(vAnomalyItem);
      vQuery.Next;
    end;
  finally
    FreeQueryAndConn(vQuery);
  end;

end;

procedure TFrameMap.UpdateBaseSafeDead;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := FMarkerList.Count - 1 downto 0 do
    if FMarkerList[I].MarkerType in [mtSafe, mtBase] then
    begin
      FMarkerList[I].Marker.Parent := nil;
      FMarkerList[I].Marker.Visible := False;
      FMarkerList.Delete(I);
    end;

  for I := 0 to FPlacesList.Count - 1 do
  begin
    FCoords := FPlacesList[I].Coords;
    AMarker.Coords := FCoords;
    AMarker.MarkerType := FPlacesList[I].MarkerType;
    AMarker.ID := FPlacesList[I].ID;
    AMarker.Tag := I;

    AMarker.LabelText := FPlacesList[I].Name;

    CreateMarker(AMarker);
  end;
end;

procedure TFrameMap.UpdateAnomalies;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := FMarkerList.Count - 1 downto 0 do
    if FMarkerList[I].MarkerType in [mtAnomaly, mtRadiation] then
    begin
      FMarkerList[I].Marker.Parent := nil;
      FMarkerList[I].Marker.Visible := False;
      FMarkerList.Delete(I);
    end;

  for I := 0 to FAnomalyList.Count - 1 do
  begin
    FCoords := FAnomalyList[I].Coords;
    AMarker.Coords := FCoords;

    if FAnomalyList[I].AnomalyType = atRadiation then
      AMarker.MarkerType := mtRadiation
    else
      AMarker.MarkerType := mtAnomaly;

    AMarker.Tag := I;
    AMarker.ID := FAnomalyList[I].ID;

    CreateMarker(AMarker);
  end;
end;

procedure TFrameMap.UpdateArts;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := FMarkerList.Count - 1 downto 0 do
    if FMarkerList[I].MarkerType = mtArt then
    begin
      FMarkerList[I].Marker.Parent := nil;
      FMarkerList[I].Marker.Visible := False;
      FMarkerList.Delete(I);
    end;

  for I := 0 to FArtefactsList.Count - 1 do
  begin
    FCoords := FArtefactsList[I].Coords;
    AMarker.Coords := FCoords;
    AMarker.MarkerType := mtArt;
    AMarker.Tag := I;
    AMarker.ID := FArtefactsList[I].ID;
    AMarker.LabelText := FArtefactsList[I].Name;

    CreateMarker(AMarker);
  end;
end;

procedure TFrameMap.LoadArtefactsToMap;
var
  vQuery: TFDQuery;
  vArtefactData: TArtefactData;
begin
  if Assigned(FArtefactsList) then
    FArtefactsList.Clear
  else
    FArtefactsList := TList<TArtefactData>.Create;

  ExeExec('select * from arts_to_map atm join arts a on a.art_id = atm.art_id;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vArtefactData.ID := vQuery.FieldByName('art_to_map_id').AsInteger;
    vArtefactData.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vArtefactData.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vArtefactData.Level := vQuery.FieldByName('level').AsInteger;
    vArtefactData.Name := vQuery.FieldByName('art_name').AsString;
    vArtefactData.Icon := TBitmap.Create;
    vArtefactData.Icon.Assign(vQuery.FieldByName('icon'));
    FArtefactsList.Add(vArtefactData);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFrameMap.ScrollBoxViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  SetArrows(imgArrowMan, LocationMarker);
end;

procedure TFrameMap.SetArrows(AArrow: TImage; ATarget: TControl);
var
  vLoc: TPointF;
begin
  if ATarget.Visible then
  begin
    vLoc := ATarget.LocalToAbsolute(TPointF.Zero);

    AArrow.Visible := (vLoc.Y > ScrollBox.Height) or (vLoc.X < 0) or (vLoc.X > ScrollBox.Width) or (vLoc.Y < 0);

    if vLoc.Y > ScrollBox.Height then // Bottom
    begin
      AArrow.Position.X := IfThen(vLoc.X + AArrow.Width > ScrollBox.Width, ScrollBox.Width - AArrow.Width, vLoc.X);
      AArrow.Position.Y := ScrollBox.Height - AArrow.Height;
      AArrow.RotationAngle := 0;
    end;

    if vLoc.Y < 0 then // Top
    begin
      AArrow.Position.X := IfThen(vLoc.X + AArrow.Width > ScrollBox.Width, ScrollBox.Width - AArrow.Width, Max(0, vLoc.X));
      AArrow.Position.Y := 0;
      AArrow.RotationAngle := -180;
    end;

    if vLoc.X < 0 then // Left
    begin
      AArrow.Position.Y := IfThen(vLoc.Y + AArrow.Height > ScrollBox.Height, ScrollBox.Height - AArrow.Height, Max(0, vLoc.Y));
      AArrow.Position.X := 0;
      AArrow.RotationAngle := 90;
    end;

    if vLoc.X > ScrollBox.Width then // Right
    begin
      AArrow.Position.Y := IfThen(vLoc.Y + AArrow.Height > ScrollBox.Height, ScrollBox.Height - AArrow.Height, Max(0, vLoc.Y));
      AArrow.Position.X := ScrollBox.Width - AArrow.Width;
      AArrow.RotationAngle := 270;
    end;

    if (vLoc.Y > ScrollBox.Height) and (vLoc.X > ScrollBox.Width) then // Bottom-Right
    begin
      AArrow.Position.X := ScrollBox.Width - AArrow.Width;
      AArrow.Position.Y := ScrollBox.Height - AArrow.Height;
      AArrow.RotationAngle := -45;
    end;

    if (vLoc.Y > ScrollBox.Height) and (vLoc.X < 0) then // Bottom-Left
    begin
      AArrow.Position.X := 0;
      AArrow.Position.Y := ScrollBox.Height - AArrow.Height;
      AArrow.RotationAngle := 45;
    end;

    if (vLoc.Y < 0) and (vLoc.X > ScrollBox.Width) then // Top-Right
    begin
      AArrow.Position.X := ScrollBox.Width - AArrow.Width;
      AArrow.Position.Y := 0;
      AArrow.RotationAngle := 225;
    end;

    if (vLoc.Y < 0) and (vLoc.X < 0) then // Top-Left
    begin
      AArrow.Position.X := 0;
      AArrow.Position.Y := 0;
      AArrow.RotationAngle := -225;
    end;
  end;
end;

destructor TFrameMap.Destroy;
begin
  inherited;
end;

procedure TFrameMap.SetupAndroidSpecifics;
begin
  // Увеличиваем размеры элементов для тач-интерфейса
  if TOSVersion.Platform = pfAndroid then
  begin
    // Настройка для лучшей производительности
    ScrollBox.EnableDragHighlight := False;
    MapImage.HitTest := true;
  end;
end;

procedure TFrameMap.LoadMap;
var
  vQuery: TFDQuery;
begin
  try
    MapImage.Bitmap.LoadFromFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'map_image.png'));
    FMapLoaded := true;

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
    ExeExec('select * from game_data;', exActive, vQuery);
    try
      FTopLeftLat := vQuery.FieldByName('map_left_top_lat').AsFloat;
      FTopLeftLon := vQuery.FieldByName('map_left_top_lon').AsFloat;
      FBottomRightLat := vQuery.FieldByName('map_right_bottom_lat').AsFloat;
      FBottomRightLon := vQuery.FieldByName('map_right_bottom_lon').AsFloat;
      FMapRealWidth := Round(CalculateFastDistance(FTopLeftLat, FTopLeftLon, FTopLeftLat, FBottomRightLon));
    finally
      FreeQueryAndConn(vQuery);
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Ошибка загрузки карты: ' + E.Message);
      FMapLoaded := False;
    end;
  end;
end;

{$IFDEF ANDROID}

procedure TFrameMap.LocationServiceChanged;
var
  LocationManagerService: JObject;
  Location: JLocation;
begin
  if not Assigned(FLocationManager) then
  begin
    LocationManagerService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
    FLocationManager := TJLocationManager.Wrap((LocationManagerService as ILocalObject).GetObjectID);

    if not Assigned(locationListener) then
      locationListener := TLocationListener.Create();
  end;

  try
    FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.GPS_PROVIDER, 0, 0, locationListener, TJLooper.JavaClass.getMainLooper);

    Location := FLocationManager.getLastKnownLocation(TJLocationManager.JavaClass.GPS_PROVIDER);
    LocationisChanged(Location);
  finally
  end;
end;

procedure TFrameMap.LocationisChanged(Location: JLocation);
begin
  if Assigned(Location) then
  begin
    FLocation := TLocationCoord2D.Create(Location.getLatitude, Location.getLongitude);
    SetLocation;
  end;
end;

procedure TFrameMap.LocationisChanged(Locations: JList);
var
  I: Integer;
  Location: JLocation;
begin
  // Проверяем, что список не пустой
  if (Locations = nil) or (Locations.size = 0) then
    Exit;

  // Получаем последнее (самое свежее) местоположение из списка
  Location := TJLocation.Wrap(JObject(Locations.get(Locations.size - 1)));

  if Assigned(Location) then
  begin
    // Обновляем основную локацию
    FLocation.Latitude := Location.getLatitude;
    FLocation.Longitude := Location.getLongitude;

    // Вызываем метод обновления позиции на карте
    SetLocation;
  end;
end;
{$ENDIF}

procedure TFrameMap.SetLocation;
begin
  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
  OrientationMarker.RotationAngle := 135 + CalculateBearing(FOldLocation, FLocation);
  FOldLocation := FLocation;
end;

function TFrameMap.CalculateBearing(const StartPoint, EndPoint: TLocationCoord2D): Double;
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

procedure TFrameMap.SetLocationMarker(Lat, Lon: Double);
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

  LocationMarker.Visible := true;
  LocationMarker.BringToFront;
  MapLayout.PrepareForPaint;
  MapLayout.Repaint;

  SetArrows(imgArrowMan, LocationMarker);
end;

function TFrameMap.CoordinatesToPixels(Lat, Lon: Double): TPointF;
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

function TFrameMap.PixelsToCoordinates(X, Y: Single): TLocationCoord2D;
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

procedure TFrameMap.SetMarker(AMarker: TImage; Lat, Lon: Double);
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
  AMarker.Visible := true;
end;

procedure TFrameMap.MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  FCoords := PixelsToCoordinates(FLongTap.X, FLongTap.Y);
  // Устанавливаем маркер
  SetMarker(imgAddPanel, FCoords.Latitude, FCoords.Longitude);
  imgAddPanel.BringToFront;
end;

procedure TFrameMap.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  LayDetailMarker.Visible := False;
  imgAddPanel.Visible := False;
  FLongTap.X := X;
  FLongTap.Y := Y;
end;

procedure TFrameMap.btnZoomInClick(Sender: TObject);
var
  vOldViewportPositionX: Single;
  vOldViewportPositionY: Single;
begin

  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale + FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale + FZoomStep);
  ZoomIn;
  ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
end;

procedure TFrameMap.btnZoomOutClick(Sender: TObject);
var
  vOldViewportPositionX: Single;
  vOldViewportPositionY: Single;
begin
  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale - FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale - FZoomStep);
  ZoomOut;
  ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
end;

procedure TFrameMap.btnDelMarkerClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := true;
  gplDelEdit.Visible := False;
end;

procedure TFrameMap.btnEditClick(Sender: TObject);
var
  AMarker: TMarkerData;
begin
  AMarker := FMarkerList[GetNumberMarker((Sender as TSpeedButton).TagObject as TImage)];
  EditMarker(AMarker);
end;

procedure TFrameMap.btnMyLocationClick(Sender: TObject);
var
  MarkerCenter: TPointF;
  TargetX: Single;
  TargetY: Single;
begin
  if LocationMarker.Visible then
  begin
    ScrollBox.AniCalculations.Animation := true;
    ScrollBox.AniCalculations.BoundsAnimation := true;
    ScrollBox.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

    // Получаем позицию центра маркера относительно MapLayout
    MarkerCenter := (TPointF.Create((LocationMarker.LocalToAbsolute(TPointF.Zero).X + LocationMarker.Width / 2) * MapLayout.Scale.X, (LocationMarker.LocalToAbsolute(TPointF.Zero).Y + LocationMarker.Height / 2)) * MapLayout.Scale.Y);
    MarkerCenter := ScrollBox.AbsoluteToLocal(MarkerCenter);

    // Вычисляем целевую позицию прокрутки для центрирования маркера
    TargetX := MarkerCenter.X - ScrollBox.Width / 2;
    TargetY := MarkerCenter.Y - ScrollBox.Height / 2;

    // Устанавливаем позицию прокрутки
    ScrollBox.AniCalculations.MouseWheel(TargetX, TargetY);
  end;
end;

procedure TFrameMap.OnMarkerClick(Sender: TObject);
begin
  LayDetailMarker.Parent := (Sender as TImage);
  LayDetailMarker.Position.X := (Sender as TImage).Width / 2 - LayDetailMarker.Width / 2;
  LayDetailMarker.Position.Y := (Sender as TImage).Height / 2 - LayDetailMarker.Height / 2;

  labMarkerText.Text := FMarkerList[GetNumberMarker(Sender as TImage)].LabelText;

  btnDeleteYes.TagObject := (Sender as TImage);
  btnEdit.TagObject := (Sender as TImage);
  LayDetailMarker.Visible := true;
  recPanelDeleteMarker.Visible := true;
  gplDelEdit.Visible := recPanelDeleteMarker.Visible;
  gplDeleteYesNo.Visible := False;
  (Sender as TImage).BringToFront;
end;

function TFrameMap.GetNumberMarker(AMarker: TImage): integer;
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

end;

procedure TFrameMap.CreateMarker(AMarker: TMarkerData);
  procedure CreateBackground(AWidth: Single);
  begin
    with TCircle.Create(AMarker.Marker) do
    begin
      Parent := AMarker.Marker;
      Align := TAlignLayout.Center;
      fill.Kind := TBrushKind.Solid;
      fill.Color := TAlphaColors.Alpha;
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := TAlphaColors.White;
      Width := AWidth;
      Height := AWidth;
      HitTest := False;
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
      BringToFront;
    end;
  end;

var
  vWidth: Single;

begin
  AMarker.Marker := TImage.Create(MapLayout);
  AMarker.Marker.Parent := MapLayout;
  AMarker.Marker.Width := 50;
  AMarker.Marker.Height := AMarker.Marker.Width;
  AMarker.Marker.OnClick := OnMarkerClick;

  case AMarker.MarkerType of
    mtArt:
      begin
        CreateBackground(30);
        CreateIcon(FArtefactsList[AMarker.Tag].Icon);
      end;
    mtBase:
      begin
        vWidth := FOriginalMapWidth / FMapRealWidth * FPlacesList[AMarker.Tag].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(vWidth / FCurrentScale);

        CreateBackground(vWidth);
        CreateIcon(ImageList.Source[1].MultiResBitmap[0].Bitmap, 50);
      end;
    mtSafe:
      begin
        vWidth := FOriginalMapWidth / FMapRealWidth * FPlacesList[AMarker.Tag].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(vWidth / FCurrentScale);

        CreateBackground(vWidth);
        CreateIcon(ImageList.Source[0].MultiResBitmap[0].Bitmap);
      end;
    mtRadiation:
      begin
        vWidth := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Tag].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(vWidth / FCurrentScale);
        AMarker.LabelText := 'Радиация';
        CreateBackground(vWidth);
        CreateIcon(ImageList.Source[3].MultiResBitmap[0].Bitmap);
      end;
    mtAnomaly:
      begin
        vWidth := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Tag].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(vWidth / FCurrentScale);
        CreateBackground(vWidth);

        case FAnomalyList[AMarker.Tag].AnomalyType of
          atElectro:
            begin
              CreateIcon(ImageList.Source[4].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Электрическая аномалия';
            end;
          atFire:
            begin
              CreateIcon(ImageList.Source[6].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Термическая аномалия';
            end;
          atPhisic:
            begin
              CreateIcon(ImageList.Source[7].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Гравитационная аномалия';
            end;
          atChimishe:
            begin
              CreateIcon(ImageList.Source[5].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Химическая аномалия';
            end;
          atPSI:
            begin
              CreateIcon(ImageList.Source[2].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'ПСИ-излучение';
            end;
        end;

      end;
  end;

  SetMarker(AMarker.Marker, FCoords.Latitude, FCoords.Longitude);

  FMarkerList.Add(AMarker);
end;

procedure TFrameMap.LoadPlaces;
var
  vQuery: TFDQuery;
  vPlaceData: TPlaceData;
begin
  if Assigned(FPlacesList) then
    FPlacesList.Clear
  else
    FPlacesList := TList<TPlaceData>.Create;

  ExeExec('select * from places;', exActive, vQuery);
  try
    vQuery.First;

    while Not vQuery.Eof do
    begin
      vPlaceData.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
      vPlaceData.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
      vPlaceData.Name := vQuery.FieldByName('name').AsString;
      vPlaceData.Radius := vQuery.FieldByName('radius').AsInteger;
      vPlaceData.ID := vQuery.FieldByName('place_id').AsInteger;
      vPlaceData.Fractions := vQuery.FieldByName('fractions').AsString;

      if vQuery.FieldByName('type').AsString = 'mtBase' then
        vPlaceData.MarkerType := mtBase
      else if vQuery.FieldByName('type').AsString = 'mtSafe' then
        vPlaceData.MarkerType := mtSafe;

      FPlacesList.Add(vPlaceData);
      vQuery.Next;
    end;
  finally
    FreeQueryAndConn(vQuery);
  end;
end;

procedure TFrameMap.ResetLocationMarkers;
var
  I: integer;

  function GetBackground: TCircle;
  var
    K: integer;
  begin
    for K := 0 to FMarkerList[I].Marker.ChildrenCount - 1 do
      if FMarkerList[I].Marker.Children[K] is TCircle then
        Result := (FMarkerList[I].Marker.Children[K] as TCircle)
  end;

begin
  for I := 0 to FMarkerList.Count - 1 do
  begin
    if (FMarkerList[I].MarkerType in [mtAnomaly, mtRadiation, mtBase, mtSafe]) then
    begin
      GetBackground.Width := FMarkerList[I].Marker.Tag * FCurrentScale;
      GetBackground.Height := GetBackground.Width;
    end;

    SetMarker(FMarkerList[I].Marker, FMarkerList[I].Coords.Latitude, FMarkerList[I].Coords.Longitude);
  end;

  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
end;

procedure TFrameMap.ZoomIn;
begin
  SetZoom(FCurrentScale + FZoomStep);
end;

procedure TFrameMap.ZoomOut;
begin
  SetZoom(FCurrentScale - FZoomStep);
end;

procedure TFrameMap.SetZoom(AScale: Double; ACenterX: Single = -1; ACenterY: Single = -1);
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
      ResetLocationMarkers;

      // Если указана точка центра, корректируем позицию прокрутки
      if (ACenterX >= 0) and (ACenterY >= 0) then
      begin
        ApplyZoom(ACenterX, ACenterY);
      end;
    end;
  end;
end;

procedure TFrameMap.btnAddAnomalyClick(Sender: TObject);
begin
  AddAnomaly;
  imgAddPanel.Visible := False;
end;

procedure TFrameMap.btnAddArtClick(Sender: TObject);
begin
  AddArt;
  imgAddPanel.Visible := False;
end;

procedure TFrameMap.btnAddPlaceClick(Sender: TObject);
begin
  AddPlace;
  imgAddPanel.Visible := False;
end;

procedure TFrameMap.btnDeleteNoClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := False;
  gplDelEdit.Visible := true;
end;

procedure TFrameMap.btnDeleteYesClick(Sender: TObject);
var
  AMarker: TImage;
  vQuery: TFDQuery;
begin
  AMarker := (Sender as TSpeedButton).TagObject as TImage;
  AMarker.Visible := False;
  gplDeleteYesNo.Visible := False;
  gplDelEdit.Visible := true;

  case FMarkerList[GetNumberMarker(AMarker)].MarkerType of
    mtArt:
      ExeExec(Format('delete from arts_to_map where art_to_map_id = %d;', [FMarkerList[GetNumberMarker(AMarker)].ID]), exExecute, vQuery);
    mtAnomaly, mtRadiation:
      ExeExec(Format('delete from anomalies where anomaly_id = %d;', [FMarkerList[GetNumberMarker(AMarker)].ID]), exExecute, vQuery);
    mtBase, mtSafe:
      ExeExec(Format('delete from places where place_id = %d;', [FMarkerList[GetNumberMarker(AMarker)].ID]), exExecute, vQuery);
  end;

  FMarkerList.Delete(GetNumberMarker(AMarker));

end;

procedure TFrameMap.ApplyZoom(ACenterX: Single = -1; ACenterY: Single = -1);
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

procedure TFrameMap.ZoomToPoint(APoint: TPointF; AScale: Double);
begin
  SetZoom(AScale, APoint.X, APoint.Y);
end;

procedure TFrameMap.UpdateZoomControls;
begin
  lblZoom.Text := Format('%.0f%%', [FCurrentScale * 100]);

  // Обновляем состояние кнопок
  btnZoomIn.Enabled := FCurrentScale < FMaxScale;
  btnZoomOut.Enabled := FCurrentScale > FMinScale;
end;

end.
