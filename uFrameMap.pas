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
  FMX.Media, System.IOUtils, FireDAC.Comp.Client, System.Threading, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.Location,
  Androidapi.JNI.Net, uLocationListener;

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
    recPanelDeleteMarker: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    InnerGlowEffect2: TInnerGlowEffect;
    InnerGlowEffect3: TInnerGlowEffect;
    MediaPlayerRad: TMediaPlayer;
    MediaPlayerAnomaly: TMediaPlayer;
    InnerGlowEffect4: TInnerGlowEffect;
    MediaPlayerDead: TMediaPlayer;
    MediaPlayerDamage: TMediaPlayer;
    LayClient: TLayout;
    timerCheckCritical: TTimer;
    timerCritical: TTimer;
    MediaPlayerStartCritical: TMediaPlayer;
    MediaPlayerNotificationCritical: TMediaPlayer;
    MediaPlayerStopCritical: TMediaPlayer;
    layCritical: TLayout;
    Image1: TImage;
    Rectangle4: TRectangle;
    labCritical: TLabel;
    imgArrowMan: TImage;
    Layout7: TLayout;
    layPersonHealth: TLayout;
    ShadowEffect1: TShadowEffect;
    cManRadius: TCircle;
    TimerLocation: TTimer;
    faMarkerX: TFloatAnimation;
    faMarkerY: TFloatAnimation;
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
    procedure timerCriticalTimer(Sender: TObject);
    procedure timerCheckCriticalTimer(Sender: TObject);
    procedure ScrollBoxViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure TimerLocationTimer(Sender: TObject);
  private
    FMapLoaded: Boolean;

    FMinScale: Double;
    FMaxScale: Double;
    FZoomStep: Double;
    FLastDistance: integer;
    FIsZooming: Boolean;
    FLongTap: TPointF;
    FCoords: TLocationCoord2D;
    FMarkerList: TList<TMarkerData>;
    FMarkerIssue: TList<TMarkerData>;
    FSmoothedAzimuth: Double;
    FCurrentScale: Double;
    locationListener: TLocationListener;
    FLocationManager: JLocationManager;
    FSensorLocation, FServiceLocation: TLocationCoord2D;
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
    procedure OnMarkerIssueClick(Sender: TObject);
    function GetNumberMarker(AMarker: TImage): integer;
    procedure ScanAnomalies;
    procedure ScanIssuies;
    procedure UpdateBaseSafeDead;
    procedure ScanBaseSafeDead;
    procedure LoadAnomalies;
    procedure ScanInnerCritical;
    procedure SetArrows(AArrow: TImage; ATarget: TControl);
    procedure UpdateAnomalies;
    procedure LocationServiceChanged;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetLocationMarkers;
    procedure LocationisChanged(Location: JLocation);
    // Масштабирование
    procedure ZoomToPoint(APoint: TPointF; AScale: Double);

    // Свойства только для чтения
    property MapLoaded: Boolean read FMapLoaded;
    property CurrentScale: Double read FCurrentScale;
    procedure UpdateIssue;
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
  FMarkerIssue := TList<TMarkerData>.Create;
  FAnomalyList := TList<TAnomalyData>.Create;

  LoadMap;

  LoadAnomalies;
  UpdateIssue;
  UpdateBaseSafeDead;
  UpdateAnomalies;

  MediaPlayerRad.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zvuk-radiacii.mp3');
  MediaPlayerAnomaly.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'detector.mp3');
  MediaPlayerDead.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zvuk-smerti.mp3');
  MediaPlayerDamage.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'damage.mp3');
  MediaPlayerStartCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'start_critical.mp3');
  MediaPlayerNotificationCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'notification_critical.mp3');
  MediaPlayerStopCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'barmen_after_surge.mp3');
end;

procedure TFrameMap.LoadAnomalies;
var
  vQuery: TFDQuery;
  vAnomalyItem: TAnomalyData;
begin
  FAnomalyList := TList<TAnomalyData>.Create;
  ExeExec('select * from anomalies;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vAnomalyItem.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vAnomalyItem.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vAnomalyItem.Radius := vQuery.FieldByName('radius').AsInteger;
    vAnomalyItem.Power := vQuery.FieldByName('power').AsInteger;

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

  FreeQueryAndConn(vQuery);
end;

procedure TFrameMap.UpdateIssue;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := FMarkerIssue.Count - 1 downto 0 do
  begin
    FMarkerIssue[I].Marker.Parent := nil;
    FMarkerIssue[I].Marker.Visible := False;

    if Assigned(FMarkerIssue[I].Arrow) then
    begin
      FMarkerIssue[I].Arrow.Parent := nil;
      FMarkerIssue[I].Arrow.Visible := False;
    end;

    FMarkerIssue.Delete(I);
  end;

  for I := 0 to FIssueList.Count - 1 do
    if FIssueList[I].Visible then
    begin
      FCoords := FIssueList[I].Coords;
      AMarker.Coords := FCoords;
      AMarker.MarkerType := mtIssue;

      AMarker.LabelText := FIssueList[I].Name;
      AMarker.LabelDetail := FIssueList[I].Detail;

      CreateMarker(AMarker);
    end;
end;

procedure TFrameMap.UpdateBaseSafeDead;
var
  AMarker: TMarkerData;
  I: integer;
begin
  for I := 0 to FPlacesList.Count - 1 do
  begin
    FCoords := FPlacesList[I].Coords;
    AMarker.Coords := FCoords;
    AMarker.MarkerType := FPlacesList[I].MarkerType;
    AMarker.Index := I;
    AMarker.Radius := FPlacesList[I].Radius;
    AMarker.LabelText := FPlacesList[I].Name;

    CreateMarker(AMarker);
  end;
end;

procedure TFrameMap.UpdateAnomalies;
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

procedure TFrameMap.ScanIssuies;
var
  I: integer;
  vDistance: Double;
  vQuery: TFDQuery;
begin
  for I := 0 to FIssueList.Count - 1 do
  begin
    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FIssueList[I].Coords.Latitude, FIssueList[I].Coords.Longitude);

    if (vDistance <= FIssueList[I].RadiusIN) and (FIssueList[I].RadiusIN > 0) then
      if FIssueList[I].CompleteAfterIN then // выполняется при входе в зону действия
      begin
        ExeExec('update issuies set status_id = 1 where issue_id = ' + FIssueList[I].ID.ToString + ';', exExecute, vQuery);
        ExeExec('select cost from issuies where issue_id = ' + FIssueList[I].ID.ToString + ';', exActive, vQuery);
        Person.Cash := Person.Cash + vQuery.FieldByName('cost').AsFloat;
        FreeQueryAndConn(vQuery);

        ExeExec('update issuies set status_id = 0 where prev_issue_id = ' + FIssueList[I].ID.ToString + ';', exExecute, vQuery);
        ReloadIssuies;
        UpdateIssue;
      end;

    if (vDistance > FIssueList[I].RadiusOUT) and (FIssueList[I].RadiusOUT > 0) then
      if FIssueList[I].CompleteAfterOUT then // выполняется при выходе из зоны действия
      begin
        ExeExec('update issuies set status_id = 1 where issue_id = ' + FIssueList[I].ID.ToString + ';', exExecute, vQuery);
        ExeExec('select cost from issuies where issue_id = ' + FIssueList[I].ID.ToString + ';', exActive, vQuery);
        Person.Cash := Person.Cash + vQuery.FieldByName('cost').AsFloat;
        FreeQueryAndConn(vQuery);
        ExeExec('update issuies set status_id = 0 where prev_issue_id = ' + FIssueList[I].ID.ToString + ';', exExecute, vQuery);
        ReloadIssuies;
        UpdateIssue;
      end
      else
      begin // Провалено при выходе из зоны
        ExeExec('update issuies set status_id = 2 where issue_id = ' + FIssueList[I].ID.ToString + ';', exExecute, vQuery);
        ReloadIssuies;
        UpdateIssue;
      end
  end;
end;

procedure TFrameMap.ScrollBoxViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
var
  I: integer;
begin
  SetArrows(imgArrowMan, LocationMarker);

  for I := 0 to FMarkerIssue.Count - 1 do
  begin
    SetArrows(FMarkerIssue[I].Arrow, FMarkerIssue[I].Marker);
  end;
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
      AArrow.Position.X := IfThen(vLoc.X + AArrow.Width > ScrollBox.Width, ScrollBox.Width - AArrow.Width, Max(layPersonHealth.Width, vLoc.X));
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
      AArrow.Position.X := IfThen(AArrow.Position.Y + AArrow.Height > layPersonHealth.Position.Y, layPersonHealth.Width, 0);
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
      AArrow.Position.X := layPersonHealth.Width;
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

procedure TFrameMap.ScanBaseSafeDead;
var
  I: integer;
  vDistance: Double;
begin
  for I := 0 to FPlacesList.Count - 1 do
  begin
    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FPlacesList[I].Coords.Latitude, FPlacesList[I].Coords.Longitude);

    if vDistance <= FPlacesList[I].Radius then
    begin
      Person.Health := Person.Health + 0.11;
    end;
  end;
end;

procedure TFrameMap.ScanInnerCritical;
var
  I: integer;
  vDistance: Double;
  vIsInnerCritical: Boolean;
begin
  vIsInnerCritical := true;

  for I := 0 to FPlacesList.Count - 1 do
  begin

    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FPlacesList[I].Coords.Latitude, FPlacesList[I].Coords.Longitude);

    if vDistance <= FPlacesList[I].Radius then
    begin
      vIsInnerCritical := False;
      break;
    end;
  end;

  if vIsInnerCritical then
  begin
    if (FSecondBeforeStartDamage <= 0) and (NOT FIsDead) then
    begin
      StartDamageGlow;
      Person.Health := Person.Health - 2;
      MediaPlayerDamage.CurrentTime := 0;
      MediaPlayerDamage.Play;
    end;
  end
  else
  begin
    MediaPlayerDamage.Stop;
    MediaPlayerDamage.CurrentTime := 0;
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

procedure TFrameMap.ActAddMarkerExecute(Sender: TObject);
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
        AMarker.MarkerType := mtPointRad;
        AMarker.LabelText := 'Радиация';
      end;
    2:
      begin
        AMarker.MarkerType := mtPointAnomaly;
        AMarker.LabelText := 'Аномалия';
      end;
    3:
      begin
        AMarker.MarkerType := mtPointBag;
        AMarker.LabelText := 'Схрон';
      end;
  end;

  CreateMarker(AMarker);
  MarkersPanel.Visible := False;
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

procedure TFrameMap.LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
begin
  LocationServiceChanged;
  FLoad := true;

  if NewLocation.Latitude <> 0 then
  begin
    FSensorLocation := TLocationCoord2D.Create(NewLocation.Latitude, NewLocation.Longitude);
    FLocation := TLocationCoord2D.Create((FServiceLocation.Latitude + FSensorLocation.Latitude) / 2, (FServiceLocation.Longitude + FSensorLocation.Longitude) / 2);
    LocationMarker.Visible := true;

    SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
    OrientationMarker.RotationAngle := 135 + CalculateBearing(OldLocation, FLocation);
  end;
end;

procedure TFrameMap.LocationisChanged(Location: JLocation);
begin
  if Assigned(Location) then
  begin
    FServiceLocation := TLocationCoord2D.Create(Location.getLatitude, Location.getLongitude);
    FLocation := TLocationCoord2D.Create((FServiceLocation.Latitude + FSensorLocation.Latitude) / 2, (FServiceLocation.Longitude + FSensorLocation.Longitude) / 2);
    SetLocationMarker(FLocation.Latitude, FLocation.Longitude);
  end;
end;

procedure TFrameMap.ScanAnomalies;
var
  I: integer;
  vDistance: Double;
  vBlockDamage: Double;
begin

  for I := 0 to FAnomalyList.Count - 1 do
  begin
    vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[I].Coords.Latitude, FAnomalyList[I].Coords.Longitude);

    if vDistance <= FAnomalyList[I].Radius then
    begin
      MediaPlayerDamage.CurrentTime := 0;
      MediaPlayerDamage.Volume := vDistance / FAnomalyList[I].Radius * 100;
      MediaPlayerDamage.Play;
      StartDamageGlow;

      case FAnomalyList[I].AnomalyType of
        atElectro:
          vBlockDamage := Person.ElectroArmor;
        atFire:
          vBlockDamage := Person.FireArmor;
        atPhisic:
          vBlockDamage := Person.PhisicArmor;
        atRadiation:
          vBlockDamage := Person.RadiationArmor;
        atChimishe:
          vBlockDamage := Person.ChimisheArmor;
        atPSI:
          vBlockDamage := Person.PsiArmor;
      end;

      Person.Health := Person.Health - FAnomalyList[I].Power * ((FAnomalyList[I].Radius - vDistance) / FAnomalyList[I].Radius) * ((100 - vBlockDamage) / 100);
    end
    else if vDistance <= FAnomalyList[I].Radius + 10 then // Сигнализируем за 10 метров до зоны действия
    begin
{$IF Defined(ANDROID)}
      Vibration(50);
{$ENDIF}
      StopDamageGlow;

      if FAnomalyList[I].AnomalyType = atRadiation then
      begin
        if (MediaPlayerRad.State = TMediaState.Stopped) or (MediaPlayerRad.State = TMediaState.Unavailable) then
        begin
          MediaPlayerRad.CurrentTime := 0;
          MediaPlayerRad.Volume := vDistance / FAnomalyList[I].Radius * 100;
          MediaPlayerRad.Play;
        end;
      end
      else
      begin
        MediaPlayerAnomaly.CurrentTime := 0;
        MediaPlayerAnomaly.Volume := vDistance / FAnomalyList[I].Radius * 100;
        MediaPlayerAnomaly.Play;
      end;
    end;
  end;

  for I := 0 to FMarkerList.Count - 1 do
    if FMarkerList[I].MarkerType in [mtRadiation, mtAnomaly] then
    begin
      vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[FMarkerList[I].Index].Coords.Latitude, FAnomalyList[FMarkerList[I].Index].Coords.Longitude);
      if Person.Detector.Level = 4 then
        FMarkerList[I].Marker.Visible := vDistance <= Person.Detector.Radius
      else
        FMarkerList[I].Marker.Visible := False;
    end;
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

  if Person.Detector.Level = 4 then
  begin
    cManRadius.Width := FOriginalMapWidth / FMapRealWidth * Person.Detector.Radius * 2 * FCurrentScale;
    cManRadius.Height := cManRadius.Width;
  end
  else
  begin
    cManRadius.Width := 0;
    cManRadius.Height := 0;
  end;

  if not FMapLoaded then
    Exit;

  Point := CoordinatesToPixels(Lat, Lon);

  // Учитываем масштаб при позиционировании маркера
  Point.X := Point.X * FCurrentScale;
  Point.Y := Point.Y * FCurrentScale;

  LocationMarker.Visible := true;
  LocationMarker.BringToFront;

  faMarkerX.Stop;
  faMarkerX.StartValue := LocationMarker.Position.X;
  faMarkerX.StopValue := Point.X - LocationMarker.Width / 2;

  faMarkerY.Stop;
  faMarkerY.StartValue := LocationMarker.Position.Y;
  faMarkerY.StopValue := Point.Y - LocationMarker.Height / 2;;

  faMarkerX.Start;
  faMarkerY.Start;

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

procedure TFrameMap.MapImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
  function GetPoints: integer;
  var
    I: integer;
  begin
    Result := 0;

    for I := 0 to FMarkerList.Count - 1 do
      if FMarkerList[I].MarkerType in [mtPoint, mtPointRad, mtPointAnomaly, mtPointBag] then
        Result := Result + 1;
  end;

begin
  FCoords := PixelsToCoordinates(FLongTap.X, FLongTap.Y);
  btnAddMarker.Enabled := GetPoints < 10;
  btnAddMarkerRad.Enabled := GetPoints < 10;
  btnAddMarkerBag.Enabled := GetPoints < 10;
  btnAddMarkerAnomaly.Enabled := GetPoints < 10;
  labMarkerCount.Text := GetPoints.ToString + '/10';
  // Устанавливаем маркер
  SetMarker(MarkersPanel, FCoords.Latitude, FCoords.Longitude);
  MarkersPanel.Visible := true;
  MarkersPanel.BringToFront;
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
  // AMarker.Visible := True;
end;

procedure TFrameMap.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MarkersPanel.Visible := False;
  LayDetailMarker.Visible := False;
  layDetailIssue.Visible := False;
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
  // ApplyZoom(vOldViewportPositionX, vOldViewportPositionY);
  ResetLocationMarkers;
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
  // ApplyZoom(vOldViewportPositionX, vOldViewportPositionY);
  ResetLocationMarkers;
end;

procedure TFrameMap.btnDelMarkerClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := true;
  btnDelMarker.Visible := False;
end;

procedure TFrameMap.btnMyLocationClick(Sender: TObject);
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
    MarkerCenter := (TPointF.Create((LocationMarker.LocalToAbsolute(TPointF.Zero).X + LocationMarker.Width / 2) * MapLayout.Scale.X, (LocationMarker.LocalToAbsolute(TPointF.Zero).Y + LocationMarker.Height / 2)) * MapLayout.Scale.Y);
    MarkerCenter := ScrollBox.AbsoluteToLocal(MarkerCenter);

    // Вычисляем целевую позицию прокрутки для центрирования маркера
    TargetX := MarkerCenter.X - ScrollBox.Width / 2;
    TargetY := MarkerCenter.Y - ScrollBox.Height / 2;

    // Устанавливаем позицию прокрутки
    ScrollBox.ViewportPosition := TPointF.Create(TargetX, TargetY);
  end;
end;

procedure TFrameMap.OnMarkerClick(Sender: TObject);
begin
  LayDetailMarker.Parent := (Sender as TImage);
  LayDetailMarker.Position.X := (Sender as TImage).Width / 2 - LayDetailMarker.Width / 2;
  LayDetailMarker.Position.Y := (Sender as TImage).Height / 2 - LayDetailMarker.Height / 2;
  labMarkerText.Text := FMarkerList[GetNumberMarker(Sender as TImage)].LabelText;
  btnDeleteYes.TagObject := (Sender as TImage);
  LayDetailMarker.Visible := true;
  layDetailIssue.Visible := False;
  recPanelDeleteMarker.Visible := NOT(FMarkerList[GetNumberMarker(Sender as TImage)].MarkerType in [mtBase, mtSafe, mtRadiation, mtAnomaly, mtArtefact]);
  btnDelMarker.Visible := recPanelDeleteMarker.Visible;
  gplDeleteYesNo.Visible := False;
  (Sender as TImage).BringToFront;
end;

procedure TFrameMap.OnMarkerIssueClick(Sender: TObject);
begin
  layDetailIssue.Parent := (Sender as TImage);
  layDetailIssue.Position.X := (Sender as TImage).Width;
  layDetailIssue.Position.Y := 0;
  labIssueText.Text := FMarkerIssue[GetNumberMarker(Sender as TImage)].LabelText;
  labIssueDetail.Text := FMarkerIssue[GetNumberMarker(Sender as TImage)].LabelDetail;
  layDetailIssue.Visible := true;
  LayDetailMarker.Visible := False;
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

  for I := 0 to FMarkerIssue.Count - 1 do
    if FMarkerIssue[I].Marker = AMarker then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TFrameMap.CreateMarker(AMarker: TMarkerData);
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
      HitTest := False;
    end;
  end;

begin
  AMarker.Marker := TImage.Create(MapLayout);
  AMarker.Marker.Parent := MapLayout;
  AMarker.Marker.Width := 40;
  AMarker.Marker.Height := AMarker.Marker.Width;
  AMarker.Marker.OnClick := OnMarkerClick;

  case AMarker.MarkerType of
    mtPoint:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[0].MultiResBitmap[0].Bitmap);
    mtPointRad:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[1].MultiResBitmap[0].Bitmap);
    mtPointAnomaly:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[2].MultiResBitmap[0].Bitmap);
    mtPointBag:
      AMarker.Marker.Bitmap.Assign(ImageList.Source[3].MultiResBitmap[0].Bitmap);
    mtIssue:
      begin

        AMarker.Marker.Bitmap.Assign(ImageList.Source[4].MultiResBitmap[0].Bitmap);

        AMarker.Arrow := TImage.Create(LayClient);
        AMarker.Arrow.Width := 22;
        AMarker.Arrow.Height := AMarker.Arrow.Width;
        AMarker.Arrow.Bitmap.Assign(ImageList.Source[7].MultiResBitmap[0].Bitmap);
        AMarker.Arrow.Parent := LayClient;

        TShadowEffect.Create(AMarker.Arrow).Parent := AMarker.Arrow;

        AMarker.Marker.OnClick := OnMarkerIssueClick;
      end;
    mtBase:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FPlacesList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Height := AMarker.Marker.Width;
        AMarker.Marker.Tag := Round(AMarker.Marker.Width / FCurrentScale);

        CreateBackground;
        CreateIcon(ImageList.Source[6].MultiResBitmap[0].Bitmap, 60);

      end;
    mtSafe:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FPlacesList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Height := AMarker.Marker.Width;
        AMarker.Marker.Tag := Round(AMarker.Marker.Width / FCurrentScale);

        CreateBackground;
        CreateIcon(ImageList.Source[5].MultiResBitmap[0].Bitmap, 50);
      end;
    mtRadiation:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Height := AMarker.Marker.Width;
        AMarker.Marker.Tag := Round(AMarker.Marker.Width / FCurrentScale);
        AMarker.LabelText := 'Радиация';
        CreateBackground;
        CreateIcon(ImageList.Source[9].MultiResBitmap[0].Bitmap);
      end;
    mtAnomaly:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(AMarker.Marker.Width / FCurrentScale);
        AMarker.Marker.Height := AMarker.Marker.Width;

        CreateBackground;

        case FAnomalyList[AMarker.Index].AnomalyType of
          atElectro:
            begin
              CreateIcon(ImageList.Source[10].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Электрическая аномалия';
            end;
          atFire:
            begin
              CreateIcon(ImageList.Source[12].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Термическая аномалия';
            end;
          atPhisic:
            begin
              CreateIcon(ImageList.Source[13].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Гравитационная аномалия';
            end;
          atChimishe:
            begin
              CreateIcon(ImageList.Source[11].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'Химическая аномалия';
            end;
          atPSI:
            begin
              CreateIcon(ImageList.Source[8].MultiResBitmap[0].Bitmap);
              AMarker.LabelText := 'ПСИ-излучение';
            end;
        end;
      end;
  end;

  SetMarker(AMarker.Marker, FCoords.Latitude, FCoords.Longitude);

  if AMarker.MarkerType = mtIssue then
    FMarkerIssue.Add(AMarker)
  else
    FMarkerList.Add(AMarker);
end;

procedure TFrameMap.ResetLocationMarkers;
var
  I: integer;
begin

  for I := 0 to FMarkerList.Count - 1 do
  begin
    if (FMarkerList[I].MarkerType in [mtAnomaly, mtRadiation, mtBase, mtSafe]) then
    begin
      FMarkerList[I].Marker.Width := FMarkerList[I].Marker.Tag * FCurrentScale;
      FMarkerList[I].Marker.Height := FMarkerList[I].Marker.Width;
    end;
    SetMarker(FMarkerList[I].Marker, FMarkerList[I].Coords.Latitude, FMarkerList[I].Coords.Longitude);
  end;

  for I := 0 to FMarkerIssue.Count - 1 do
    SetMarker(FMarkerIssue[I].Marker, FMarkerIssue[I].Coords.Latitude, FMarkerIssue[I].Coords.Longitude);

  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);

  if MarkersPanel.Visible then
    SetMarker(MarkersPanel, FCoords.Latitude, FCoords.Longitude);
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

      // Если указана точка центра, корректируем позицию прокрутки
      if (ACenterX >= 0) and (ACenterY >= 0) then
      begin
        ApplyZoom(ACenterX, ACenterY);
      end;
    end;
  end;
end;

procedure TFrameMap.timerCheckCriticalTimer(Sender: TObject);
var
  I: integer;
  a: tdatetime;
  vIssue: TIssueData;
begin
  if Assigned(FCritical) then
  begin
    if Not FIsCriticalStart then
    begin

      for I := 0 to FCritical.Count - 1 do
        if FormatDateTime('h:n', Time()) = FormatDateTime('h:n', FCritical[I].TimeStart) then
        begin
          FCurrentCritical := FCritical[I];
          FSecondBeforeStartDamage := FCurrentCritical.MinuteBeforeStartDamage * 60;
          timerCritical.Enabled := true;
          FIsCriticalStart := true;
          labCritical.Text := 'Приближается выброс';
          break;
        end;

      if FIsCriticalStart then
      begin
        for I := 0 to FIssueList.Count - 1 do
        begin
          vIssue := FIssueList[I];
          vIssue.Visible := False;
          FIssueList[I] := vIssue;
        end;

        for I := 0 to FPlacesList.Count - 1 do
        begin
          vIssue.Coords := FPlacesList[I].Coords;
          vIssue.Name := 'Укрыться от выброса';
          vIssue.Detail := 'Найти убежище и переждать выброс';
          vIssue.Cost := 0;
          vIssue.RadiusIN := FPlacesList[I].Radius;
          vIssue.RadiusOUT := 0;
          vIssue.CompleteAfterIN := true;
          vIssue.CompleteAfterOUT := False;
          vIssue.BlockDetail := 'critical';
          vIssue.Visible := true;
          FIssueList.Add(vIssue);
        end;

        UpdateIssue;
      end;

    end
    else
    begin
      if FormatDateTime('h:n', Time()) = FormatDateTime('h:n', FCurrentCritical.TimeStop) then
      begin
        FIsCriticalStart := False;
        MediaPlayerStartCritical.Stop;
        MediaPlayerStopCritical.CurrentTime := 0;
        MediaPlayerStopCritical.Play;

        for I := FIssueList.Count - 1 downto 0 do
        begin
          if FIssueList[I].BlockDetail = 'critical' then
          begin
            FIssueList.Delete(I);
          end
          else
          begin
            vIssue := FIssueList[I];
            vIssue.Visible := true;
            FIssueList[I] := vIssue;
          end;
        end;

        UpdateIssue;
      end;
    end;
  end;

  layCritical.Visible := FIsCriticalStart;
end;

procedure TFrameMap.timerCriticalTimer(Sender: TObject);
begin
  if FIsCriticalStart then
  begin
    if FSecondBeforeStartDamage > 0 then
    begin
      if FSecondBeforeStartDamage < 30 then
        labCritical.Text := 'Выброс скоро начнется';

      if FSecondBeforeStartDamage mod 20 = 0 then
      begin
        MediaPlayerNotificationCritical.CurrentTime := 0;
        MediaPlayerNotificationCritical.Play;
      end;

      ScanInnerCritical;
      Dec(FSecondBeforeStartDamage);

      if MediaPlayerNotificationCritical.CurrentTime = MediaPlayerNotificationCritical.Duration then
        MediaPlayerNotificationCritical.Stop;
    end
    else
    begin
{$IF Defined(ANDROID)}
      Vibration(800);
{$ENDIF}
      labCritical.Text := 'Начался выброс';
      ScanInnerCritical;
      MediaPlayerNotificationCritical.Stop;
      MediaPlayerNotificationCritical.CurrentTime := 0;
      MediaPlayerStartCritical.Play;

      if MediaPlayerStartCritical.CurrentTime = MediaPlayerStartCritical.Duration then
        MediaPlayerStartCritical.CurrentTime := 0;
    end;

  end;
end;

procedure TFrameMap.TimerLocationTimer(Sender: TObject);
begin
  if FLoad then
  begin
    LocationSensor.Active := False;
    LocationSensor.Active := true;
    FLoad := False;
  end;
end;

procedure TFrameMap.TimerSensorTimer(Sender: TObject);
begin
  if Assigned(Person) then
    if Person.Health < 100 then
      ScanBaseSafeDead;

  if Not FIsDead then
  begin
    ScanAnomalies;
    ScanIssuies;
  end;
end;

procedure TFrameMap.btnDeleteNoClick(Sender: TObject);
begin
  gplDeleteYesNo.Visible := False;
  btnDelMarker.Visible := true;
end;

procedure TFrameMap.btnDeleteYesClick(Sender: TObject);
var
  AMarker: TImage;
begin
  AMarker := (Sender as TSpeedButton).TagObject as TImage;
  AMarker.Visible := False;
  FMarkerList.Delete(GetNumberMarker(AMarker));
  gplDeleteYesNo.Visible := False;
  btnDelMarker.Visible := true;
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
