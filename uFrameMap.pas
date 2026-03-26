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
  uLocationListener, uGenericBaseData, classes.send, classes.marker, REST.Json, FMX.Surfaces;

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
    OrientationMarker: TImage;
    btnMyLocation: TButton;
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
    gplDeleteYesNo: TGridPanelLayout;
    btnDeleteNo: TSpeedButton;
    btnDeleteYes: TSpeedButton;
    recPanelDeleteMarker: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    InnerGlowEffect2: TInnerGlowEffect;
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
    faMarkerX: TFloatAnimation;
    faMarkerY: TFloatAnimation;
    timerScanAnomaliesNextTo: TTimer;
    layTopMenu: TLayout;
    layClock: TLayout;
    labTime: TLabel;
    layBattery: TLayout;
    GlowEffect1: TGlowEffect;
    Image3: TImage;
    BatteryProgress: TRectangle;
    TimerSystem: TTimer;
    Layout5: TLayout;
    btnExit: TSpeedButton;
    layBtnKill: TLayout;
    btnKill: TSpeedButton;
    Image5: TImage;
    MediaPlayerScanAnomaly: TMediaPlayer;
    Layout3: TLayout;
    btnSendMarkers: TSpeedButton;
    laySendQR: TLayout;
    recSkin1: TRectangle;
    InnerGlowEffect16: TInnerGlowEffect;
    imgQR: TImage;
    Layout9: TLayout;
    btnCloseQR: TSpeedButton;
    InnerGlowEffect9: TInnerGlowEffect;
    gplMenuMarker: TGridPanelLayout;
    btnDelMarker: TSpeedButton;
    Image2: TImage;
    InnerGlowEffect3: TInnerGlowEffect;
    btnSendMarker: TSpeedButton;
    InnerGlowEffect5: TInnerGlowEffect;
    InnerGlowEffect6: TInnerGlowEffect;
    InnerGlowEffect7: TInnerGlowEffect;
    imgFog: TImage;
    imgFogDefault: TImage;
    Rectangle5: TRectangle;
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
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
    procedure timerScanAnomaliesNextToTimer(Sender: TObject);
    procedure TimerSystemTimer(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnKillClick(Sender: TObject);
    procedure btnSendMarkersClick(Sender: TObject);
    procedure btnSendMarkerClick(Sender: TObject);
    procedure LayClientClick(Sender: TObject);
    procedure btnCloseQRClick(Sender: TObject);
  private
    FMapLoaded: Boolean;

    FMinScale: Double;
    FMaxScale: Double;
    FZoomStep: Double;
    FLongTap: TPointF;
    FCoords: TLocationCoord2D;
    FMarkerList: TList<TMarkerData>;
    FMarkerIssue: TList<TMarkerData>;
    FCurrentScale: Double;
{$IFDEF ANDROID}
    locationListener: TLocationListener;
    FLocationManager: JLocationManager;
{$ENDIF}
    FServiceLocation: TLocationCoord2D;
    FLoad: Boolean;
    procedure LoadMap;
    procedure SetLocationMarker(Lat, Lon: Double);
    function CoordinatesToPixels(Lat, Lon: Double): TPointF;
    function PixelsToCoordinates(X, Y: Single): TLocationCoord2D;
    procedure UpdateZoomControls;
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
    procedure ScanAnomaliesNextTo;
    procedure EraseCircleFromImage(AImage: TImage; CenterX, CenterY, Radius: single);
    procedure SetBitmap(ABitmap: TBitmap);
{$IFDEF ANDROID}
    procedure SetLocation;
    function BatteryPercent: integer;
{$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetLocationMarkers;
    procedure LoadMarkers;
{$IFDEF ANDROID}
    procedure LocationServiceChanged;
    procedure LocationisChanged(Location: JLocation);
{$ENDIF}
    // Масштабирование
    procedure ZoomToPoint(APoint: TPointF; AScale: Double);

    // Свойства только для чтения
    property MapLoaded: Boolean read FMapLoaded;
    property CurrentScale: Double read FCurrentScale;
    procedure UpdateIssue;
    procedure NewMarkerToMap(ACoords: TLocationCoord2D; AText: String; AMarkerType: TMarkerType; AIsOwner: boolean = true);
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
  layClock.Visible := TOSVersion.Check(11);
  layBattery.Visible := TOSVersion.Check(11);
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

  LoadMap;

  LoadMarkers;
  LoadAnomalies;
  UpdateIssue;
  UpdateBaseSafeDead;
  UpdateAnomalies;

  MediaPlayerRad.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zvuk-radiacii.mp3');
  MediaPlayerScanAnomaly.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'detector.mp3');
  MediaPlayerDead.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zvuk-smerti.mp3');
  MediaPlayerDamage.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'damage.mp3');
  MediaPlayerStartCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'start_critical.mp3');
  MediaPlayerNotificationCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'notification_critical.mp3');
  MediaPlayerStopCritical.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'barmen_after_surge.mp3');
  labTime.TextSettings.Font.Family := 'lcd';
end;

{$IFDEF ANDROID}

function TFrameMap.BatteryPercent: integer;
var
  IntentFilter: JIntentFilter;
  Intent: JIntent;
  Level, Scale: integer;
begin
  Result := -1;
  // Создаем фильтр для получения изменений батареи
  IntentFilter := TJIntentFilter.Create;
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_BATTERY_CHANGED);

  // Регистрация приемника (null - нет конкретного слушателя, IntentFilter дает состояние)
  Intent := TAndroidHelper.Context.registerReceiver(nil, IntentFilter);

  // Получаем уровень и масштаб
  Level := Intent.getIntExtra(StringToJString('level'), -1);
  Scale := Intent.getIntExtra(StringToJString('scale'), -1);

  if (Level <> -1) and (Scale <> -1) then
    Result := Round((Level / Scale) * 100);
end;
{$ENDIF}

procedure TFrameMap.LayClientClick(Sender: TObject);
begin
  BtnClickMedia;
  gplDeleteYesNo.Visible := False;
  gplMenuMarker.Visible := true;
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

procedure TFrameMap.LoadMarkers;
var
  vQuery: TFDQuery;
  ACoords: TLocationCoord2D;
  vIsOwner: Boolean;
  I: integer;
begin
  for I := FMarkerList.Count - 1 downto 0 do
  if FMarkerList[I].MarkerType in [mtPoint, mtPointRad, mtPointAnomaly, mtPointBag] then
  begin
    FMarkerList[I].Marker.Parent := nil;
    FMarkerList[I].Marker.Visible := False;
    FMarkerList.Delete(I);
  end;

  ExeExec('select * from markers;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    ACoords.Latitude := vQuery.FieldByName('lat').AsFloat;
    ACoords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vIsOwner := vQuery.FieldByName('is_owner').AsBoolean;

    case vQuery.FieldByName('marker_type_id').AsInteger of
      0:
        if vIsOwner then
          NewMarkerToMap(ACoords, 'Моя точка', mtPoint)
        else
          NewMarkerToMap(ACoords, 'Чужая точка', mtPoint, false);
      1:
          NewMarkerToMap(ACoords, 'Радиация', mtPointRad, vIsOwner);
      2:
          NewMarkerToMap(ACoords, 'Аномалия', mtPointAnomaly, vIsOwner);
      3:
        if vIsOwner then
          NewMarkerToMap(ACoords, 'Мой схрон', mtPoint)
        else
          NewMarkerToMap(ACoords, 'Чужой схрон', mtPoint, false);
    end;
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
        Person.Cash := Person.Cash + vQuery.FieldByName('cost').AsInteger;
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
        Person.Cash := Person.Cash + vQuery.FieldByName('cost').AsInteger;
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
    if (FSecondBeforeStartDamage <= 0) and (NOT Person.IsDead) then
    begin
      FKillType := ktCritical;
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
  vMarkerType: integer;
  vQuery: TFDQuery;
begin
  vMarkerType := (Sender as TSpeedButton).Tag;

  case vMarkerType of
    0:
        NewMarkerToMap(FCoords, 'Моя точка', mtPoint);
    1:
        NewMarkerToMap(FCoords, 'Радиация', mtPointRad);
    2:
        NewMarkerToMap(FCoords, 'Аномалия', mtPointAnomaly);
    3:
        NewMarkerToMap(FCoords, 'Схрон', mtPointBag);
  end;

  ExeExec(Format('insert into markers (lat, lon, marker_type_id, is_owner) values (%s, %s, %d, true);',[StringReplace(FCoords.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FCoords.Longitude.ToString, ',', '.', [rfReplaceAll]), vMarkerType]), exExecute, vQuery);
  MarkersPanel.Visible := False;
end;

procedure TFrameMap.NewMarkerToMap(ACoords: TLocationCoord2D; AText: String; AMarkerType: TMarkerType; AIsOwner: boolean = true);
var
  AMarker: TMarkerData;
begin
  AMarker.Coords := ACoords;
  AMarker.MarkerType := AMarkerType;
  AMarker.LabelText := AText;
  AMarker.IsOwner := AIsOwner;
  CreateMarker(AMarker);
end;

procedure TFrameMap.LoadMap;
var
  vQuery: TFDQuery;
  vBitmapFog: TBitmap;
begin
  try
    MapImage.Bitmap.LoadFromFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'map_image.png'));

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

   vBitmapFog := TBitmap.Create(Round(FOriginalMapWidth), Round(FOriginalMapHeight));

   try
     if FileExists(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png')) then
         vBitmapFog.LoadFromFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png'))
     else
     begin
       vBitmapFog := TBitmap.Create(Round(FOriginalMapWidth), Round(FOriginalMapHeight));
       vBitmapFog.Canvas.BeginScene;
       vBitmapFog.Canvas.DrawBitmap(imgFogDefault.Bitmap,RectF(0, 0, imgFogDefault.Bitmap.Width, imgFogDefault.Bitmap.Height),RectF(0, 0, FOriginalMapWidth, FOriginalMapHeight),1);
       vBitmapFog.Canvas.EndScene;
     end;

     ImgFog.Bitmap.Assign(vBitmapFog);
     ImgFog.Repaint;

     FSurfaceFog := TBitmapSurface.Create;
     FSurfaceFog.Assign(vBitmapFog);
   finally
     vBitmapFog.Free;
   end;

   FMapLoaded := true;
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
  ProviderName: JString;
begin
  try
     if not Assigned(FLocationManager) then
     begin
       LocationManagerService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
       FLocationManager := TJLocationManager.Wrap((LocationManagerService as ILocalObject).GetObjectID);

       if not Assigned(locationListener) then
         locationListener := TLocationListener.Create();
     end;

     try
       if TOSVersion.Check(12) then
         ProviderName := TJLocationManager.JavaClass.FUSED_PROVIDER
       else
         ProviderName := TJLocationManager.JavaClass.GPS_PROVIDER;

       FLocationManager.requestLocationUpdates(ProviderName, 0, 0, locationListener, TJLooper.JavaClass.getMainLooper);

       Location := FLocationManager.getLastKnownLocation(ProviderName);
       LocationisChanged(Location);
     finally
     end;
  finally
  end;
end;

procedure TFrameMap.LocationisChanged(Location: JLocation);
begin
  if Assigned(Location) then
  begin
    FServiceLocation := TLocationCoord2D.Create(Location.getLatitude, Location.getLongitude);
    FLocation := TLocationCoord2D.Create(FServiceLocation.Latitude, FServiceLocation.Longitude);
    SetLocation;
  end;
end;

procedure TFrameMap.SetLocation;
var
 Point: TPointF;
begin
  LocationMarker.Visible := true;
  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);

  Point := CoordinatesToPixels(FLocation.Latitude, FLocation.Longitude);

  if (Point.X <> 0) and FMapLoaded then
    EraseCircleFromImage(ImgFog, Point.X, Point.Y , 50 / FMapRealWidth * FOriginalMapWidth);

  OrientationMarker.RotationAngle := 135 + CalculateBearing(FOldLocation, FLocation);
  FOldLocation := FLocation;
end;
{$ENDIF}

procedure TFrameMap.ScanAnomalies;
var
  I: integer;
  vDistance: Double;
  vBlockDamage: Double;
  FileName: string;
begin

  for I := 0 to FAnomalyList.Count - 1 do
    if NOT((Person.GroupId = 5) and (FAnomalyList[I].AnomalyType = atPSI)) then // 5- Монолит. ПСИ не действует на монлоит
    begin
      vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[I].Coords.Latitude, FAnomalyList[I].Coords.Longitude);

      if (vDistance <= FAnomalyList[I].Radius) then
      begin
        SetMediaVolume(100);
        MediaPlayerDamage.CurrentTime := 0;
        MediaPlayerDamage.Volume := 1;
        MediaPlayerDamage.Play;
        StartDamageGlow;

        FKillType := ktAnomaly;

        case FAnomalyList[I].AnomalyType of
          atElectro:
          begin
            vBlockDamage := Person.ElectroArmor;
            FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'electro.mp3');
          end;
          atFire:
          begin
            vBlockDamage := Person.FireArmor;
            FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'fire.mp3');
          end;
          atPhisic:
          begin
            vBlockDamage := Person.PhisicArmor;
            FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'phisic.mp3');
          end;
          atRadiation:
            begin
              vBlockDamage := Person.RadiationArmor;
              FKillType := ktRadiation;
              FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'radiation.mp3');
            end;
          atChimishe:
          begin
            vBlockDamage := Person.ChimisheArmor;
            FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'chimishe.mp3');
          end;
          atPSI:
            begin
              vBlockDamage := Person.PsiArmor;
              FKillType := ktPSI;
              FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'psi.mp3');
            end;
        end;

        if MediaPlayerAnomaly.State <> TMediaState.Playing then
         begin
           MediaPlayerAnomaly.FileName := FileName;
           MediaPlayerAnomaly.CurrentTime := 0;
           MediaPlayerAnomaly.Play;
         end;

        if (MediaPlayerAnomaly.CurrentTime = MediaPlayerAnomaly.Duration) then
          MediaPlayerAnomaly.Stop;

        Person.Health := Person.Health - FAnomalyList[I].Power * ((FAnomalyList[I].Radius - vDistance) / FAnomalyList[I].Radius) * ((100 - vBlockDamage) / 100);
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

    if (MediaPlayerAnomaly.State = TMediaState.Playing)  then
      TTask.Run(
      procedure
      begin
        while (MediaPlayerAnomaly.State = TMediaState.Playing)  do
          continue;
      end);
end;

procedure TFrameMap.ScanAnomaliesNextTo; // Приближение к аномалии
var
  I: integer;
  vDistance: Double;
begin
  timerScanAnomaliesNextTo.Interval := 1000;

  for I := 0 to FAnomalyList.Count - 1 do
    if NOT((Person.GroupId = 5) and (FAnomalyList[I].AnomalyType = atPSI)) then // 5- Монолит. ПСИ не действует на монлоит
    begin
      vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FAnomalyList[I].Coords.Latitude, FAnomalyList[I].Coords.Longitude);

      if (vDistance > FAnomalyList[I].Radius) and (vDistance <= FAnomalyList[I].Radius + 10) then // Сигнализируем за 10 метров до зоны действия
      begin
{$IF Defined(ANDROID)}
        Vibration(50);
{$ENDIF}
        StopDamageGlow;

        timerScanAnomaliesNextTo.Interval := Round((vDistance - FAnomalyList[I].Radius) * 100);

        if FAnomalyList[I].AnomalyType = atRadiation then
        begin
          if (MediaPlayerRad.State = TMediaState.Stopped) or (MediaPlayerRad.State = TMediaState.Unavailable) then
          begin
            MediaPlayerRad.CurrentTime := 0;
            MediaPlayerRad.Play;
          end;
        end
        else
        begin
          MediaPlayerScanAnomaly.CurrentTime := 0;
          MediaPlayerScanAnomaly.Play;
        end;
      end;
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

procedure TFrameMap.EraseCircleFromImage(AImage: TImage; CenterX, CenterY, Radius: single);
var
  X, Y: Integer;
  DistSqr, RadiusSqr: Single;
  MinX, MaxX, MinY, MaxY: Integer;
  PixelPtr: PAlphaColor;
begin
  RadiusSqr := Radius * Radius;

  try
    MinX := Round(Max(0, CenterX - Radius));
    MaxX := Round(Min(AImage.Bitmap.Width - 1, CenterX + Radius));
    MinY := Round(Max(0, CenterY - Radius));
    MaxY := Round(Min(AImage.Bitmap.Height - 1, CenterY + Radius));

    for Y := MinY to MaxY do
    begin
      PixelPtr := FSurfaceFog.Scanline[Y];
      Inc(PixelPtr, MinX);

      for X := MinX to MaxX do
      begin
        DistSqr := (X - CenterX) * (X - CenterX) + (Y - CenterY) * (Y - CenterY);

        if DistSqr <= RadiusSqr then
          begin
            if PixelPtr^ <> TAlphaColorRec.Null then
              PixelPtr^ := TAlphaColorRec.Null;
          end;
        Inc(PixelPtr);
      end;
    end;

    TTask.Run(procedure
    begin
      AImage.Bitmap.Assign(FSurfaceFog);
      AImage.Repaint;
    end)

  finally
  end;
  
end;

procedure TFrameMap.SetBitmap(ABitmap: TBitmap);
begin
ImgFog.Visible := true;
  ImgFog.Bitmap.Assign(ABitmap);
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
      if (FMarkerList[I].MarkerType in [mtPoint, mtPointRad, mtPointAnomaly, mtPointBag]) and (FMarkerList[I].IsOwner = true) then
        Result := Result + 1;
  end;

begin
  if not Person.IsDead then
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
  BtnClickMedia;





































































  LayDetailMarker.Visible := false;
  layDetailIssue.Visible := false;
  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale + FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale + FZoomStep);
  
  TTask.Run(procedure
  begin
    MapLayout.Visible := false;
    ZoomIn;


    TThread.Synchronize(nil,
    procedure
    begin
      ResetLocationMarkers;
      MapLayout.Visible := true;
      ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
    end)
  end)
end;

procedure TFrameMap.btnZoomOutClick(Sender: TObject);
 var
    vOldViewportPositionX: Single;
    vOldViewportPositionY: Single;
 begin
  BtnClickMedia;
  LayDetailMarker.Visible := false;
  layDetailIssue.Visible := false;
  vOldViewportPositionX := ScrollBox.ViewportPosition.X / FCurrentScale * (FCurrentScale - FZoomStep);
  vOldViewportPositionY := ScrollBox.ViewportPosition.Y / FCurrentScale * (FCurrentScale - FZoomStep);


  TTask.Run(procedure
  begin
    MapLayout.Visible := false;
    ZoomOut;


    TThread.Synchronize(nil,
    procedure
    begin
      ResetLocationMarkers;
      MapLayout.Visible := true;
      ScrollBox.ViewportPosition := TPointF.Create(vOldViewportPositionX, vOldViewportPositionY);
    end)
  end)
end;

procedure TFrameMap.btnDelMarkerClick(Sender: TObject);
begin
  BtnClickMedia;
  gplDeleteYesNo.Visible := true;
  gplMenuMarker.Visible := False;
end;

procedure TFrameMap.btnExitClick(Sender: TObject);
begin
  BtnClickMedia;
  MessageDlg('Выйдя из сети твой профиль будет удален. Ты хочешь покинуть сталкерскую сеть?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    var
      vQuery: TFDQuery;
    begin
      if (AResult = mrYes) then
      begin
        Person.UserId := -1;
        ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [10, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]),
          exExecute, vQuery);
        ExeExec(DeleteAllSQL, exExecute, vQuery);

        if FileExists(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png')) then
          DeleteFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png'));

        MessageDlg('Ты вышел из сети. Для повторного входа войди в ПДА.', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0,
          procedure(const AResult: TModalResult)
          begin
            if (AResult = mrOk) then
            begin
              Application.Terminate;
            end;
          end);
      end;
    end);
end;

procedure TFrameMap.btnKillClick(Sender: TObject);
begin
  Person.Health := 80;
  BtnClickMedia;
  MessageDlg('Ты умер в бою?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
      begin
        FKillType := ktWeapon;
        Person.Health := 0;
      end;
    end);
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

procedure TFrameMap.btnSendMarkerClick(Sender: TObject);
var
  vSend : TSend;
  AMarker : TImage;
begin
  BtnClickMedia;
  gplDeleteYesNo.Visible := false;
  gplMenuMarker.Visible := true;
  LayDetailMarker.Visible := false;
  laySendQR.Visible := true;

  vSend := TSend.Create;
  vSend.Marker := TMarker.Create;
  AMarker := (Sender as TSpeedButton).TagObject as TImage;
  vSend.Marker.Coords := FMarkerList[GetNumberMarker(AMarker)].Coords;

  case FMarkerList[GetNumberMarker(AMarker)].MarkerType of
    mtPoint:
        vSend.Marker.MarkerType := 0;
    mtPointRad:
        vSend.Marker.MarkerType := 1;
    mtPointAnomaly:
        vSend.Marker.MarkerType := 2;
    mtPointBag:
        vSend.Marker.MarkerType := 3;
  end;

  GenerateQRCode(TJson.ObjectToJsonString(vSend), imgQR);
end;

procedure TFrameMap.btnSendMarkersClick(Sender: TObject);
begin
  MessageDlg('Ты отправляешь свои маркеры всей своей группировке. Эти данные может скачать любой сталкер из твоей группировки. Данные будут переданы при подключении к сталкерской сети. Ты согласен?', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
          procedure(const AResult: TModalResult)
          var
            vQuery: TFDQuery;
          begin
            if (AResult = mrYes) then
            begin
              FIsSendMarkers := true;
              ExeExec(Format('insert into notifications(name, detail, is_open, group_id, is_owner, data) values (''Доступны новые маркеры'', ''Создал: %s \n\nСообщение: \nЯ делюсь своими маркерами с вами:\nвсего маркеров - '' || (select count(1) from markers where is_owner = true), false, %d, true, (select data from notifications_out));',[Person.UserName ,Person.GroupId]), exExecute, vQuery);
            end;
          end);
end;

procedure TFrameMap.OnMarkerClick(Sender: TObject);
begin
  LayDetailMarker.Parent := (Sender as TImage);
  LayDetailMarker.Position.X := (Sender as TImage).Width / 2 - LayDetailMarker.Width / 2;
  LayDetailMarker.Position.Y := (Sender as TImage).Height / 2 - LayDetailMarker.Height / 2;
  labMarkerText.Text := FMarkerList[GetNumberMarker(Sender as TImage)].LabelText;
  btnDeleteYes.TagObject := (Sender as TImage);
  btnSendMarker.TagObject := (Sender as TImage);
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
      begin
        if AMarker.IsOwner then
          AMarker.Marker.Bitmap.Assign(ImageList.Source[0].MultiResBitmap[0].Bitmap)
        else
          AMarker.Marker.Bitmap.Assign(ImageList.Source[14].MultiResBitmap[0].Bitmap);
      end;
    mtPointRad:
       begin
        if AMarker.IsOwner then
          AMarker.Marker.Bitmap.Assign(ImageList.Source[1].MultiResBitmap[0].Bitmap)
        else
          AMarker.Marker.Bitmap.Assign(ImageList.Source[15].MultiResBitmap[0].Bitmap);
      end;
    mtPointAnomaly:
       begin
        if AMarker.IsOwner then
          AMarker.Marker.Bitmap.Assign(ImageList.Source[2].MultiResBitmap[0].Bitmap)
        else
          AMarker.Marker.Bitmap.Assign(ImageList.Source[16].MultiResBitmap[0].Bitmap);
      end;
    mtPointBag:
       begin
        if AMarker.IsOwner then
          AMarker.Marker.Bitmap.Assign(ImageList.Source[3].MultiResBitmap[0].Bitmap)
        else
          AMarker.Marker.Bitmap.Assign(ImageList.Source[17].MultiResBitmap[0].Bitmap);
      end;
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
        AMarker.Marker.Visible := False;
      end;
    mtAnomaly:
      begin
        AMarker.Marker.Width := FOriginalMapWidth / FMapRealWidth * FAnomalyList[AMarker.Index].Radius * 2 * FCurrentScale;
        AMarker.Marker.Tag := Round(AMarker.Marker.Width / FCurrentScale);
        AMarker.Marker.Height := AMarker.Marker.Width;
        AMarker.Marker.Visible := False;
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

  SetMarker(AMarker.Marker, AMarker.Coords.Latitude, AMarker.Coords.Longitude);

  if AMarker.MarkerType = mtIssue then
    FMarkerIssue.Add(AMarker)
  else
    FMarkerList.Add(AMarker);
end;

procedure TFrameMap.ResetLocationMarkers;

begin

TTask.Run(
procedure
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

  faMarkerX.Duration := 0; // Для скрытия плавного движения при зуме
  faMarkerY.Duration := 0;

  SetLocationMarker(FLocation.Latitude, FLocation.Longitude);

  faMarkerX.Duration := 0.2;
  faMarkerY.Duration := 0.2;

  if MarkersPanel.Visible then
    SetMarker(MarkersPanel, FCoords.Latitude, FCoords.Longitude);
end);
  
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
    end;
  end;
end;

procedure TFrameMap.timerCheckCriticalTimer(Sender: TObject);
var
  I: integer;
  vIssue: TIssueData;
begin
  if Assigned(FCritical) then
  begin
    if Not FIsCriticalStart then
    begin

      for I := 0 to FCritical.Count - 1 do
        if FormatDateTime('hh:nn', Time()) = FormatDateTime('hh:nn', FCritical[I].TimeStart) then
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
      if FormatDateTime('hh:nn', Time()) = FormatDateTime('hh:nn', FCurrentCritical.TimeStop) then
      begin
        FIsCriticalStart := False;
        timerCritical.Enabled := false;
        MediaPlayerStartCritical.Stop;
        MediaPlayerStopCritical.CurrentTime := 0;
        MediaPlayerStopCritical.Play;

        for I := FIssueList.Count - 1 downto 0 do
          if FIssueList[I].BlockDetail = 'critical' then
            FIssueList.Delete(I);

        for I := FIssueList.Count - 1 downto 0 do
        begin
          vIssue := FIssueList[I];
          vIssue.Visible := true;
          FIssueList[I] := vIssue;
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

procedure TFrameMap.timerScanAnomaliesNextToTimer(Sender: TObject);
begin
  if Not Person.IsDead then
    ScanAnomaliesNextTo;
end;

procedure TFrameMap.TimerSensorTimer(Sender: TObject);
begin

   {$IFDEF ANDROID}
      LocationServiceChanged;
   {$ENDIF}

    if Assigned(Person) then
      if Person.Health < 100 then
        ScanBaseSafeDead;

   if Not Person.IsDead then
    begin
      ScanAnomalies;
      ScanIssuies;
    end;
end;

procedure TFrameMap.TimerSystemTimer(Sender: TObject);
begin
{$IF Defined(ANDROID)}
  BatteryProgress.Width := BatteryPercent() / 100 * BatteryProgress.Tag;
{$ENDIF}
  labTime.Text := FormatDateTime('hh:nn:ss', Time());
end;

procedure TFrameMap.btnCloseQRClick(Sender: TObject);
begin
  laySendQR.Visible := false;
end;

procedure TFrameMap.btnDeleteNoClick(Sender: TObject);
begin
  BtnClickMedia;
  gplDeleteYesNo.Visible := False;
  gplMenuMarker.Visible := true;
end;

procedure TFrameMap.btnDeleteYesClick(Sender: TObject);
var
  AMarker: TImage;
  vQuery: TFDQuery;
begin
  BtnClickMedia;
  AMarker := (Sender as TSpeedButton).TagObject as TImage;

  ExeExec(Format('delete from markers where lat = %s and lon= %s;',[StringReplace(FMarkerList[GetNumberMarker(AMarker)].Coords.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FMarkerList[GetNumberMarker(AMarker)].Coords.Longitude.ToString, ',', '.', [rfReplaceAll])]), exExecute, vQuery);
  AMarker.Visible := False;
  FMarkerList.Delete(GetNumberMarker(AMarker));
  gplDeleteYesNo.Visible := False;
  gplMenuMarker.Visible := true;
end;

procedure TFrameMap.ZoomToPoint(APoint: TPointF; AScale: Double);
begin
  SetZoom(AScale, APoint.X, APoint.Y);
end;

procedure TFrameMap.UpdateZoomControls;
begin
  // Обновляем состояние кнопок
  btnZoomIn.Enabled := FCurrentScale < FMaxScale;
  btnZoomOut.Enabled := FCurrentScale > FMinScale;
end;

end.
