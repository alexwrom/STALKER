unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os,
  Androidapi.JNI.Net,  Androidapi.JNI.Media,
{$ENDIF}
  Generics.Collections, DelphiZXingQRCode, FMX.Graphics, System.UITypes, System.Types, FMX.Layouts, Math, FMX.Platform,
  DateUtils, System.TimeSpan, FMX.Surfaces;

const
  cCriticalColor = $FF890000;
  cNormalColor = $FFC98826;
  cFullColor = $FF067501;

  cWorseColor = $FFA31010;
  cEgualColor = $FFC98826;
  cBetterColor = $FF0D8409;

type
  TMarkerType = (mtPoint, mtPointRad, mtPointAnomaly, mtPointBag, mtIssue, mtBase, mtSafe, mtRadiation, mtAnomaly, mtArtefact);
  TAnomalyType = (atElectro, atFire, atPhisic, atRadiation, atChimishe, atPSI);
  TBagType = (btMedical, btArmor, btWeapon, btArt, btDetector);
  TSendType = (stSell, stCancelSell, stMedic, stTehnic);
  TKillType = (ktWeapon, ktAnomaly, ktPSI, ktCritical, ktRadiation, ktStopZombi, ktLive);

  TColumn = record
    Name: string;
    TypeCol: string;
  end;

  TMarkerData = record
    Marker: TImage;
    Coords: TLocationCoord2D;
    LabelText: string;
    LabelDetail: string;
    MarkerType: TMarkerType;
    Arrow: TImage;
    Index: integer;
    Radius: integer;
    IsOwner: boolean;
  end;

  TCritical = record
    Name: string;
    TimeStart: TDateTime;
    TimeStop: TDateTime;
    MinuteBeforeStartDamage: integer; // Минуты от начала выброса до начала урона
  end;

  TAnomalyData = record
    Coords: TLocationCoord2D;
    Radius: integer;
    Power: integer;
    AnomalyType: TAnomalyType;
  end;

  TPerc = record
    ID: integer;
    PhisicArmor: double;
    RadiationArmor: double;
    ElectroArmor: double;
    FireArmor: double;
    PsiArmor: double;
    ChimisheArmor: double;
  end;

  TBagData = record
    Icon: TBitmap;
    BagType: TBagType;
    TableName: string;
    RowID: integer;
    Count: integer;
    Health: double;
    HealthRestore: double;
    CountSlots: integer;
    Percs: TPerc;
    Cost: Extended;
  end;

  TPlaceData = record
    Name: string;
    Coords: TLocationCoord2D;
    MarkerType: TMarkerType;
    Radius: integer;
  end;

  TNotificationData = record
    Name: string;
    ID: integer;
    MessageText: string;
    IsOpen: boolean;
    Data: string;
  end;

  TArtefactData = record
    Coords: TLocationCoord2D;
    Level: integer;
  end;

  TIssueData = record
    ID: integer;
    Coords: TLocationCoord2D;
    Name: string;
    Detail: string;
    Cost: integer;
    PrevID: integer;
    RadiusIN: integer;
    RadiusOUT: integer;
    CompleteAfterOUT: boolean; // true - задача выполнена при Покидании точки
    CompleteAfterIN: boolean; // true - задача выполнена при достижении точки
    StatusID: integer;
    BlockStatusID: integer;
    BlockDetail: string;
    Visible: boolean;
  end;

  TWiFiNetwork = record
    SSID: string;
    BSSID: string;
    Distance: double;
  end;

  TExecType = (exActive, exExecute);

  TDetector = record
    Radius: integer;
    Level: integer;
  end;

  TPerson = class
  private
    FHealth: double;
    FWeaponHealth: double;
    FArmorHealth: double;
    FPhisicArmor: double;
    FRadiationArmor: double;
    FElectroArmor: double;
    FFireArmor: double;
    FPsiArmor: double;
    FChimisheArmor: double;
    FDetector: TDetector;
    FGroupId: integer;
    FCash: Extended;
    FIsClassicBag: boolean;
    FCountContener: integer;
    FArmorId: integer;
    FWeaponId: integer;
    FUserName: string;
    FUserId: integer;
    FLevelMedic: integer;
    FLevelTehnic: integer;
    FWeaponLevel: integer;
    FIsDead: boolean;
    procedure SetHealth(const Value: double);
    
    procedure SetCash(const Value: Extended);
    procedure SetIsClassicBag(const Value: boolean);
    procedure SetGroupId(const Value: integer);
    procedure SetUserId(const Value: integer);
    procedure SetHealthArmor(AValue: double);
    procedure SetHealthWeapon(AValue: double);
    procedure SetIsDead(const Value: boolean);
  public
    constructor Create;
    property UserName: string read FUserName write FUserName;
    property GroupId: integer read FGroupId write SetGroupId;
    property UserId: integer read FUserId write SetUserId;
    property Health: double read FHealth write SetHealth;
    property ArmorId: integer read FArmorId write FArmorId;
    property ArmorHealth: double read FArmorHealth write SetHealthArmor;
    property WeaponId: integer read FWeaponId write FWeaponId;
    property WeaponHealth: double read FWeaponHealth write SetHealthWeapon;
    property WeaponLevel: integer read FWeaponLevel write FWeaponLevel;
    property PsiArmor: double read FPsiArmor write FPsiArmor;
    property ElectroArmor: double read FElectroArmor write FElectroArmor;
    property FireArmor: double read FFireArmor write FFireArmor;
    property PhisicArmor: double read FPhisicArmor write FPhisicArmor;
    property ChimisheArmor: double read FChimisheArmor write FChimisheArmor;
    property RadiationArmor: double read FRadiationArmor write FRadiationArmor;
    property Cash: Extended read FCash write SetCash;
    property CountContener: integer read FCountContener write FCountContener;
    property IsClassicBag: boolean read FIsClassicBag write SetIsClassicBag;
    property Detector: TDetector read FDetector write FDetector;
    property LevelMedic: integer read FLevelMedic write FLevelMedic;
    property LevelTehnic: integer read FLevelTehnic write FLevelTehnic;
    property IsDead: boolean read FIsDead write SetIsDead;
    
  end;

function GetUserAppPath: string;
procedure GoToDetector;
function ExeExec(Str: UnicodeString; Typ: TExecType; var AQuery: TFDQuery): boolean;
function CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: double): double;
procedure FreeQueryAndConn(var AQuery: TFDQuery);
procedure SetHealthProgress(AHealthProgress: TRectangle; AValue: double);
procedure GenerateQRCode(const AText: string; AImage: TImage);
procedure ReloadIssuies;
procedure StartDamageGlow;
procedure StopDamageGlow;
procedure ReloadBag;
procedure ReloadPercs;
function IsFullBelt: boolean;
procedure ActiveScaner(AValue: boolean);
procedure StartApp;
procedure SetDetector(ID: integer);
function GetOrientation: Single;
procedure CreateFrameLogin;
procedure SetMediaVolume(VolumePercent: Integer);
{$IF Defined(ANDROID)}
procedure Vibration(AValue: integer);
{$ENDIF}
procedure BtnClickMedia;
procedure NewMarkerToMap(ACoords: TLocationCoord2D; AText: String; AMarkerType: TMarkerType; AIsOwner: boolean);
procedure SetNotifications;
procedure ReloadNotificationData;
procedure ReloadMarkers;
procedure OpenMap;
procedure OpenPercs;

var
  Person: TPerson;
  FLocation, FOldLocation: TLocationCoord2D;
  FArtefactsList: TList<TArtefactData>;
  FAnomalyList: TList<TAnomalyData>;
  FIssueList: TList<TIssueData>;
  FPlacesList: TList<TPlaceData>;
  FBagList: TList<TBagData>;
  FIsMerchantZone: boolean;
  FArmorPerc: TPerc;
  FCritical: TList<TCritical>;
  FIsCriticalStart: boolean;
  FSecondBeforeStartDamage: integer;
  FCurrentCritical: TCritical;

  FOriginalMapWidth: double;
  FOriginalMapHeight: double;
  FMapRealWidth: integer;
  FTopLeftLat: double;
  FTopLeftLon: double;
  FBottomRightLat: double;
  FBottomRightLon: double;

  FKillType: TKillType;
  FIsSendMarkers: boolean;
  FVolume: integer;
  FSurfaceFog: TBitmapSurface;

implementation

uses uMainForm;
{ TPerson }

constructor TPerson.Create;
begin

end;

procedure TPerson.SetHealthArmor(AValue: double);
begin
  FArmorHealth := AValue;

  if Assigned(MainForm.FFramePercs) then
  begin
    MainForm.FFramePercs.ArmorHealthProgress.Width := AValue * MainForm.FFramePercs.ArmorHealthProgress.Tag / 100;

    if AValue < 40 then
      MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cCriticalColor
    else if AValue < 80 then
      MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cNormalColor
    else
      MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cFullColor;
  end;
end;

procedure TPerson.SetHealthWeapon(AValue: double);
begin
  FWeaponHealth := AValue;

  if Assigned(MainForm.FFramePercs) then
  begin
    MainForm.FFramePercs.WeaponHealthProgress.Width := AValue * MainForm.FFramePercs.WeaponHealthProgress.Tag / 100;

    if AValue < 40 then
      MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cCriticalColor
    else if AValue < 80 then
      MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cNormalColor
    else
      MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cFullColor;
  end;
end;

procedure TPerson.SetIsClassicBag(const Value: boolean);
var
  vQuery: TFDQuery;
begin
  FIsClassicBag := Value;

  if Assigned(Person) then
    ExeExec('update users set is_classic_bag = ' + FIsClassicBag.ToString + ';', exExecute, vQuery);
end;

procedure TPerson.SetIsDead(const Value: boolean);
begin
  FIsDead := Value;
  MainForm.FFrameMap.layBtnKill.Visible := not FIsDead;
  MainForm.ImgBtnPercs.Enabled := not FIsDead;
  MainForm.imgBtnBag.Enabled := not FIsDead;
  MainForm.imgBtnIssuies.Enabled := not FIsDead;
  MainForm.FFrameMap.btnSendMarkers.Enabled := not FIsDead;
end;

procedure TPerson.SetUserId(const Value: integer);
begin
  FUserId := Value;
end;

procedure CancelingAllIssuies;
var
  vQuery: TFDQuery;
begin
  ExeExec('update issuies set status_id = 2 where status_id = 0;', exExecute, vQuery);
  ExeExec('update issuies_block set status_id = 2 where status_id = 0;', exExecute, vQuery);

  ReloadIssuies;
  MainForm.FFrameMap.UpdateIssue;
end;

procedure TPerson.SetCash(const Value: Extended);
var
  FDQuery: TFDQuery;
begin
  FCash := Value;

  if Assigned(MainForm.FFrameBag) then
    MainForm.FFrameBag.labCash.Text := Format('%.0n', [FCash]);

  ExeExec(Format('update users set cash = %d;', [Round(FCash)]), exExecute, FDQuery);
end;

procedure TPerson.SetGroupId(const Value: integer);
var
  FDQuery: TFDQuery;
  IsExistsSkin: boolean;

  procedure SetSkin(ARec: TRectangle);
  begin
    if IsExistsSkin then
    begin
      ARec.Fill.Bitmap.Bitmap.Assign(FDQuery.FieldByName('skin'));
      ARec.Fill.Kind := TBrushKind.Bitmap;
    end
    else
    begin
      ARec.Fill.Color := $FF111611;
      ARec.Fill.Kind := TBrushKind.Solid;
    end;
  end;

begin
  FGroupId := Value;

  ExeExec('select skin from groups where group_id = ' + Value.ToString + ';', exActive, FDQuery);
  try
    IsExistsSkin := FDQuery.RecordCount = 1;

    if Assigned(MainForm.FFramePercs) then
    begin
      SetSkin(MainForm.FFramePercs.recSkin);
      SetSkin(MainForm.FFramePercs.recSkin1);
      SetSkin(MainForm.FFramePercs.recSkin2);
    end;

    if Assigned(MainForm.FFrameDetector) then
      SetSkin(MainForm.FFrameDetector.recSkin);

    if Assigned(MainForm.FFrameQRScanner) then
    begin
      SetSkin(MainForm.FFrameQRScanner.recSkin);
      SetSkin(MainForm.FFrameQRScanner.recSkin1);
    end;

    if Assigned(MainForm.FFrameIssuies) then
    begin
      SetSkin(MainForm.FFrameIssuies.recSkin);
      SetSkin(MainForm.FFrameIssuies.recSkin1);
      SetSkin(MainForm.FFrameIssuies.recSkin2);
    end;

    if Assigned(MainForm.FFrameBag) then
    begin
      SetSkin(MainForm.FFrameBag.recSkin);
      SetSkin(MainForm.FFrameBag.recSkin1);
      SetSkin(MainForm.FFrameBag.recSkin2);
      SetSkin(MainForm.FFrameBag.recSkin3);
      SetSkin(MainForm.FFrameBag.recSkin4);
    end;

    if Assigned(MainForm.FFrameSettings) then
      SetSkin(MainForm.FFrameSettings.recSkin);

    SetSkin(MainForm.recSkin);
    SetSkin(MainForm.recSkin1);
  finally
    FreeQueryAndConn(FDQuery);
  end;
end;

procedure TPerson.SetHealth(const Value: double);
var
  vQuery: TFDQuery;
  vDiff: double;
  AFormatSettings: TFormatSettings;
  vIntKillType: integer;
  vLastActionDateTime: TDateTime;
  Hours: integer;
  Minutes: integer;
  Seconds: integer;
  TotalSeconds: Int64;
  TimeSpan: TTimeSpan;
  vActionTypeID : integer;
begin
  AFormatSettings.DateSeparator := '.';
  AFormatSettings.TimeSeparator := ':';
  AFormatSettings.ShortDateFormat := 'DD.MM.YYYY';
  AFormatSettings.LongTimeFormat := 'hh:nn:ss';

  if NOT MainForm.TimerZombi.Enabled then // Если режим зомби, то нас ничего не лечит
    if (RoundTo(FHealth, -2) <> RoundTo(Value, -2)) then
    begin
      if  Person.IsDead then
        if ((Value > 20) and (RoundTo(FHealth, -2) < RoundTo(Value, -2))) then
        begin
          MainForm.layDeadGlow.Visible := false;
          Person.IsDead := false;
          FKillType := ktLive;
        end
        else
        begin
          MainForm.animBlood.Stop;
          MainForm.layDeadGlow.Opacity := 1;
          MainForm.layDeadGlow.Visible := true;
          Person.IsDead := true;
        end;

      if MainForm.TabControl.ActiveTab <> MainForm.TabPercs then
        MainForm.layPersonHealth.Visible := true;

      if RoundTo(FHealth, -2) = 0 then
        ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [6, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]),
          exExecute, vQuery);

      if Value < 0 then
      begin
        vDiff := FHealth;
        FHealth := 0;
      end
      else if Value > 100 then
      begin
        vDiff := -100;
        FHealth := 100;
      end
      else
      begin
        vDiff := RoundTo(FHealth, -2) - RoundTo(Value, -2);
        FHealth := RoundTo(Value, -2);
      end;

      if vDiff > 0 then
      begin
{$IF Defined(ANDROID)}
        Vibration(500);
{$ENDIF}
        ArmorHealth := ArmorHealth - vDiff * 0.2;

        if ArmorHealth < 0 then
          ArmorHealth := 0;

        WeaponHealth := WeaponHealth - vDiff * 0.5;

        if WeaponHealth < 0 then
          WeaponHealth := 0;

        ExeExec(Format('update users set health = %s, armor_health = %s, weapon_health = %s;', [StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]), StringReplace(ArmorHealth.ToString, ',', '.', [rfReplaceAll]),
          StringReplace(WeaponHealth.ToString, ',', '.', [rfReplaceAll])]), exExecute, vQuery);

      end
      else
        ExeExec('update users set health = ' + StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]) + ';', exExecute, vQuery);

      SetHealthProgress(MainForm.HealthProgress, FHealth);

      if Assigned(MainForm.FFramePercs) then
      begin
        SetHealthProgress(MainForm.FFramePercs.HealthProgress, FHealth);
        MainForm.FFramePercs.ReloadPercs;
      end;
    end
    else
    begin
      SetHealthProgress(MainForm.HealthProgress, FHealth);

      if (RoundTo(FHealth, -2) = 0) and (not Person.IsDead) then
      begin
        SetMediaVolume(100);
        CancelingAllIssuies;
        Person.IsDead := true;
        MainForm.animBlood.Stop;
        MainForm.recSelect.Parent := MainForm.imgBtnMap;
        MainForm.layDeadGlow.Opacity := 1;
        MainForm.layDeadGlow.Visible := true;

        MainForm.TabControl.ActiveTab := MainForm.TabMap;
        MainForm.StopDetector;

        if Assigned(MainForm.FFrameMap) then
        begin
          MainForm.FFrameMap.MediaPlayerDead.CurrentTime := 0;
          MainForm.FFrameMap.MediaPlayerDead.Play;
          MainForm.FFrameMap.MediaPlayerRad.Stop;
          MainForm.FFrameMap.MediaPlayerScanAnomaly.Stop;
          MainForm.FFrameMap.MediaPlayerAnomaly.Stop;
        end;

        if Assigned(MainForm.FFramePercs) then
        begin
          SetHealthProgress(MainForm.FFramePercs.HealthProgress, FHealth);

          ArmorHealth := Ifthen(Random(4) in [1, 2, 3], IfThen(ArmorHealth - 50 < 0, 0, ArmorHealth - 50), ArmorHealth);
          WeaponHealth := Ifthen(Random(4) in [1, 2, 3], 0, WeaponHealth);

          ExeExec(Format('update users set health = %s, armor_health = %s, weapon_health = %s;', [StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]), StringReplace(ArmorHealth.ToString, ',', '.', [rfReplaceAll]),
            StringReplace(WeaponHealth.ToString, ',', '.', [rfReplaceAll])]), exExecute, vQuery);

        end;

        if FKillType = ktLive then
        begin
          ExeExec('select * from last_action_life;', exActive, vQuery);
          try
            if vQuery.RecordCount > 0 then
            begin
              vLastActionDateTime := StrToDateTime(vQuery.FieldByName('action_date_time').AsString, AFormatSettings);
              vActionTypeID := vQuery.FieldByName('action_type_id').AsInteger;

              case vActionTypeID of
                1:
                  FKillType := ktWeapon;
                2:
                  begin
                    FKillType := ktCritical;
                    MainForm.layDeadGlow.Visible := false;

                    TotalSeconds := 1 * 60 * 60 - SecondsBetween(NOW(), vLastActionDateTime);

                    // Создаем TTimeSpan
                    TimeSpan := TTimeSpan.FromSeconds(TotalSeconds);

                    // Получаем компоненты времени
                    Hours := TimeSpan.Hours;
                    Minutes := TimeSpan.Minutes;
                    Seconds := TimeSpan.Seconds;

                    if (Seconds >= 0) then
                      MainForm.labZombTimer.Text := Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);

                    MainForm.layZombTimer.Visible := true;
                    MainForm.TimerZombi.Enabled := true;
                  end;
                3:
                  FKillType := ktAnomaly;
                4:
                  FKillType := ktRadiation;
                5, 9:     //Если вышел с приложения более чем на 1 минуту, то ты зомби на 30 минут
                  if ((vActionTypeID = 9) and (SecondsBetween(NOW(), vLastActionDateTime) > 60)) or (vActionTypeID = 5) then
                    begin
                      FKillType := ktPSI;
                      MainForm.layDeadGlow.Visible := false;
                      TotalSeconds := 30 * 60 - SecondsBetween(NOW(), vLastActionDateTime);

                      // Создаем TTimeSpan
                      TimeSpan := TTimeSpan.FromSeconds(TotalSeconds);

                      // Получаем компоненты времени
                      Hours := TimeSpan.Hours;
                      Minutes := TimeSpan.Minutes;
                      Seconds := TimeSpan.Seconds;

                      if (Seconds >= 0) then
                        MainForm.labZombTimer.Text := Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);

                      MainForm.layZombTimer.Visible := true;
                      MainForm.TimerZombi.Enabled := true;
                  end;
              end;

            end;
          finally
            FreeQueryAndConn(vQuery);
          end;
        end
        else
        begin
          case FKillType of
            ktStopZombi:
              vIntKillType := 7;
            ktWeapon:
              vIntKillType := 1;
            ktAnomaly:
              vIntKillType := 3;
            ktRadiation:
              vIntKillType := 4;
            ktCritical:
              begin
                MainForm.layZombTimer.Visible := true;
                MainForm.labZombTimer.Text := '01:00:00';
                MainForm.TimerZombi.Enabled := true;
                vIntKillType := 2;
              end;
            ktPSI:
              begin
                MainForm.layZombTimer.Visible := true;
                MainForm.labZombTimer.Text := '00:30:00';
                MainForm.TimerZombi.Enabled := true;
                vIntKillType := 5;
              end;
          end;

          ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [vIntKillType, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.',
            [rfReplaceAll])]), exExecute, vQuery);
        end;
      end;

    end;
end;

procedure SetMediaVolume(VolumePercent: Integer);
{$IF Defined(ANDROID)}
var
  AudioManager: JAudioManager;
  MaxVolume, NewVolume: Integer;
  Obj: JObject;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
  // Получаем сервиз AUDIO_SERVICE из контекста Activity
  Obj := SharedActivityContext.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  // Преобразуем полученный объект в интерфейс JAudioManager
  AudioManager := TJAudioManager.Wrap((Obj as ILocalObject).GetObjectID);

  if AudioManager <> nil then
  begin
    // Получаем максимальную громкость для музыкального потока
    MaxVolume := AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);

    // Рассчитываем новое значение (VolumePercent от 0 до 100)
    NewVolume := Round(MaxVolume * (VolumePercent / 100));

    // Устанавливаем новую громкость
    // Третий параметр (flags) обычно 0, но можно передать, например,
    // TJAudioManager.JavaClass.FLAG_SHOW_UI, чтобы показать системный индикатор громкости
    AudioManager.setStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, NewVolume, 0);
  end;
  {$ENDIF}
end;

{$IF Defined(ANDROID)}

procedure Vibration(AValue: integer);
// Процедура вибрации
var
  Vibrator: JVibrator;
begin
  Vibrator := TJVibrator.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.VIBRATOR_SERVICE));

  if Vibrator.hasVibrator() then
    Vibrator.vibrate(AValue);
end;
{$ENDIF}

procedure SetHealthProgress(AHealthProgress: TRectangle; AValue: double);
begin
  if AHealthProgress = MainForm.HealthProgress then
    AHealthProgress.Height := AValue * AHealthProgress.Tag / 100
  else
    AHealthProgress.Width := AValue * AHealthProgress.Tag / 100;

  if AValue <= 40 then
    AHealthProgress.Fill.Color := cCriticalColor
  else if AValue <= 80 then
    AHealthProgress.Fill.Color := cNormalColor
  else
    AHealthProgress.Fill.Color := cFullColor;

  MainForm.imgPersonHealth.BringToFront;
end;

function GetUserAppPath: string;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  result := System.IOUtils.TPath.GetDocumentsPath;
{$ENDIF}
{$IFDEF MSWINDOWS}
  result := ExtractFilePath(paramstr(0));
{$ENDIF}
end;

function ExeExec(Str: UnicodeString; Typ: TExecType; var AQuery: TFDQuery): boolean;
var
  FDConn: TFDConnection;
begin
  result := true;
  // 0 - запрос на отображение списка
  // 1 - запрос на выполнение
  FDConn := TFDConnection.Create(nil);
  FDConn.Params.DriverID := 'SQLite';

  // if Assigned(AQuery) then
  // FreeQueryAndConn(AQuery);

  AQuery := TFDQuery.Create(nil);

  AQuery.Connection := FDConn;
  FDConn.Params.Database := System.IOUtils.TPath.Combine(GetUserAppPath, 'base.db');
  try
    FDConn.Connected := true;
  except
    result := false;
  end;

  if FDConn.Connected then
  begin
    AQuery.Active := false;
    AQuery.SQL.clear;

    case Typ of
      exActive:
        begin
          AQuery.SQL.Add(Str);
          AQuery.Active := true;
        end;

      exExecute:
        try
          AQuery.SQL.Append('BEGIN TRANSACTION;');
          AQuery.SQL.Append(Str);
          // AQuery.SQL.Append('commit;');
          AQuery.ExecSQL();
          FDConn.Commit;
          FreeQueryAndConn(AQuery);
        except
          result := false;
        end;

    end;
  end;
end;

procedure GoToDetector;
begin
  MainForm.TabControl.ActiveTab := MainForm.TabDetector;
  MainForm.FFrameDetector.LoadDetector;
  MainForm.FFrameDetector.timerScannerArtefacts.Enabled := true;
  MainForm.layPersonHealth.Visible := true;
  MainForm.recSelect.Parent := nil;

  MainForm.FFrameDetector.FCurrentScale := FMapRealWidth / (FOriginalMapWidth * Person.Detector.Radius / MainForm.FFrameDetector.layVelesDisplay.Width);
  MainForm.FFrameDetector.MapLayout.Width := FOriginalMapWidth * MainForm.FFrameDetector.FCurrentScale;
  MainForm.FFrameDetector.MapLayout.Height := FOriginalMapHeight * MainForm.FFrameDetector.FCurrentScale;
  MainForm.FFrameDetector.UpdateAnomalies;
  MainForm.FFrameDetector.UpdateArtefacts;
end;

function GetOrientation: Single;
begin
  result := MainForm.FFrameMap.OrientationMarker.RotationAngle;
end;

function CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: double): double;
const
  R = 6371000; // Радиус Земли в метрах
  DegToRad = Pi / 180;
var
  X, Y: double;
begin
  // Приближенный расчет для небольших расстояний
  X := (Lon2 - Lon1) * Cos(DegToRad * (Lat1 + Lat2) / 2);
  Y := (Lat2 - Lat1);

  result := R * Sqrt(X * X + Y * Y) * DegToRad;
end;

procedure FreeQueryAndConn(var AQuery: TFDQuery);
begin
  if AQuery.Active then
    AQuery.Active := false;

  AQuery.Connection.Connected := false;
  FreeAndNil(AQuery);
end;

procedure GenerateQRCode(const AText: string; AImage: TImage);
var
  QRCode: TDelphiZXingQRCode;
  Bitmap: TBitmap;
  Row: integer;
  Col: integer;
  vScale: Single;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := AText;
    QRCode.Encoding := TQRCodeEncoding.qrAuto;
    QRCode.QuietZone := 4;

    vScale := AImage.Height / QRCode.Rows;

    Bitmap := TBitmap.Create();
    try
      Bitmap.SetSize(Round(AImage.Height), Round(AImage.Height));

      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Col := 0 to QRCode.Columns - 1 do
        begin
          if not Bitmap.Canvas.BeginScene then
            Exit;
          try
            Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;

            if (QRCode.IsBlack[Row, Col]) then
              Bitmap.Canvas.Fill.Color := TAlphaColors.Black
            else
              Bitmap.Canvas.Fill.Color := TAlphaColors.White;

            // Рисуем пиксель
            Bitmap.Canvas.FillRect(RectF(Col * vScale, Row * vScale, Col * vScale + vScale, Row * vScale + vScale), // Прямоугольник 1x1
              0, 0, [], 1.0);
          finally
            Bitmap.Canvas.EndScene;
          end;
        end;
      end;
      AImage.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  finally
    QRCode.Free;
  end;
end;

procedure ReloadIssuies;
begin
  MainForm.LoadIsuies;
end;

procedure ReloadBag;
begin
  MainForm.btnToBagClick(nil);
end;

procedure StartDamageGlow;
begin
  MainForm.layDeadGlow.Visible := true;
  MainForm.animBlood.Start;
end;

procedure StopDamageGlow;
begin
  MainForm.animBlood.Stop;
end;

procedure ReloadPercs;
begin
  MainForm.FFramePercs.ReloadPercs;
end;

function IsFullBelt: boolean;
var
  vQuery: TFDQuery;
begin
  ExeExec('select count(1) as cnt from belt;', exActive, vQuery);
  result := vQuery.FieldByName('cnt').AsInteger = Person.CountContener;
  FreeQueryAndConn(vQuery);
end;

procedure ActiveScaner(AValue: boolean);
begin
  MainForm.imgBtnQRScanner.Enabled := AValue;
  MainForm.imgBtnQRScanner.Opacity := Ifthen(AValue, 0.7, 0.4);
end;

procedure StartApp;
begin
  MainForm.StartApp;
end;

procedure CreateFrameLogin;
begin
  MainForm.CreateFrameLogin;
end;

procedure SetDetector(ID: integer);
var
  vQuery: TFDQuery;
begin
  ExeExec('select * from detectors where detector_id = ' + ID.ToString + ';', exActive, vQuery);

  if vQuery.RecordCount > 0 then
    MainForm.FFramePercs.SetDetector(vQuery.FieldByName('detector_id').AsInteger, vQuery.FieldByName('radius').AsInteger, vQuery.FieldByName('level').AsInteger);

  FreeQueryAndConn(vQuery);
end;

procedure BtnClickMedia;
begin
  SetMediaVolume(FVolume);
  MainForm.MediaPlayerMenu.CurrentTime := 0;
  MainForm.MediaPlayerMenu.Play;
{$IF Defined(ANDROID)}
  Vibration(50);
{$ENDIF}
end;

procedure ReloadMarkers;
begin
  MainForm.FFrameMap.LoadMarkers;
end;

procedure NewMarkerToMap(ACoords: TLocationCoord2D; AText: String; AMarkerType: TMarkerType; AIsOwner: boolean);
begin
  MainForm.FFrameMap.NewMarkerToMap(ACoords, AText, AMarkerType, AIsOwner);
end;

procedure SetNotifications;
begin
  MainForm.MediaPlayerNotification.CurrentTime := 0;
  MainForm.MediaPlayerNotification.Play;
  MainForm.animNotification.Enabled := true;
  MainForm.FFrameIssuies.animNotification.Enabled := true;
end;

procedure ReloadNotificationData;
begin
  MainForm.FFrameIssuies.LoadInfoData;
end;

procedure OpenMap;
begin
  MainForm.btnToMapClick(nil);
end;

procedure OpenPercs;
begin
  MainForm.btnToPercsClick(nil);
end;

end.
