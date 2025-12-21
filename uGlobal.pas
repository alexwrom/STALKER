unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
  Generics.Collections, DelphiZXingQRCode, FMX.Graphics;

const
  cCriticalColor = $32F92F2F;
  cNormalColor = $32F9AC2F;
  cFullColor = $6422FC1A;

type
  TMarkerType = (mtPoint, mtRad, mtAnomaly, mtBag, mtIssue, mtBase, mtSafe);
  TAnomalyType = (atElectro, atFire, atPhisic, atRadiation, atChimishe, atPSI);

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

  TPlaceData = record
    Name: string;
    Coords: TLocationCoord2D;
    MarkerType: TMarkerType;
    Radius: integer;
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

  TPerc = record
    PhisicArmor: double;
    RadiationArmor: double;
    ElectroArmor: double;
    FireArmor: double;
    PsiArmor: double;
    ChimisheArmor: double;
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
    FUserId: integer;
    procedure SetHealth(const Value: double);
    procedure SetHealthArmor(AValue: double);
    procedure SetHealthWeapon(AValue: double);

  public
    constructor Create;
    property UserId: integer read FUserId write FUserId;
    property Health: double read FHealth write SetHealth;
    property ArmorHealth: double read FArmorHealth write FArmorHealth;
    property WeaponHealth: double read FWeaponHealth write FWeaponHealth;
    property PsiArmor: double read FPsiArmor write FPsiArmor;
    property ElectroArmor: double read FElectroArmor write FElectroArmor;
    property FireArmor: double read FFireArmor write FFireArmor;
    property PhisicArmor: double read FPhisicArmor write FPhisicArmor;
    property ChimisheArmor: double read FChimisheArmor write FChimisheArmor;
    property RadiationArmor: double read FRadiationArmor write FRadiationArmor;

    property Detector: TDetector read FDetector write FDetector;
  end;

function GetUserAppPath: string;
procedure GoToDetector;
function ExeExec(Str: string; Typ: TExecType; var AQuery: TFDQuery): boolean;
function CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: double): double;
procedure FreeQueryAndConn(var AQuery: TFDQuery);
procedure SetHealthProgress(AHealthProgress: TRectangle; AValue: double);
procedure GenerateQRCode(const AText: string; ASize: integer; AImage: TImage);
procedure ReloadIssies;

var
  Person: TPerson;
  FLocation: TLocationCoord2D;
  FArtefactsList: TList<TArtefactData>;
  FIssueList: TList<TIssueData>;
  FPlacesList: TList<TPlaceData>;
  FIsDead: boolean;

implementation

uses uMainForm;
{ TPerson }

constructor TPerson.Create;
begin

end;

procedure TPerson.SetHealthArmor(AValue: double);
begin
  MainForm.FPercsFrame.ArmorHealthProgress.Width := AValue * MainForm.FPercsFrame.ArmorHealthProgress.Tag / 100;

  if AValue < 33 then
    MainForm.FPercsFrame.ArmorHealthProgress.Fill.Color := cCriticalColor
  else if AValue < 66 then
    MainForm.FPercsFrame.ArmorHealthProgress.Fill.Color := cNormalColor
  else
    MainForm.FPercsFrame.ArmorHealthProgress.Fill.Color := cFullColor;
end;

procedure TPerson.SetHealthWeapon(AValue: double);
begin
  MainForm.FPercsFrame.WeaponHealthProgress.Width := AValue * MainForm.FPercsFrame.WeaponHealthProgress.Tag / 100;

  if AValue < 33 then
    MainForm.FPercsFrame.WeaponHealthProgress.Fill.Color := cCriticalColor
  else if AValue < 66 then
    MainForm.FPercsFrame.WeaponHealthProgress.Fill.Color := cNormalColor
  else
    MainForm.FPercsFrame.WeaponHealthProgress.Fill.Color := cFullColor;
end;

procedure TPerson.SetHealth(const Value: double);
var
  vQuery: TFDQuery;
  vDiff: double;
begin
  if (FHealth <> Value) then
  begin
    MainForm.FMapFrame.igeDeadGlow.Enabled := false;
    MainForm.layMenu.Enabled := true;

    if MainForm.TabControl.ActiveTab <> MainForm.TabPercs then
      MainForm.imgPersonHealth.Visible := true;

    FIsDead := false;

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
      vDiff := FHealth - Value;
      FHealth := Value;
    end;

    if vDiff > 0 then
    begin
      FArmorHealth := FArmorHealth - vDiff * 0.2;

      if FArmorHealth < 0 then
        FArmorHealth := 0;

      FWeaponHealth := FWeaponHealth - vDiff * 0.5;

      if FWeaponHealth < 0 then
        FWeaponHealth := 0;

      ExeExec(Format('update users set health = %s, armor_health = %s, weapon_health = %s where user_id = %d;', [StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]),
        StringReplace(FArmorHealth.ToString, ',', '.', [rfReplaceAll]), StringReplace(FWeaponHealth.ToString, ',', '.', [rfReplaceAll]), Person.UserId]), exExecute, vQuery);

      SetHealthArmor(FArmorHealth);
      SetHealthWeapon(FWeaponHealth);
    end
    else
      ExeExec('update users set health = ' + StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]) + ' where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);

    SetHealthProgress(MainForm.HealthProgress, FHealth);

    if Assigned(MainForm.FPercsFrame) then
    begin
      SetHealthProgress(MainForm.FPercsFrame.HealthProgress, FHealth);
      MainForm.FPercsFrame.ReloadPercs;
    end;

    if FHealth = 0 then
    begin
      FIsDead := true;
      MainForm.FMapFrame.igeDeadGlow.Enabled := true;
      MainForm.layMenu.Enabled := false;
      MainForm.TabControl.ActiveTab := MainForm.TabMap;
      MainForm.StopDetector;
    end;
  end
  else
  begin
    SetHealthProgress(MainForm.HealthProgress, FHealth);

    if Assigned(MainForm.FPercsFrame) then
      SetHealthProgress(MainForm.FPercsFrame.HealthProgress, FHealth);
  end;
end;

procedure SetHealthProgress(AHealthProgress: TRectangle; AValue: double);
begin
  if AHealthProgress = MainForm.HealthProgress then
    AHealthProgress.Height := AValue * AHealthProgress.Tag / 100
  else
    AHealthProgress.Width := AValue * AHealthProgress.Tag / 100;

  if AValue < 33 then
    AHealthProgress.Fill.Color := cCriticalColor
  else if AValue < 66 then
    AHealthProgress.Fill.Color := cNormalColor
  else
    AHealthProgress.Fill.Color := cFullColor;
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

function ExeExec(Str: string; Typ: TExecType; var AQuery: TFDQuery): boolean;
var
  FDConn: TFDConnection;
  FilePath: string;
begin
  result := true;
  // 0 - запрос на отображение списка
  // 1 - запрос на выполнение
  FDConn := TFDConnection.Create(nil);
  FDConn.Params.DriverID := 'SQLite';
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
          AQuery.SQL.Append('commit;');
          AQuery.ExecSQL();
          FDConn.Connected := false;
        except
          result := false;
        end;

    end;
  end;
end;

procedure GoToDetector;
begin
  MainForm.TabControl.ActiveTab := MainForm.TabDetector;
  MainForm.FDetectorFrame.timerScannerArtefacts.Enabled := true;
  MainForm.imgPersonHealth.Visible := true;
  MainForm.recSelect.Parent := nil;
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
  AQuery.Connection.Connected := false;
  FreeAndNil(AQuery);
end;

procedure GenerateQRCode(const AText: string; ASize: integer; AImage: TImage);
var
  QRCode: TDelphiZXingQRCode;
  Bitmap: TBitmap;
  Row, Col: integer;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := AText;
    QRCode.Encoding := TQRCodeEncoding.qrAuto;
    QRCode.QuietZone := 4;

    Bitmap := TBitmap.Create(ASize, ASize);
    try
      // Рисуем QR-код...
      AImage.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  finally
    QRCode.Free;
  end;
end;

procedure ReloadIssies;
begin
  MainForm.LoadIsuies;
end;

end.
