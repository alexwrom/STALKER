unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
  Generics.Collections, DelphiZXingQRCode, FMX.Graphics, System.UITypes, System.Types, FMX.Layouts, Math;

const
  cCriticalColor = $32F92F2F;
  cNormalColor = $86C98826;
  cFullColor = $6422FC1A;

  cWorseColor = $FFA31010;
  cEgualColor = $FFC98826;
  cBetterColor = $FF0D8409;

type
  TMarkerType = (mtPoint, mtRad, mtAnomaly, mtBag, mtIssue, mtBase, mtSafe);
  TAnomalyType = (atElectro, atFire, atPhisic, atRadiation, atChimishe, atPSI);
  TBagType = (btMedical, btArmor, btWeapon, btArt, btDetector);
  TSendType = (stSell, stIssue, stAnswerSell, stCancelSell, stUpdateData, stUserExists, stLoadArmor);

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
    LoadData: string;
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
    FUserId: integer;
    FGroupId: integer;
    FCash: Extended;
    FIsClassicBag: boolean;
    FCountContener: integer;
    FArmorId: integer;
    FWeaponId: integer;
    FUserName: string;
    procedure SetHealth(const Value: double);
    procedure SetHealthArmor(AValue: double);
    procedure SetHealthWeapon(AValue: double);
    procedure SetCash(const Value: Extended);
    procedure SetIsClassicBag(const Value: boolean);
    procedure SetGroupId(const Value: integer);

  public
    constructor Create;
    property UserId: integer read FUserId write FUserId;
    property UserName: string read FUserName write FUserName;
    property GroupId: integer read FGroupId write SetGroupId;
    property Health: double read FHealth write SetHealth;
    property ArmorId: integer read FArmorId write FArmorId;
    property ArmorHealth: double read FArmorHealth write FArmorHealth;
    property WeaponId: integer read FWeaponId write FWeaponId;
    property WeaponHealth: double read FWeaponHealth write FWeaponHealth;
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

var
  Person: TPerson;
  FLocation: TLocationCoord2D;
  FArtefactsList: TList<TArtefactData>;
  FIssueList: TList<TIssueData>;
  FPlacesList: TList<TPlaceData>;
  FBagList: TList<TBagData>;
  FIsDead: boolean;
  FIsMerchantZone: boolean;

implementation

uses uMainForm;
{ TPerson }

constructor TPerson.Create;
begin

end;

procedure TPerson.SetHealthArmor(AValue: double);
begin
  MainForm.FFramePercs.ArmorHealthProgress.Width := AValue * MainForm.FFramePercs.ArmorHealthProgress.Tag / 100;

  if AValue < 33 then
    MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cCriticalColor
  else if AValue < 66 then
    MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cNormalColor
  else
    MainForm.FFramePercs.ArmorHealthProgress.Fill.Color := cFullColor;
end;

procedure TPerson.SetHealthWeapon(AValue: double);
begin
  MainForm.FFramePercs.WeaponHealthProgress.Width := AValue * MainForm.FFramePercs.WeaponHealthProgress.Tag / 100;

  if AValue < 33 then
    MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cCriticalColor
  else if AValue < 66 then
    MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cNormalColor
  else
    MainForm.FFramePercs.WeaponHealthProgress.Fill.Color := cFullColor;
end;

procedure TPerson.SetIsClassicBag(const Value: boolean);
var
  vQuery: TFDQuery;
begin
  FIsClassicBag := Value;

  if Person.UserId <> -1 then
    ExeExec('update users set is_classic_bag = ' + FIsClassicBag.ToString + ';', exExecute, vQuery);
end;

procedure CancelingAllIssuies;
var
  vQuery: TFDQuery;
begin
  ExeExec('update issuies set status_id = 2 where status_id = 0);', exExecute, vQuery);
  ExeExec('update issuies_block set status_id = 2 where status_id = 0);', exExecute, vQuery);

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

  ExeExec(Format('update users set cash = %d where user_id = %d;', [Round(FCash), Person.UserId]), exExecute, FDQuery);
end;

procedure TPerson.SetGroupId(const Value: integer);
var
  FDQuery: TFDQuery;
begin
  FGroupId := Value;
  ExeExec('select skin from groups wjere group_id = ' + Value.ToString, exActive, FDQuery);
  try
    if FDQuery.RecordCount = 1 then
    begin
      MainForm.FFramePercs.recSkin1.Fill.Bitmap.Bitmap.Assign(FDQuery.FieldByName('skin'));
      MainForm.FFramePercs.recSkin2.Fill.Bitmap.Bitmap.Assign(FDQuery.FieldByName('skin'));
      MainForm.FFramePercs.recSkin3.Fill.Bitmap.Bitmap.Assign(FDQuery.FieldByName('skin'));
      MainForm.FFramePercs.recSkin3.Fill.Kind := TBrushKind.Bitmap;
    end
    else
    begin
      MainForm.FFramePercs.recSkin3.Fill.Color := $FF111611;
      MainForm.FFramePercs.recSkin3.Fill.Kind := TBrushKind.Solid;
    end;
  finally
    FreeQueryAndConn(FDQuery);
  end;
end;

procedure TPerson.SetHealth(const Value: double);
var
  vQuery: TFDQuery;
  vDiff: double;
begin
  if (FHealth <> Value) then
  begin
    if NOT MainForm.animBlood.Running then
      MainForm.igeDeadGlow.Enabled := false;

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

      ExeExec(Format('update users set health = %s, armor_health = %s, weapon_health = %s where user_id = %d;', [StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]), StringReplace(FArmorHealth.ToString, ',', '.', [rfReplaceAll]),
        StringReplace(FWeaponHealth.ToString, ',', '.', [rfReplaceAll]), Person.UserId]), exExecute, vQuery);

      SetHealthArmor(FArmorHealth);
      SetHealthWeapon(FWeaponHealth);
    end
    else
      ExeExec('update users set health = ' + StringReplace(FHealth.ToString, ',', '.', [rfReplaceAll]) + ' where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);

    SetHealthProgress(MainForm.HealthProgress, FHealth);

    if Assigned(MainForm.FFramePercs) then
    begin
      SetHealthProgress(MainForm.FFramePercs.HealthProgress, FHealth);
      MainForm.FFramePercs.ReloadPercs;
    end;

    if FHealth = 0 then
    begin
      CancelingAllIssuies;
      FIsDead := true;
      MainForm.animBlood.Stop;
      MainForm.recSelect.Parent := MainForm.imgBtnMap;
      MainForm.igeDeadGlow.Opacity := 1;
      MainForm.igeDeadGlow.Enabled := true;

      MainForm.layMenu.Enabled := false;
      MainForm.TabControl.ActiveTab := MainForm.TabMap;
      MainForm.StopDetector;
      MainForm.FFrameMap.MediaPlayerRad.Stop;
      MainForm.FFrameMap.MediaPlayerAnomaly.Stop;
    end;
  end
  else
  begin
    SetHealthProgress(MainForm.HealthProgress, FHealth);

    if Assigned(MainForm.FFramePercs) then
      SetHealthProgress(MainForm.FFramePercs.HealthProgress, FHealth);
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

function ExeExec(Str: UnicodeString; Typ: TExecType; var AQuery: TFDQuery): boolean;
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
  MainForm.FFrameDetector.timerScannerArtefacts.Enabled := true;
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

procedure GenerateQRCode(const AText: string; AImage: TImage);
var
  QRCode: TDelphiZXingQRCode;
  Bitmap: TBitmap;
  Row: integer;
  Col: integer;
  vScale: single;
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
  MainForm.igeDeadGlow.Enabled := true;
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
  MainForm.imgBtnQRScanner.Opacity := IfThen(AValue, 1, 0.5);
end;

procedure StartApp;
begin
  MainForm.StartApp;
end;

end.
