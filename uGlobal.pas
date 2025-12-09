unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils;

const
  cCriticalColor = $32F92F2F;
  cNormalColor = $32F9AC2F;
  cFullColor = $6422FC1A;

type
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
    procedure SetHealth(const Value: double);

  public
    constructor Create;
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

var
  Person: TPerson;

implementation

uses uMainForm;
{ TPerson }

constructor TPerson.Create;
begin
  Health := 100;
end;

procedure TPerson.SetHealth(const Value: double);
begin
  FHealth := Value;
  MainForm.HealthProgress.Width := Value * MainForm.HealthProgress.Tag / 100;

  if Value < 33 then
    MainForm.HealthProgress.Fill.Color := cCriticalColor
  else if Value < 66 then
    MainForm.HealthProgress.Fill.Color := cNormalColor
  else
    MainForm.HealthProgress.Fill.Color := cFullColor;
end;

function GetUserAppPath: string;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  result := TPath.GetDocumentsPath;
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
  FDConn.Params.Database := TPath.Combine(GetUserAppPath, 'base.db');
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
  MainForm.btnToDetectorClick(nil);
end;

end.
