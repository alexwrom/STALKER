unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
  Generics.Collections, FMX.Graphics, System.UITypes, System.Types, FMX.Layouts, Math, FMX.Types;

const
  cCriticalColor = $FF890000;
  cNormalColor = $FFC98826;
  cFullColor = $FF067501;

  cWorseColor = $FFA31010;
  cEgualColor = $FFC98826;
  cBetterColor = $FF0D8409;

type
  TMarkerType = (mtPoint, mtPointRad, mtPointAnomaly, mtPointBag, mtIssue, mtBase, mtSafe, mtArt, mtAnomaly, mtRadiation);
  TAnomalyType = (atElectro, atFire, atPhisic, atRadiation, atChimishe, atPSI);
  TBagType = (btMedical, btArmor, btWeapon, btArt, btDetector);
  TSendType = (stSell, stIssue, stAnswerSell, stCancelSell, stUpdateData, stUserExists, stLoadArmor, stCancel);

  TMarkerData = record
    Marker: TImage;
    Coords: TLocationCoord2D;
    LabelText: string;
    LabelDetail: string;
    MarkerType: TMarkerType;
    Arrow: TImage;
    Tag: integer;
    ID: integer;
  end;

  TColumn = record
    Name: string;
    TypeCol: string;
  end;

  TAnomalyData = record
    Coords: TLocationCoord2D;
    Radius: integer;
    Power: integer;
    AnomalyType: TAnomalyType;
    Tag: integer;
    ID: integer;
  end;

  TPlaceData = record
    Name: string;
    Coords: TLocationCoord2D;
    MarkerType: TMarkerType;
    Radius: integer;
    ID: integer;
  end;

  TArtefactData = record
    Icon: TBitmap;
    Name: string;
    Coords: TLocationCoord2D;
    Level: integer;
    ID: integer;
  end;

  TWiFiNetwork = record
    SSID: string;
    BSSID: string;
    Distance: double;
  end;

  TExecType = (exActive, exExecute);

function ExeExec(Str: UnicodeString; Typ: TExecType; var AQuery: TFDQuery): boolean;
function CalculateFastDistance(const Lat1, Lon1, Lat2, Lon2: double): double;
procedure FreeQueryAndConn(var AQuery: TFDQuery);
function GetUserAppPath: string;
procedure StartApp;
procedure EditMarker(AMarker: TMarkerData);
procedure AddArt;
procedure AddAnomaly;
procedure AddPlace;

var
  FCoords: TLocationCoord2D;
  FLocation, FOldLocation: TLocationCoord2D;
  FArtefactsList: TList<TArtefactData>;
  FPlacesList: TList<TPlaceData>;
  FMarkerList: TList<TMarkerData>;
  FAnomalyList: TList<TAnomalyData>;
  FIsDead: boolean;
  FIsMerchantZone: boolean;

implementation

uses uMainForm;
{ TPerson }

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

procedure StartApp;
begin
  MainForm.StartApp;
end;

procedure EditMarker(AMarker: TMarkerData);
begin
  case AMarker.MarkerType of
    mtArt:
      begin
        MainForm.layAddArt.Visible := true;
        MainForm.recSelectArt.Parent := MainForm.FControlListArt.SelectTo(AMarker.Tag).rcBackground;
        MainForm.recSelectArt.Align := TAlignLayout.Contents;
        MainForm.recSelectArt.BringToFront;
        MainForm.recSelectArt.Visible := true;

        MainForm.btnAddArtSave.Tag := MainForm.FControlListArt.SelectTo(AMarker.Tag).Tag;
        MainForm.btnAddArtSave.Enabled := true;
        MainForm.layAddArt.Tag := AMarker.ID; // layAddArt.Tag - art_to_map_id
      end;
    mtAnomaly, mtRadiation:
      begin
        MainForm.layAddAnomaly.Visible := true;
        MainForm.layAddAnomaly.Tag := AMarker.ID; // layAddAnomaly.Tag - anomaly_id

        case FAnomalyList[AMarker.Tag].AnomalyType of
          atElectro:
            MainForm.cbAnomalyType.ItemIndex := 1;
          atFire:
            MainForm.cbAnomalyType.ItemIndex := 3;
          atPhisic:
            MainForm.cbAnomalyType.ItemIndex := 2;
          atRadiation:
            MainForm.cbAnomalyType.ItemIndex := 0;
          atChimishe:
            MainForm.cbAnomalyType.ItemIndex := 4;
          atPSI:
            MainForm.cbAnomalyType.ItemIndex := 5;
        end;

        MainForm.sbRadius.Value := FAnomalyList[AMarker.Tag].Radius;
        MainForm.sbPower.Value := FAnomalyList[AMarker.Tag].Power;
      end;
    mtBase, mtSafe:
      begin
        MainForm.layAddPlace.Visible := true;
        MainForm.layAddPlace.Tag := AMarker.ID; // layAddPlace.Tag - place_id

        case FPlacesList[AMarker.Tag].MarkerType of
          mtBase:
            MainForm.cbPlaceType.ItemIndex := 0;
          mtSafe:
            MainForm.cbPlaceType.ItemIndex := 1;
        end;

        MainForm.sbRadiusPlace.Value := FPlacesList[AMarker.Tag].Radius;
        MainForm.ePlaceName.Text := FPlacesList[AMarker.Tag].Name;
      end;
  end;
end;

procedure AddArt;
begin
  MainForm.layAddArt.Visible := true;
  MainForm.recSelectArt.Parent := nil;
end;

procedure AddAnomaly;
begin
  MainForm.layAddAnomaly.Visible := true;
end;

procedure AddPlace;
begin
  MainForm.layAddPlace.Visible := true;
  MainForm.ePlaceName.Text := '';
end;

end.
