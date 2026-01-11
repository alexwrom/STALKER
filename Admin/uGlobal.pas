unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
  Generics.Collections, FMX.Graphics, System.UITypes, System.Types, FMX.Layouts, Math, DelphiZXingQRCode, WinSock;

type
  TExecType = (exActive, exExecute);
  TSendType = (stSell, stIssue, stAnswerSell, stCancelSell, stUpdateData, stUserExists);

  TColumn = record
    Name: string;
    TypeCol: string;
  end;

  TPerson = class
  private
    FUserId: integer;
    FGroupId: integer;
    FUserName: string;

  public
    property UserId: integer read FUserId write FUserId;
    property UserName: string read FUserName write FUserName;
    property GroupId: integer read FGroupId write FGroupId;
  end;

var
  Person: TPerson;

function GetLocalIP: string;
procedure GenerateQRCode(const AText: string; AImage: TImage);
function GetUserAppPath: string;
function ExeExec(Str: string; Typ: TExecType; var AQuery: TFDQuery): boolean;
procedure FreeQueryAndConn(var AQuery: TFDQuery);

implementation

function GetLocalIP: String;
const
  WSVer = $101;
var
  wsaData: TWSAData;
  P: PHostEnt;
  Buf: array [0 .. 127] of Char;
begin
  Result := '';
  if WSAStartup(WSVer, wsaData) = 0 then
  begin
    if GetHostName(@Buf, 128) = 0 then
    begin
      P := GetHostByName(@Buf);
      if P <> nil then
        Result := iNet_ntoa(PInAddr(P^.h_addr_list^)^);
    end;
    WSACleanup;
  end;
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

function ExeExec(Str: string; Typ: TExecType; var AQuery: TFDQuery): boolean;
var
  FDConn: TFDConnection;
  FilePath: string;
begin
  Result := true;
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
    Result := false;
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
          Result := false;
        end;

    end;
  end;
end;

function GetUserAppPath: string;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Result := System.IOUtils.TPath.GetDocumentsPath;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := ExtractFilePath(paramstr(0));
{$ENDIF}
end;

procedure FreeQueryAndConn(var AQuery: TFDQuery);
begin
  AQuery.Connection.Connected := false;
  FreeAndNil(AQuery);
end;

end.
