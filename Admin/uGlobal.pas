unit uGlobal;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, System.IOUtils, FireDAC.Comp.Client, FireDAC.Comp.DataSet, System.SysUtils, System.Sensors, FMX.Objects,
  Generics.Collections, FMX.Graphics, System.UITypes, System.Types, FMX.Layouts, Math;

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

function GetUserAppPath: string;
function ExeExec(Str: string; Typ: TExecType; var AQuery: TFDQuery): boolean;
procedure FreeQueryAndConn(var AQuery: TFDQuery);

implementation

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

function GetUserAppPath: string;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  result := System.IOUtils.TPath.GetDocumentsPath;
{$ENDIF}
{$IFDEF MSWINDOWS}
  result := ExtractFilePath(paramstr(0));
{$ENDIF}
end;

procedure FreeQueryAndConn(var AQuery: TFDQuery);
begin
  AQuery.Connection.Connected := false;
  FreeAndNil(AQuery);
end;

end.
