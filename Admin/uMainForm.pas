unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdGlobal,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, uGlobal, Rest.Json, Classes.action,
  FireDAC.Comp.Client, FMX.StdCtrls, Generics.Collections, uGenericBaseData;

type
  TForm1 = class(TForm)
    IdTCPServer: TIdTCPServer;
    Memo1: TMemo;
    Button1: TButton;
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  vInsert: TList<UnicodeString>;
  I: Integer;
begin
  vInsert := GoGenericBaseData(1);

  for I := 0 to vInsert.Count - 1 do
    Memo1.Lines.Add(vInsert[I]);
end;

procedure TForm1.IdTCPServerExecute(AContext: TIdContext);
var
  vContext: string;
  vPerson: TPerson;
  vAnswer: TAction;
  FDQuery: TFDQuery;
  StrData: TStringList;
begin
  vContext := AContext.Connection.Socket.ReadLn();

  vPerson := TPerson.Create;
  vPerson := TJson.JsonToObject<TPerson>(vContext);

  ExeExec('select count(*) as cnt from users where nickname = ' + QuotedStr(vPerson.UserName) + ';', exActive, FDQuery);

  if FDQuery.FieldByName('cnt').AsInteger = 0 then // Создаем пользователя и высылаем все данные
  begin
    FreeQueryAndConn(FDQuery);
    StrData := TStringList.Create;
    try

      vAnswer := TAction.Create;
      vAnswer.SendType := stUpdateData;
      vAnswer.PageCount := 1;
    finally
      FreeAndNil(StrData);
    end;
  end
  else // Ищем для него информацию по уведомлениях
  begin
    FreeQueryAndConn(FDQuery);

  end;

  AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(vAnswer), IndyUTF8Encoding(true));
  AContext.Connection.Disconnect;

end;

end.
