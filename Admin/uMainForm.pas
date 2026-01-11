unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdGlobal,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, uGlobal, Rest.Json, Classes.action,
  FireDAC.Comp.Client, FMX.StdCtrls, Generics.Collections, uGenericBaseData, StrUtils;

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
  StrData: string;
  vPageCount: Integer;
begin
  vPageCount := 0;
  StrData := GoGenericBaseData(1, vPageCount);
  Memo1.Lines.Add('Count: ' + vPageCount.ToString);
  Memo1.Lines.Add(StrData);
end;

procedure TForm1.IdTCPServerExecute(AContext: TIdContext);
var
  vContext: string;
  vPerson: TPerson;
  vAnswer: TAction;
  FDQuery: TFDQuery;
  StrData: UnicodeString;
  vPageCount: Integer;
begin
  vContext := AContext.Connection.Socket.ReadLn();

  vPerson := TPerson.Create;
  vPerson := TJson.JsonToObject<TPerson>(vContext);

  ExeExec('select count(*) as cnt from users where nickname = ' + QuotedStr(vPerson.UserName) + ';', exActive, FDQuery);

  if FDQuery.FieldByName('cnt').AsInteger = 0 then // Создаем пользователя и высылаем все данные
  begin
    FreeQueryAndConn(FDQuery);

    ExeExec('insert into users(nickname) values (' + QuotedStr(vPerson.UserName) + ');', exActive, FDQuery);
    vAnswer := TAction.Create;
    vAnswer.SendType := stUpdateData;
    vPageCount := 0;
    StrData := GoGenericBaseData(1, vPageCount);
    StrData := 'insert into users(nickname) values (' + QuotedStr(vPerson.UserName) + ');' + #13#10 + StrData;
    vAnswer.PageCount := vPageCount + 2;
  end
  else
  begin
    FreeQueryAndConn(FDQuery);

    if vPerson.UserId = -1 then // Если в базе есть логин, а на телефоне не зарегистрирован, то возвращаем ошибку
    begin
      vAnswer := TAction.Create;
      vAnswer.SendType := stUserExists;
      vPageCount := 1;
    end
    else // Ищем для него информацию по уведомлениях
    begin

    end;
  end;

  AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(vAnswer) + IfThen(StrData = '', '', #13#10 + StrData), IndyUTF8Encoding(true));
  AContext.Connection.Disconnect;

end;

end.
