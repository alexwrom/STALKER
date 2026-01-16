unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdGlobal,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, uGlobal, Rest.Json, Classes.action,
  FireDAC.Comp.Client, FMX.StdCtrls, Generics.Collections, uGenericBaseData, StrUtils, Classes.send, Threading,
  FMX.Objects;

type
  TMainForm = class(TForm)
    IdTCPServer: TIdTCPServer;
    Button1: TButton;
    Memo1: TMemo;
    ImgQR: TImage;
    labStatusLoadData: TLabel;
    ProgressBar: TProgressBar;
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStrdata: UnicodeString;
    FPageCount: Integer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.Button1Click(Sender: TObject);
var
  vSend: TSend;

  I: Integer;
  s: string;
begin
  Memo1.Text := FStrdata;

  { vSend := TSend.Create;
    try
    vSend.Ip := GetLocalIP;
    GenerateQRCode(TJson.ObjectToJsonString(vSend), ImgQR);
    finally
    FreeAndNil(vSend);
    end; }
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FPageCount := 0;

  TTask.Run(
    procedure
    begin
      FStrdata := GoGenericBaseData(1, FPageCount);

      TThread.Synchronize(nil,
      procedure
      begin
        ProgressBar.Value := 0;
        labStatusLoadData.Text := 'Готово';
      end);
    end);
end;

procedure TMainForm.IdTCPServerExecute(AContext: TIdContext);
var
  vContext: string;
  vPerson: TPerson;
  vAnswer: TAction;
  FDQuery: TFDQuery;
  vStrData: UnicodeString;
begin
  vContext := AContext.Connection.Socket.ReadLn();

  vPerson := TPerson.Create;
  vPerson := TJson.JsonToObject<TPerson>(vContext);

  ExeExec('select count(*) as cnt from users where nickname = ' + QuotedStr(vPerson.UserName) + ';', exActive, FDQuery);

  if FDQuery.FieldByName('cnt').AsInteger = 0 then // Создаем пользователя и высылаем все данные
  begin
    FreeQueryAndConn(FDQuery);

    ExeExec('insert into users (nickname, group_id) values (' + QuotedStr(vPerson.UserName) + ', 1);', exExecute, FDQuery);
    vAnswer := TAction.Create;
    vAnswer.SendType := stUpdateData;
    vStrData := 'insert into users (nickname, group_id) values (' + QuotedStr(vPerson.UserName) + ', 1);' + #13#10 + FStrdata;
    vAnswer.PageCount := FPageCount + 1;
  end
  else
  begin
    FreeQueryAndConn(FDQuery);

    if vPerson.UserId = -1 then // Если в базе есть логин, а на телефоне не зарегистрирован, то возвращаем ошибку
    begin
      vAnswer := TAction.Create;
      vAnswer.SendType := stUserExists;
      FPageCount := 1;
    end
    else // Ищем для него информацию по уведомлениях
    begin

    end;
  end;

  AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(vAnswer) + #13#10 + vStrData, IndyUTF8Encoding(true));
  AContext.Connection.Disconnect;

end;

end.
