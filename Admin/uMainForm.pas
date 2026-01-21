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
    ProgressBar: TProgressBar;
    labStatusLoadData: TLabel;
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStrdata: UnicodeString;
    FStrDataForAdmin: UnicodeString;
    FPageCount: Integer;
    FPageCountAdmin: Integer;
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
  vPageCount: Integer;
  I: Integer;
  s: string;
begin
  Memo1.Text := GoGenericBaseData(vPageCount);

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
      ProgressBar.Value := 0;
      ProgressBar.Max := 18;
      FStrdata := GoGenericBaseData(FPageCount);
      ProgressBar.Value := 0;
      ProgressBar.Max := 7;
      FStrDataForAdmin := GoGenericBaseData(FPageCountAdmin, true);

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
  StrData: UnicodeString;
  vPageCount: Integer;
  vStrData: UnicodeString;
  vAction: TAction;
  vStringData: TList<UnicodeString>;
  vStr, vString: UnicodeString;
  I: Integer;
begin
  vContext := AContext.Connection.Socket.ReadLn(IndyUTF8Encoding(true));

  if vContext = 'PDA ADMIN' then
  begin
    vAnswer := TAction.Create;
    vAnswer.SendType := stUpdateData;
    vPageCount := 0;
    vAnswer.PageCount := FPageCountAdmin;
    vStrData := FStrDataForAdmin;
  end
  else if Copy(vContext, 1, 7) = 'INSERT:' then
  begin
    try
      ExeExec('delete from arts_to_map; delete from anomalies; delete from places;', exExecute, FDQuery);

      vAction := TJson.JsonToObject<TAction>(Copy(vContext, 8));

      vStringData := TList<UnicodeString>.Create;

      if vAction.PageCount > 0 then
      begin

        for I := 1 to vAction.PageCount do
        begin
          vStr := AContext.Connection.Socket.ReadLn(#13#10, IndyUTF8Encoding(true));
          vStringData.Add(vStr);
        end;
      end;

      case vAction.SendType of
        stUpdateData:
          begin
            For I := 0 to vStringData.Count - 1 do
              vString := vString + vStringData[I];

            ExeExec(vString, exExecute, FDQuery);
          end;
      end;

      vAnswer := TAction.Create;
      vAnswer.SendType := stUpdateData;
      vAnswer.PageCount := 0;
      vStrData := '';
    except
      vAnswer := TAction.Create;
      vAnswer.SendType := stCancel;
      vAnswer.PageCount := 0;
      vStrData := '';
    end;

  end
  else
  begin
    vPerson := TPerson.Create;
    vPerson := TJson.JsonToObject<TPerson>(vContext);

    ExeExec('select count(*) as cnt from users where nickname = ' + QuotedStr(vPerson.UserName) + ';', exActive, FDQuery);

    if FDQuery.FieldByName('cnt').AsInteger = 0 then // Создаем пользователя и высылаем все данные
    begin
      FreeQueryAndConn(FDQuery);

      ExeExec('insert into users (nickname, group_id) values (' + QuotedStr(vPerson.UserName) + ', 1);', exExecute, FDQuery);
      vAnswer := TAction.Create;
      vAnswer.SendType := stUserExists;
      vPageCount := 1;
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
  end;

  AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(vAnswer) + #13#10 + StrData, IndyUTF8Encoding(true));
  AContext.Connection.Disconnect;

end;

end.
