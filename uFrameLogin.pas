unit uFrameLogin;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Effects, FMX.Objects, FMX.Layouts,
  Threading, uGlobal, FireDAC.Comp.Client, Rest.Json, IOUtils,
  OAuth2, Classes.Data, Classes.answer, Permissions, Classes.user;

type
  TFrameLogin = class(TFrame)
    layEnterName: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    imgBottom: TImage;
    imgTop: TImage;
    Image1: TImage;
    Image3: TImage;
    Rectangle4: TRectangle;
    InnerGlowEffect9: TInnerGlowEffect;
    layBtn: TLayout;
    Image4: TImage;
    btnConfirmName: TCornerButton;
    eNickName: TEdit;
    labSTALKER: TLabel;
    InnerGlowEffect1: TInnerGlowEffect;
    Label1: TLabel;
    labNotConnect: TLabel;
    Label5: TLabel;
    recLoading: TRectangle;
    AniIndicator1: TAniIndicator;
    Label3: TLabel;
    Label4: TLabel;
    ProgressBar: TProgressBar;
    ePassword: TEdit;
    Layout1: TLayout;
    btnShowPassword: TSpeedButton;
    Image2: TImage;
    procedure btnConfirmNameClick(Sender: TObject);
    procedure eNickNameEnter(Sender: TObject);
    procedure eNickNameExit(Sender: TObject);
    procedure eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure btnShowPasswordClick(Sender: TObject);
    procedure ePasswordKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  private
    function GetData: boolean;
    procedure GetImage(AHex: UnicodeString);
    function GetMap: boolean;
    procedure GetServerData;
    constructor Create(AOwner: TComponent); override;
    procedure NextStepToLoad;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameLogin.GetServerData;
begin
  TTask.Run(
    procedure
    begin
      if GetData and GetMap then
        StartApp
      else
      begin
        layEnterName.Visible := true;
        recLoading.Visible := false;
      end;
    end);
end;

function TFrameLogin.GetData: boolean;
var
  AData: TData;
  AAnswer: TAnswer;
  vSQL: UnicodeString;
  I: Integer;
  vQuery: TFDQuery;
  vUser: TUser;
  vJSON : string;
begin
  Result := false;
  ProgressBar.Value := 0;
  try
    vUser := TUser.Create;
    vUser.Username := eNickName.Text;
    vUser.Password := ePassword.Text;
    vUser.UserID := -1;

    AAnswer := TJSON.JsonToObject<TAnswer>(PostDataServer('api/get_data',TJSON.ObjectToJsonString(vUser)));

    if Assigned(AAnswer) then
      if (AAnswer.Status = 'success') then
      begin
        AData := TJSON.JsonToObject<TData>(AAnswer.Json);

        TThread.Synchronize(nil,
          procedure
          begin
            ProgressBar.Max := AData.SQL.Count;
          end);

        for I := 0 to AData.SQL.Count - 1 do
        begin
          vSQL := vSQL + AData.SQL[I];

          TThread.Synchronize(nil,
            procedure
            begin
              ProgressBar.Value := ProgressBar.Value + 1;
            end);
        end;

        try
          ExeExec(vSQL, exExecute, vQuery);
          Result := true;
        except
          Result := false;

          TThread.Synchronize(nil,
            procedure
            begin
              Showmessage('Ошибка обновления базы');
            end);
        end;
      end
      else
      begin
        Result := false;

        TThread.Synchronize(nil,
          procedure
          begin
            Showmessage(AAnswer.Message);
          end);
      end
    else
    begin
      Result := false;

      TThread.Synchronize(nil,
        procedure
        begin
          Showmessage('Нет связи с сетью');
        end);
    end;
  finally
    FreeAndNil(AAnswer);
    FreeAndNil(AData);
  end;
end;

function TFrameLogin.GetMap: boolean;
var
  AAnswer: TAnswer;
begin
  Result := false;
  try
    try
      AAnswer := TJSON.JsonToObject<TAnswer>(GetDataServer('api/get_map'));
      GetImage(AAnswer.Json);
      Result := true;
    except
      Result := false;

      TThread.Synchronize(nil,
        procedure
        begin
          Showmessage('Ошибка сохранения карты');
        end);
    end;

  finally
    FreeAndNil(AAnswer);
  end;
end;

procedure TFrameLogin.GetImage(AHex: UnicodeString);
var
  Stream: TMemoryStream;
  I: Integer;
  ByteValue: Byte;
  Bytes: TBytes;
  HexByte: UnicodeString;
  vPath: string;
begin
  SetLength(Bytes, Length(AHex) div 2);
  // Конвертируем шестнадцатеричную строку в байты
  for I := 0 to (Length(AHex) div 2) - 1 do
  begin
    HexByte := Copy(AHex, I * 2 + 1, 2);
    try
      ByteValue := StrToInt('$' + HexByte);
      Bytes[I] := ByteValue;
    except
    end;
  end;

  // Создаем поток из байт
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(Bytes[0], Length(Bytes));
    Stream.Position := 0;

    vPath := System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'map_image.png');

    if FileExists(vPath) then
      TFile.Delete(vPath);

    Stream.SaveToFile(vPath);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFrameLogin.NextStepToLoad;
begin
   Person.Username := eNickName.Text;
   layEnterName.Visible := false;
   recLoading.Visible := true;
   layPanel.Margins.Bottom := 0;

   GetServerData;
end;

procedure TFrameLogin.btnConfirmNameClick(Sender: TObject);
begin
  if (layBtn.Visible) then
    if eNickName.Text = '' then
      Showmessage('Введи свое имя')
    else if ePassword.Text = '' then
      Showmessage('Введи свой пароль')
    else
    begin
      if TOSVersion.Check(12) then
        NextStepToLoad
      else
      PermissionsService.RequestPermissions(['android.permission.WRITE_EXTERNAL_STORAGE'],
        procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
        var
          vQuery: TFDQuery;
        begin
          if (Length(GrantResults) = 1) and (GrantResults[0] = TPermissionStatus.Granted) then
            NextStepToLoad
          else
          begin
            Showmessage('Необходимы разрешения к памяти устройства.');
          end;
        end);
    end;
end;

procedure TFrameLogin.btnShowPasswordClick(Sender: TObject);
begin
  ePassword.Password := Not ePassword.Password;
end;

constructor TFrameLogin.Create(AOwner: TComponent);
begin
  inherited;
  labSTALKER.TextSettings.Font.Family := 'montblancctt';
end;

procedure TFrameLogin.eNickNameEnter(Sender: TObject);
begin
  layPanel.Margins.Bottom := 200;
end;

procedure TFrameLogin.eNickNameExit(Sender: TObject);
begin
  layPanel.Margins.Bottom := 0;
end;

procedure TFrameLogin.eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (Key = 13) then
    ePassword.SetFocus;
end;

procedure TFrameLogin.ePasswordKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (Key = 13) then
    btnConfirmNameClick(nil);
end;

end.
