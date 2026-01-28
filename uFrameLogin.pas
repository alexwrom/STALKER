unit uFrameLogin;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Effects, FMX.Objects, FMX.Layouts,
  Threading, uGlobal, FireDAC.Comp.Client, Rest.Json, IOUtils,
  OAuth2, Classes.Data, Classes.answer, Permissions, classes.user;

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
    procedure btnConfirmNameClick(Sender: TObject);
    procedure eNickNameEnter(Sender: TObject);
    procedure eNickNameExit(Sender: TObject);
    procedure eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  private
    procedure GetData;
    procedure GetImage(AHex: UnicodeString);
    procedure GetMap;
    procedure GetServerData;
    constructor Create(AOwner: TComponent); override;
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
      try
        GetData;
        GetMap;
      finally
        recLoading.Visible := false;
        StartApp;
      end;
    end);
end;

procedure TFrameLogin.GetData;
var
  AData: TData;
  AAnswer: TAnswer;
  vSQL: UnicodeString;
  I: Integer;
  vQuery: TFDQuery;
  vUser: TUser;
begin
  ProgressBar.Value := 0;
  try
    vUser:= TUser.Create;
    vUser.Username := eNickName.Text;
    vUser.Password := '111';
    vUser.UserID := -1;

    AAnswer := TJSON.JsonToObject<TAnswer>(PostDataServer('api/get_data', TJson.ObjectToJsonString(vUser)));
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

    ExeExec(vSQL, exExecute, vQuery);
  finally
    FreeAndNil(AAnswer);
    FreeAndNil(AData);
  end;
end;

procedure TFrameLogin.GetMap;
var
  AAnswer: TAnswer;
begin
  try
    AAnswer := TJSON.JsonToObject<TAnswer>(GetDataServer('api/get_map'));
    GetImage(AAnswer.Json);
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

procedure TFrameLogin.btnConfirmNameClick(Sender: TObject);
begin
  if eNickName.Text = '' then
    Showmessage('Введите ваше имя')
  else
  begin
    PermissionsService.RequestPermissions(['android.permission.WRITE_EXTERNAL_STORAGE'],
      procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
        begin
          Person.UserName := eNickName.Text;
          layEnterName.Visible := false;
          recLoading.Visible := true;
          GetServerData;
        end
        else
        begin
          Showmessage('Необходимы разрешения для к памяти устройства.');
        end;
      end);
  end;
end;

constructor TFrameLogin.Create(AOwner: TComponent);
begin
  inherited;
  labSTALKER.TextSettings.Font.Family := 'montblancctt';
end;

procedure TFrameLogin.eNickNameEnter(Sender: TObject);
begin
  layPanel.MArgins.Bottom := 180;
end;

procedure TFrameLogin.eNickNameExit(Sender: TObject);
begin
  layPanel.MArgins.Bottom := 0;
end;

procedure TFrameLogin.eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (Key = 13) and (btnConfirmName.Visible) then
    btnConfirmNameClick(nil);
end;

end.
