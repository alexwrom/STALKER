unit uFrameQRScaner;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Objects,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager, FMX.Platform, Permissions, FMX.Controls.Presentation,
  uGlobal, Classes.sell, Rest.Json, Classes.send, FireDAC.Comp.Client, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdGlobal, Classes.action, StrUtils, FMX.Effects, FMX.Memo, FMX.Memo.Types, FMX.ScrollBox, FMX.Edit, FMX.Layouts, Generics.Collections;

type
  TFrameQRScanner = class(TFrame)
    Camera: TCameraComponent;
    imgCamera: TImage;
    IdTCPClient: TIdTCPClient;
    Rectangle1: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
  private
    fScanInProgress: Boolean;
    fFrameTake: Integer;
    fScanBitmap: TBitmap;
    procedure CameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure ParseImage;
    procedure CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
    { Private declarations }
  public
    { Public declarations }
    procedure StartScan;
    procedure StopScan;
    destructor Destroy; override;
    constructor Create(AObject: TFmxObject);
  end;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

constructor TFrameQRScanner.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);

end;

procedure TFrameQRScanner.CameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    Camera.Active := false;
    Camera.Active := True;
  end
  else
    TDialogService.ShowMessage('Нужны разрешения на использование камеры')
end;

procedure TFrameQRScanner.StartScan;
begin
  fFrameTake := 0;
  fScanBitmap := nil;
  Camera.OnSampleBufferReady := CameraSampleBufferReady;
  PermissionsService.RequestPermissions(['android.permission.CAMERA'], CameraPermissionRequestResult);
end;

procedure TFrameQRScanner.StopScan;
begin
  Camera.Active := false;
end;

procedure TFrameQRScanner.CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      Camera.SampleBufferToBitmap(imgCamera.Bitmap, True);

      if (fScanInProgress) then
      begin
        exit;
      end;

      inc(fFrameTake);
      if (fFrameTake mod 4 <> 0) then
      begin
        exit;
      end;

      if Assigned(fScanBitmap) then
        FreeAndNil(fScanBitmap);

      fScanBitmap := TBitmap.Create();
      fScanBitmap.Assign(imgCamera.Bitmap);

      ParseImage();
    end);

end;

destructor TFrameQRScanner.Destroy;
begin
  if Assigned(fScanBitmap) then
    FreeAndNil(fScanBitmap);

  inherited Destroy;
end;

procedure TFrameQRScanner.ParseImage();
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      ReadResult: TReadResult;
      ScanManager: TScanManager;
    begin
      fScanInProgress := True;
      ScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
      try

        try
          ReadResult := ScanManager.Scan(fScanBitmap);
        except
        end;

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          var
            FDQuery: TFDQuery;
            vAnswer: string;
            vSend: TSend;
            vAction: TAction;
            vSell: TSell;
            vStringData: TList<UnicodeString>;
            I: Integer;
            Page, vRowID: Integer;
            vStr, vTableName: string;
          begin
            if (ReadResult <> nil) then
            begin
              StopScan;

              vSend := TSend.Create;
              vSend := TJson.JsonToObject<TSend>(ReadResult.Text);

              if vSend.Code <> '' then
              begin
                case Copy(vSend.Code,1,2).ToInteger of
                  1:vTableName := 'armors';
                  2:vTableName := 'arts';
                  3:vTableName := 'medical';
                  4:vTableName := 'weapons';
                end;

                vRowID := Copy(vSend.Code,3,3).ToInteger;

                ExeExec(Format('insert into bag (table_name, row_id, health) values(''%s'', %d, 100);', [vTableName, vRowID]), exExecute, FDQuery);
              end
              else
              begin
                IdTCPClient.Host := vSend.Ip;
                IdTCPClient.Port := 2026;

                IdTCPClient.Connect;
                try
                  IdTCPClient.IOHandler.WriteLn(TJson.ObjectToJsonString(Person), IndyUTF8Encoding(True));
                  vStringData := TList<UnicodeString>.Create;
                  try

                    vAction := TJson.JsonToObject<TAction>(IdTCPClient.IOHandler.ReadLn(#13#10, IndyUTF8Encoding(True)));
                    Page := 1;

                    if vAction.PageCount > 1 then
                    begin
                      while Page <> vAction.PageCount do
                      begin
                        vStr := IdTCPClient.IOHandler.ReadLn(#13#10, IndyUTF8Encoding(True));

                        if vStr[1] = '~' then
                        begin
                          vStringData[vStringData.Count - 1] := vStringData[vStringData.Count - 1] + Copy(vStr, 2, Length(vStr) - 1);
                          Dec(Page);
                        end
                        else
                          vStringData.Add(vStr);

                        inc(Page);
                      end;
                    end;

                    case vAction.SendType of
                      stSell:
                        begin
                          vSell := TSell.Create;
                          vSell := TJson.JsonToObject<TSell>(vAction.JSONObject);

                          ExeExec(Format('insert into bag (table_name, row_id, health) values(''%s'', %d, %d);', [vSell.TableName, vSell.RowID, Round(vSell.Health)]), exExecute, FDQuery);
                          Person.Cash := Person.Cash - vSell.Cost;
                          ReloadBag;
                        end;

                      stCancelSell:
                        ShowMessage('Недостаточно средств');

                      stLoadArmor:
                        begin

                        end;
                    end;
                  finally
                    FreeAndNil(vStringData);
                  end;

                finally
                  IdTCPClient.Disconnect;
                end;
              end;
            end;
          end);

      finally
        if ReadResult <> nil then
          FreeAndNil(ReadResult);

        ScanManager.Free;
        fScanInProgress := false;
      end;

    end).Start();

end;

end.
