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
  IdGlobal, Classes.action, StrUtils, FMX.Effects, FMX.Memo, FMX.Memo.Types, FMX.ScrollBox, FMX.Edit, FMX.Layouts, Generics.Collections, classes.medicdata, classes.tehnicdata;

type
  TFrameQRScanner = class(TFrame)
    Camera: TCameraComponent;
    imgCamera: TImage;
    IdTCPClient: TIdTCPClient;
    recSkin: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    layInfo: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    Image10: TImage;
    Image11: TImage;
    Image13: TImage;
    Image8: TImage;
    Layout4: TLayout;
    Image3: TImage;
    btnYes: TCornerButton;
    Rectangle5: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    Label1: TLabel;
    recSkin1: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    btnChangeCamera: TSpeedButton;
    procedure btnYesClick(Sender: TObject);
    procedure btnChangeCameraClick(Sender: TObject);
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

procedure TFrameQRScanner.btnChangeCameraClick(Sender: TObject);
begin
   Camera.Active := false;

   if Camera.Kind = TCameraKind.BackCamera then
     Camera.Kind := TCameraKind.FrontCamera
   else
     Camera.Kind := TCameraKind.BackCamera;

   Camera.Active := True;
end;

procedure TFrameQRScanner.btnYesClick(Sender: TObject);
begin
  StartScan;
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
  layInfo.Visible := false;
  fFrameTake := 0;
  fScanBitmap := nil;
  Camera.OnSampleBufferReady := CameraSampleBufferReady;
  PermissionsService.RequestPermissions(['android.permission.CAMERA'], CameraPermissionRequestResult);
end;

procedure TFrameQRScanner.StopScan;
begin
  Camera.Active := false;
  layInfo.Visible := True;
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
            vSend: TSend;
            vAction: TAction;
            vSell: TSell;
            vRowID: Integer;
            vTableName: string;
            vQuery: TFDQuery;
            vPercName : string;
            vLevelPerc : integer;
            vMedic : TMedicData;
            vTehnic : TTehnicData;
          begin
            if (ReadResult <> nil) then
            begin
{$IF Defined(ANDROID)}
              Vibration(100);
{$ENDIF}
              StopScan;

              vSend := TSend.Create;

              try
                vSend := TJson.JsonToObject<TSend>(ReadResult.Text);

                if (vSend.Code <> '') and (not Person.IsDead) then
                begin
                  case Length(vSend.Code) of
                    2: // Детектор      {"code":"01"}
                      begin
                        SetDetector(vSend.Code.ToInteger());
                        ExeExec(Format('update users set detector_id = %d;', [vSend.Code.ToInteger()]), exExecute, FDQuery);
                      end;
                    3: // Смена группировки  {"code":"001"}
                      begin
                        Person.GroupId := vSend.Code.ToInteger;
                        ExeExec(Format('update users set group_id = %d;', [vSend.Code.ToInteger()]), exExecute, FDQuery);
                      end;
                    4: // Смена специальности  {"code":"0101"}
                      begin
                        vLevelPerc := Copy(vSend.Code, 3, 2).ToInteger;

                        case Copy(vSend.Code, 1, 2).ToInteger of
                          1:
                            begin
                              vPercName := 'level_medic';
                              Person.LevelMedic := vLevelPerc;
                            end;
                          2:
                            begin
                              vPercName := 'level_tehnic';
                              Person.LevelTehnic := vLevelPerc;
                            end;
                        end;

                        ExeExec(Format('update users set %s = %d;', [vPercName, vLevelPerc]), exExecute, FDQuery);
                      end;
                    5: // Добавление в сумку   {"code":"01001"}
                      begin
                        case Copy(vSend.Code, 1, 2).ToInteger of
                          1:
                            vTableName := 'armors';
                          2:
                            vTableName := 'arts';
                          3:
                            vTableName := 'medical';
                          4:
                            vTableName := 'weapons';
                        end;

                        vRowID := Copy(vSend.Code, 3, 3).ToInteger;

                        ExeExec(Format('insert into bag (table_name, row_id, health) values(''%s'', %d, 100);', [vTableName, vRowID]), exExecute, FDQuery);

                        {if vTableName = 'arts' then                     // Сделать удаление арта с карты при его нахождении
                          ExeExec(Format('update art_to_map set active = false where art_id = %d;', [vRowID]), exExecute, FDQuery);  }
                      end;
                    7: // Деньги      {"code":"0050000"}
                      begin
                        Person.Cash := Person.Cash + vSend.Code.ToInteger;
                        ExeExec(Format('update users set cash = %d;', [Round(Person.Cash)]), exExecute, FDQuery);
                      end;
                  end;
                end
                else
                if Assigned(vSend.Marker) and (not Person.IsDead) then
                  begin
                     case vSend.Marker.MarkerType of
                      0:
                          NewMarkerToMap(vSend.Marker.Coords, 'Чужая точка', mtPoint, false);
                      1:
                          NewMarkerToMap(vSend.Marker.Coords, 'Радиация', mtPointRad, false);
                      2:
                          NewMarkerToMap(vSend.Marker.Coords, 'Аномалия', mtPointAnomaly, false);
                      3:
                          NewMarkerToMap(vSend.Marker.Coords, 'Чужой схрон', mtPointBag, false);
                    end;

                    ExeExec(Format('insert into markers (lat, lon, marker_type_id, is_owner) values (%s, %s, %d, false);',[StringReplace(vSend.Marker.Coords.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(vSend.Marker.Coords.Longitude.ToString, ',', '.', [rfReplaceAll]), vSend.Marker.MarkerType]), exExecute, vQuery);
                  end
                else
                begin
                  if FIsMerchantZone then
                    begin
                      IdTCPClient.Host := vSend.Ip;
                      IdTCPClient.Port := 2026;

                      IdTCPClient.Connect;
                      try
                        IdTCPClient.IOHandler.WriteLn(TJson.ObjectToJsonString(Person), IndyUTF8Encoding(True));
                        vAction := TJson.JsonToObject<TAction>(IdTCPClient.IOHandler.ReadLn(#13#10, IndyUTF8Encoding(True)));

                        case vAction.SendType of
                          stSell:
                            if (not Person.IsDead) then
                              begin
                                vSell := TSell.Create;

                                try
                                  vSell := TJson.JsonToObject<TSell>(vAction.JSONObject);

                                  ExeExec(Format('insert into bag (table_name, row_id, health) values(''%s'', %d, %d);', [vSell.TableName, vSell.RowID, Round(vSell.Health)]), exExecute, FDQuery);
                                  Person.Cash := Person.Cash - vSell.Cost;
                                finally
                                  vSell.Free;
                                end;

                                ReloadBag;
                              end;

                          stCancelSell:
                            ShowMessage('Недостаточно средств');

                          stIncorrectLevelTehnic:
                            begin
                              btnYesClick(nil);
                              ShowMessage('Ремонт невозможен. Уровень техника не соответствует');
                              OpenPercs;
                            end;

                          stMedic:
                            begin
                              vMedic := TJson.JsonToObject<TMedicData>(vAction.JSONObject);
                              btnYesClick(nil);

                              if (Person.IsDead) then
                              begin
                                if vMedic.IsRestore then
                                  begin
                                    Person.Health := Person.Health + vMedic.Health;
                                    OpenMap;
                                  end
                              end
                                else
                                  Person.Health := Person.Health + vMedic.Health;
                            end;

                           stTehnic:
                            begin
                              btnYesClick(nil);

                              if (not Person.IsDead) then
                              begin
                                vTehnic := TJson.JsonToObject<TTehnicData>(vAction.JSONObject);

                                if Person.IsRestoreWeapon then
                                  begin
                                    if NOT vTehnic.IsFree then
                                      begin
                                        ExeExec(Format('select round(%s * cost) as cost from weapons where weapon_id = %d', [StringReplace((1 - Person.WeaponHealth / 100).ToString, ',', '.', [rfReplaceAll]), Person.WeaponId]), exActive, FDQuery);

                                        try
                                          Person.Cash := Person.Cash - FDQuery.FieldByName('cost').AsInteger;
                                        finally
                                          FreeQueryAndConn(FDQuery);
                                        end;
                                      end;

                                    Person.WeaponHealth := 100;
                                  end
                                else
                                  begin
                                    if NOT vTehnic.IsFree then
                                      begin
                                        ExeExec(Format('select round(%s * cost) as cost from armors where armor_id = %d', [StringReplace((1 - Person.ArmorHealth / 100).ToString, ',', '.', [rfReplaceAll]), Person.ArmorId]), exActive, FDQuery);

                                        try
                                          Person.Cash := Person.Cash - FDQuery.FieldByName('cost').AsInteger;
                                        finally
                                          FreeQueryAndConn(FDQuery);
                                        end;
                                      end;

                                    Person.ArmorHealth := 100;
                                  end;

                                ReloadPercs;
                                OpenPercs;
                              end
                              else
                                ShowMessage('Ты мерт, чтобы ремонтировать свои вещи');
                            end;
                        end;
                      finally
                        IdTCPClient.Disconnect;
                      end;
                    end
                  else
                    begin
                      ShowMessage('Нет подключения к сталкерской сети.');
                      StartScan;
                    end;
                end;
              finally
                vSend.Free;
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
