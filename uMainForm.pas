unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, System.Sensors,
  System.Sensors.Components, Math, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Objects, uFramePercs, uFrameMap, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, uGlobal, uFrameDetector,
  uFrameQRScaner, uFrameIssuies, uFrameBag, Classes.sell, Classes.action, Rest.Json, IdGlobal, StrUtils, Threading,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, // Для JString
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.Provider,
  Androidapi.Helpers, // Для StringToJString и JStringToString
  Androidapi.JNI.Os,
  FMX.Platform.Android,
{$ENDIF}
  uScanerWiFi, FMX.Ani, FMX.Effects, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdTCPConnection, IdTCPClient, FMX.Edit;

type

  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabMap: TTabItem;
    TabPercs: TTabItem;
    Image8: TImage;
    Image11: TImage;
    Image13: TImage;
    Image10: TImage;
    FDConn: TFDConnection;
    FDQuery: TFDQuery;
    layMenu: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    imgBtnMap: TImage;
    ImgBtnPercs: TImage;
    imgBtnBag: TImage;
    imgBtnIssuies: TImage;
    Image7: TImage;
    Image9: TImage;
    Image12: TImage;
    Image14: TImage;
    Image15: TImage;
    btnToMap: TSpeedButton;
    btnToPercs: TSpeedButton;
    btnToBag: TSpeedButton;
    btnToIssuies: TSpeedButton;
    TabDetector: TTabItem;
    TabQRScanner: TTabItem;
    imgPersonHealth: TImage;
    HealthProgress: TRectangle;
    recSelect: TRectangle;
    igeDeadGlow: TInnerGlowEffect;
    animBlood: TFloatAnimation;
    layDamage: TLayout;
    imgBtnQRScanner: TImage;
    btnToQRScanner: TSpeedButton;
    Image2: TImage;
    TabIssueis: TTabItem;
    TabBag: TTabItem;
    IdTCPServer: TIdTCPServer;
    timerScannerWifiMerchant: TTimer;
    recBackgroudMenu: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
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
    Label1: TLabel;
    Label2: TLabel;
    labNotConnect: TLabel;
    AniIndicator1: TAniIndicator;
    recLoading: TRectangle;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    InnerGlowEffect1: TInnerGlowEffect;
    procedure FormCreate(Sender: TObject);
    procedure btnToMapClick(Sender: TObject);
    procedure btnToPercsClick(Sender: TObject);
    procedure btnToBagClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnToQRScannerClick(Sender: TObject);
    procedure btnToIssuiesClick(Sender: TObject);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure timerScannerWifiMerchantTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnConfirmNameClick(Sender: TObject);
    procedure eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure eNickNameEnter(Sender: TObject);
    procedure eNickNameExit(Sender: TObject);
  private
    vStringData: TList<UnicodeString>;
    procedure LoadArtefacts;
    procedure LoadPlaces;
    procedure LoadBag;
    procedure GetData;

  public
    { Public declarations }
    FFrameMap: TFrameMap;
    FFramePercs: TFramePercs;
    FFrameDetector: TFrameDetector;
    FFrameQRScanner: TFrameQRScanner;
    FFrameIssuies: TFrameIssuies;
    FFrameBag: TFrameBag;
    procedure StartApp;
    procedure LoadIsuies;
    procedure StopDetector;
    procedure CreateBagFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Permissions;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  CreateBagFrame;
  timerScannerWifiMerchant.Enabled := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Self.FullScreen := true;
  labSTALKER.TextSettings.Font.Family := 'montblancctt';
{$ENDIF}
end;

procedure TMainForm.LoadBag;
var
  vQuery: TFDQuery;
  vBagData: TBagData;
begin
  if NOT Assigned(FBagList) then
    FBagList := TList<TBagData>.Create
  else
    FBagList.Clear;

  ExeExec('select * from my_bag order by sor;', exActive, vQuery);
  if vQuery.RecordCount > 0 then
  begin
    vQuery.First;

    while Not vQuery.Eof do
    begin
      vBagData.Icon := TBitmap.Create;
      vBagData.Icon.Assign(vQuery.FieldByName('icon'));
      vBagData.TableName := vQuery.FieldByName('table_name').AsString;
      vBagData.RowID := vQuery.FieldByName('row_id').AsInteger;
      vBagData.Count := vQuery.FieldByName('count').AsInteger;

      if vBagData.TableName = 'arts' then
        vBagData.BagType := btArt
      else if vBagData.TableName = 'armors' then
        vBagData.BagType := btArmor
      else if vBagData.TableName = 'weapons' then
        vBagData.BagType := btWeapon
      else if vBagData.TableName = 'medical' then
        vBagData.BagType := btMedical
      else if vBagData.TableName = 'detectors' then
        vBagData.BagType := btDetector;

      vBagData.Health := vQuery.FieldByName('health').AsFloat;
      vBagData.HealthRestore := vQuery.FieldByName('health_restore').AsFloat;
      vBagData.Percs.PhisicArmor := vQuery.FieldByName('phisic').AsInteger;
      vBagData.Percs.RadiationArmor := vQuery.FieldByName('radiation').AsInteger;
      vBagData.Percs.ElectroArmor := vQuery.FieldByName('electro').AsInteger;
      vBagData.Percs.FireArmor := vQuery.FieldByName('fire').AsInteger;
      vBagData.Percs.PsiArmor := vQuery.FieldByName('psi').AsInteger;
      vBagData.Percs.ChimisheArmor := vQuery.FieldByName('chimishe').AsInteger;
      vBagData.CountSlots := vQuery.FieldByName('count_slots').AsInteger;
      vBagData.Cost := vQuery.FieldByName('cost').AsInteger;
      FBagList.Add(vBagData);
      vQuery.Next;
    end;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TMainForm.LoadIsuies;
var
  vQuery: TFDQuery;
  vIssueList: TIssueData;
begin
  if Assigned(FIssueList) then
    FIssueList.Clear
  else
    FIssueList := TList<TIssueData>.Create;

  ExeExec('select * from open_issuies;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vIssueList.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vIssueList.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vIssueList.ID := vQuery.FieldByName('issue_id').AsInteger;
    vIssueList.PrevID := vQuery.FieldByName('prev_issue_id').AsInteger;
    vIssueList.Cost := vQuery.FieldByName('cost').AsInteger;
    vIssueList.Name := vQuery.FieldByName('name').AsString;
    vIssueList.Detail := vQuery.FieldByName('detail').AsString;
    vIssueList.RadiusIN := vQuery.FieldByName('radius_in').AsInteger;
    vIssueList.RadiusOUT := vQuery.FieldByName('radius_out').AsInteger;
    vIssueList.CompleteAfterOUT := vQuery.FieldByName('complete_after_out').AsBoolean;
    vIssueList.CompleteAfterIN := vQuery.FieldByName('complete_after_in').AsBoolean;
    FIssueList.Add(vIssueList);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TMainForm.LoadArtefacts;
var
  vQuery: TFDQuery;
  vArtefactData: TArtefactData;
begin
  FArtefactsList := TList<TArtefactData>.Create;
  ExeExec('select * from arts_to_map;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vArtefactData.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vArtefactData.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vArtefactData.Level := vQuery.FieldByName('level').AsInteger;
    FArtefactsList.Add(vArtefactData);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TMainForm.LoadPlaces;
var
  vQuery: TFDQuery;
  vPlaceData: TPlaceData;
begin
  FPlacesList := TList<TPlaceData>.Create;
  ExeExec('select * from places;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vPlaceData.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vPlaceData.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vPlaceData.Name := vQuery.FieldByName('name').AsString;
    vPlaceData.Radius := vQuery.FieldByName('radius').AsInteger;

    if vQuery.FieldByName('type').AsString = 'mtBase' then
      vPlaceData.MarkerType := mtBase
    else if vQuery.FieldByName('type').AsString = 'mtSafe' then
      vPlaceData.MarkerType := mtSafe;

    FPlacesList.Add(vPlaceData);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  vUserExists: boolean;
begin
  ExeExec('select user_id  from users limit 1;', exActive, FDQuery);
  vUserExists := FDQuery.RecordCount = 1;
  FreeQueryAndConn(FDQuery);

  if vUserExists then
  begin
    StartApp;
  end
  else
  begin
    Person := TPerson.Create;
    Person.UserId := -1;
    Person.GroupId := -1;
    Person.CountContener := -1;
    layEnterName.Visible := true;
  end;
end;

procedure TMainForm.StartApp;
begin
  layEnterName.Visible := false;
  layMenu.Enabled := true;

  ExeExec('select user_id, group_id  from users limit 1;', exActive, FDQuery);
  Person := TPerson.Create;
  Person.UserId := FDQuery.FieldByName('user_id').AsInteger;
  Person.GroupId := FDQuery.FieldByName('group_id').AsInteger;
  Person.CountContener := -1;
  FreeQueryAndConn(FDQuery);

  LoadArtefacts;
  LoadIsuies;
  LoadPlaces;

  FFrameMap := TFrameMap.Create(TabMap);
  FFrameMap.Parent := TabMap;

  FFramePercs := TFramePercs.Create(TabPercs);
  FFramePercs.Parent := TabPercs;
  Person.GroupId := Person.GroupId;

  FFrameDetector := TFrameDetector.Create(TabDetector);
  FFrameDetector.Parent := TabDetector;

  FFrameIssuies := TFrameIssuies.Create(TabIssueis);
  FFrameIssuies.Parent := TabIssueis;

  if not Assigned(FFrameQRScanner) then
  begin
    FFrameQRScanner := TFrameQRScanner.Create(TabQRScanner);
    FFrameQRScanner.Parent := TabQRScanner;
  end;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION', 'android.permission.CHANGE_WIFI_STATE', 'android.permission.CAMERA'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        FFrameMap.LocationSensor.Active := true;
      end
      else
      begin
        Showmessage('Необходимы разрешения для сканирования Wi-Fi');
      end;
    end);
end;

procedure TMainForm.IdTCPServerExecute(AContext: TIdContext);
var
  vAnswerText: string;
  vSell: TSell;
  FDQuery: TFDQuery;
begin
  vAnswerText := AContext.Connection.Socket.ReadLn();

  if Assigned(FFrameBag.FActiveAction) then
    try
      case FFrameBag.FActiveAction.SendType of
        stSell:
          begin
            vSell := TSell.Create;
            vSell := TJson.JsonToObject<TSell>(FFrameBag.FActiveAction.JSONObject);

            FFrameBag.FActiveAction.PageCount := 1;

            if TJson.JsonToObject<TPerson>(vAnswerText).Cash - vSell.Cost >= 0 then
            begin
              ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''' + vSell.TableName + ''' and row_id = ' + vSell.RowID.ToString + ' and health = ' + vSell.Health.ToString + ' limit 1);', exExecute, FDQuery);
              Person.Cash := Person.Cash + vSell.Cost;

              AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(FFrameBag.FActiveAction), IndyUTF8Encoding(true));
              AContext.Connection.Disconnect;
            end
            else
            begin
              FFrameBag.FActiveAction.SendType := stCancelSell;
              FFrameBag.FActiveAction.JSONObject := '0';
              AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(FFrameBag.FActiveAction), IndyUTF8Encoding(true));
              AContext.Connection.Disconnect;
            end;
          end;
      end;
    finally
      FFrameBag.laySellQR.Visible := false;
      ReloadBag;
    end;
end;

procedure TMainForm.btnConfirmNameClick(Sender: TObject);
begin
  Person.UserName := eNickName.Text;
  layEnterName.Visible := false;
  recLoading.Visible := true;
  GetData;
end;

procedure TMainForm.GetData;
var
  FDQuery: TFDQuery;
  vAnswer: string;
  vAction: TAction;
  vSell: TSell;
  I: Integer;
  vStr: string;
  Page: Integer;
begin
  TTask.Run(
    procedure
    var
      vString: UnicodeString;
      I: Integer;
      FDQuery: TFDQuery;
      IdTCPClient: TIdTCPClient;
    begin
      IdTCPClient := TIdTCPClient.Create(nil);
      IdTCPClient.Host := '192.168.4.60';
      IdTCPClient.Port := 2026;
      try
        try
          IdTCPClient.Connect;

          IdTCPClient.IOHandler.WriteLn(TJson.ObjectToJsonString(Person), IndyUTF8Encoding(true));
          vStringData := TList<UnicodeString>.Create;
          try
            vAction := TJson.JsonToObject<TAction>(IdTCPClient.IOHandler.ReadLn(#13#10, IndyUTF8Encoding(true)));
            Page := 1;

            if vAction.PageCount > 1 then
            begin
              while Page <> vAction.PageCount do
              begin
                vStr := IdTCPClient.IOHandler.ReadLn(#13#10, IndyUTF8Encoding(true));

                if vStr[1] = '~' then
                begin
                  vStringData[vStringData.Count - 1] := vStringData[vStringData.Count - 1] + Copy(vStr, 2, Length(vStr) - 1);
                  Dec(Page);
                end
                else
                  vStringData.Add(vStr);

                Inc(Page);
              end;
            end;

            case vAction.SendType of
              stUpdateData:
                begin

                  For I := 0 to vStringData.Count - 1 do
                    vString := vString + vStringData[I];

                  ExeExec(vString, exExecute, FDQuery);

                  TThread.Synchronize(TThread.CurrentThread,
                    procedure
                    begin
                      recLoading.Visible := false;
                      sleep(3000);
                      StartApp;
                    end);
                end;

              stUserExists:
                begin
                  TThread.Synchronize(TThread.CurrentThread,
                    procedure
                    begin
                      Showmessage('Сталкер с таким именем уже зарегистрирован.');
                    end);

                  recLoading.Visible := false;
                  layEnterName.Visible := true;
                end;
            end;
          finally
            FreeAndNil(vStringData);
          end;

        finally
          IdTCPClient.Disconnect;
        end
      except
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            Showmessage('Сталкерская сеть недоступна.');
          end);

        recLoading.Visible := false;
        layEnterName.Visible := true;
      end;
    end);
end;

procedure TMainForm.btnToBagClick(Sender: TObject);
begin
  LoadBag;
  recSelect.Parent := imgBtnBag;

  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := false;

  CreateBagFrame;
  TabControl.ActiveTab := TabBag;
end;

procedure TMainForm.CreateBagFrame;
begin
  if Assigned(FFrameBag) then
  begin
    FFrameBag.Parent := nil;
    FFrameBag.Visible := false;
    FreeAndNil(FFrameBag);
  end;

  FFrameBag := TFrameBag.Create(TabBag);
  FFrameBag.Parent := TabBag;
  FFrameBag.layBag.Height := FFrameBag.Height - FFrameBag.layTopBorder.Height - FFrameBag.recCash.Height + 63;
  FFrameBag.layBag.Width := FFrameBag.Width;
  FFrameBag.CreateElements;

  FFrameBag.LoadBagElements;

  FFrameBag.SwitchStyle.IsChecked := Person.IsClassicBag;
  FFrameBag.BringToFront;

  Person.Cash := Person.Cash;
end;

procedure TMainForm.eNickNameEnter(Sender: TObject);
begin
  layPanel.MArgins.Bottom := 180;
end;

procedure TMainForm.eNickNameExit(Sender: TObject);
begin
  layPanel.MArgins.Bottom := 0;
end;

procedure TMainForm.eNickNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = 13 then
    btnConfirmNameClick(nil);
end;

procedure TMainForm.btnToIssuiesClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabIssueis;
  recSelect.Parent := imgBtnIssuies;
  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := true;
  FFrameIssuies.btnToActiveClick(nil);
  FFrameIssuies.ClearSelection;
end;

procedure TMainForm.btnToMapClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabMap;
  recSelect.Parent := imgBtnMap;
  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := true;
end;

procedure TMainForm.btnToPercsClick(Sender: TObject);
begin
  SetHealthProgress(FFramePercs.HealthProgress, Person.Health);
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := false;
  Person.GroupId := Person.GroupId;
end;

procedure TMainForm.btnToQRScannerClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabQRScanner;
  recSelect.Parent := imgBtnQRScanner;
  FFrameQRScanner.StartScan;
  StopDetector;
  imgPersonHealth.Visible := true;
end;

procedure TMainForm.StopDetector;
var
  I: Integer;
begin
  if FFrameDetector.timerScannerArtefacts.Enabled then
    for I := 0 to 2 do
    begin
      sleep(100);
      FFrameDetector.timerScannerArtefacts.Enabled := false;
      FFrameDetector.TimerSensor.Enabled := false;
    end;
end;

procedure TMainForm.timerScannerWifiMerchantTimer(Sender: TObject);
begin
{$IFDEF ANDROID}
  TThread.CreateAnonymousThread(
    procedure
    begin

      ConnectToMerchatZone; // Поиск зоны торговли

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Assigned(FFrameBag) then
            FFrameBag.laySells.Visible := FIsMerchantZone;

          ActiveScaner(FIsMerchantZone);

          layBtn.Visible := FIsMerchantZone;
          labNotConnect.Visible := NOT FIsMerchantZone;
        end);
    end).Start;
{$ENDIF}
end;

end.
