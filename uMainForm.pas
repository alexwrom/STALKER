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
  uFrameQRScaner, uFrameIssuies, uFrameBag, Classes.sell, Classes.action, Rest.Json, IdGlobal, StrUtils, Threading, System.IOUtils,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, // Для JString
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.Provider,
  Androidapi.Helpers, // Для StringToJString и JStringToString
  Androidapi.JNI.Os,
  FMX.Platform.Android,
{$ENDIF}
  uScanerWiFi, FMX.Ani, FMX.Effects, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdTCPConnection, IdTCPClient, FMX.Edit, FMX.Media,
  uFrameLogin, Classes.answer, OAuth2, Classes.Data, uGenericBaseData, Classes.userdata;

type

  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabMap: TTabItem;
    TabPercs: TTabItem;
    Image8: TImage;
    Image11: TImage;
    Image13: TImage;
    Image10: TImage;
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
    recSkin1: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    recSkin: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    InnerGlowEffect4: TInnerGlowEffect;
    InnerGlowEffect5: TInnerGlowEffect;
    InnerGlowEffect6: TInnerGlowEffect;
    InnerGlowEffect7: TInnerGlowEffect;
    layPersonHealth: TLayout;
    recBack: TRectangle;
    MediaPlayer: TMediaPlayer;
    TimerUpdateData: TTimer;
    layZombTimer: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    Rectangle4: TRectangle;
    labZombTimer: TLabel;
    InnerGlowEffect1: TInnerGlowEffect;
    layBtnKill: TLayout;
    Image5: TImage;
    btnKill: TSpeedButton;
    Layout1: TLayout;
    Image1: TImage;
    TimerZombi: TTimer;
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
    procedure TimerUpdateDataTimer(Sender: TObject);
    procedure btnKillClick(Sender: TObject);
    procedure TimerZombiTimer(Sender: TObject);
  private

    procedure LoadArtefacts;
    procedure LoadPlaces;
    procedure LoadBag;
    procedure LoadCritical;

    procedure SynhronizeServer;

  public
    { Public declarations }
    FFrameMap: TFrameMap;
    FFramePercs: TFramePercs;
    FFrameDetector: TFrameDetector;
    FFrameQRScanner: TFrameQRScanner;
    FFrameIssuies: TFrameIssuies;
    FFrameBag: TFrameBag;
    FFrameLogin: TFrameLogin;
    procedure CreateFrameLogin;
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
  timerScannerWifiMerchant.Enabled := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Self.FullScreen := true;
  FKillType := ktLive;
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
    vIssueList.Visible := true;
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
if Assigned(FArtefactsList) then
    FArtefactsList.Clear
  else
    FArtefactsList := TList<TArtefactData>.Create;

  FArtefactsList.Clear;
  ExeExec('select * from arts_to_map atm join arts a on a.art_id = atm.art_id;', exActive, vQuery);
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

procedure TMainForm.LoadCritical;
var
  vQuery: TFDQuery;
  FCriticalItem: TCritical;
begin
if Assigned(FCritical) then
    FCritical.Clear
  else
    FCritical := TList<TCritical>.Create;

  ExeExec('select * from critical_issuies;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    FCriticalItem.Name := vQuery.FieldByName('name').AsString;
    FCriticalItem.TimeStart := vQuery.FieldByName('time_start').AsDateTime;
    FCriticalItem.TimeStop := vQuery.FieldByName('time_stop').AsDateTime;
    FCriticalItem.MinuteBeforeStartDamage := vQuery.FieldByName('minute_before_start_damage').AsInteger;
    FCritical.Add(FCriticalItem);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TMainForm.LoadPlaces;
var
  vQuery: TFDQuery;
  vPlaceData: TPlaceData;
begin
if Assigned(FPlacesList) then
    FPlacesList.Clear
  else
    FPlacesList := TList<TPlaceData>.Create;

  ExeExec('select * from places;', exActive, vQuery);
  try
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
  finally
    FreeQueryAndConn(vQuery);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  vUserExists: boolean;
  FDQuery: TFDQuery;
begin
  Person := TPerson.Create;
  ExeExec('select Count(1) as cnt from users;', exActive, FDQuery);
  vUserExists := FDQuery.FieldByName('cnt').AsInteger > 0;
  FreeQueryAndConn(FDQuery);

  if vUserExists then
  begin
    StartApp;
  end
  else
  begin
    Person.UserId := -1;
    Person.GroupId := 1;
    Person.CountContener := -1;
    CreateFrameLogin;
  end;
end;

procedure TMainForm.SynhronizeServer;
var
  AAnswer: TAnswer;
begin
  try
    AAnswer := TJSON.JsonToObject<TAnswer>(GetDataServer('api/upload_user_data'));
  finally
    FreeAndNil(AAnswer);
  end;
end;

procedure TMainForm.CreateFrameLogin;
begin
  if not Assigned(FFrameLogin) then
  begin
    FFrameLogin := TFrameLogin.Create(Self);
    FFrameLogin.Parent := Self;
  end;

  FFrameLogin.Visible := true;
  FFrameLogin.BringToFront;
end;

procedure TMainForm.StartApp;
var
  FDQuery: TFDQuery;
begin
  ExeExec('select user_id, group_id from users limit 1;', exActive, FDQuery);

  Person.UserId := FDQuery.FieldByName('user_id').AsInteger;
  Person.GroupId := FDQuery.FieldByName('group_id').AsInteger;
  Person.CountContener := -1;
  FreeQueryAndConn(FDQuery);

  LoadArtefacts;
  LoadIsuies;
  LoadPlaces;
  LoadCritical;

  if Assigned(FFrameMap) then
    FreeAndNil(FFrameMap);

  FFrameMap := TFrameMap.Create(TabMap);
  FFrameMap.Parent := TabMap;
  FFrameMap.timerCheckCritical.Enabled := true;

  if Assigned(FFramePercs) then
    FreeAndNil(FFramePercs);

  FFramePercs := TFramePercs.Create(TabPercs);
  FFramePercs.Parent := TabPercs;
  Person.GroupId := Person.GroupId;

  if Assigned(FFrameDetector) then
    FreeAndNil(FFrameDetector);

  FFrameDetector := TFrameDetector.Create(TabDetector);
  FFrameDetector.Parent := TabDetector;

  if Assigned(FFrameIssuies) then
    FreeAndNil(FFrameIssuies);

  FFrameIssuies := TFrameIssuies.Create(TabIssueis);
  FFrameIssuies.Parent := TabIssueis;

  if Assigned(FFrameQRScanner) then
    FreeAndNil(FFrameQRScanner);

  FFrameQRScanner := TFrameQRScanner.Create(TabQRScanner);
  FFrameQRScanner.Parent := TabQRScanner;

  if Assigned(FFrameLogin) then
    FFrameLogin.Visible := false;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION', 'android.permission.CHANGE_WIFI_STATE', 'android.permission.CAMERA'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
{$IFDEF ANDROID}
        FFrameMap.LocationServiceChanged;
{$ENDIF}
      end
      else
      begin
        Showmessage('Необходимы разрешения для сканирования GPS');
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
            vSell := TJSON.JsonToObject<TSell>(FFrameBag.FActiveAction.JSONObject);

            FFrameBag.FActiveAction.PageCount := 1;

            if TJSON.JsonToObject<TPerson>(vAnswerText).Cash - vSell.Cost >= 0 then
            begin
              ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''' + vSell.TableName + ''' and row_id = ' + vSell.RowID.ToString + ' and health = ' + vSell.Health.ToString + ' limit 1);', exExecute, FDQuery);
              Person.Cash := Person.Cash + vSell.Cost;

              AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFrameBag.FActiveAction), IndyUTF8Encoding(true));
              AContext.Connection.Disconnect;
            end
            else
            begin
              FFrameBag.FActiveAction.SendType := stCancelSell;
              FFrameBag.FActiveAction.JSONObject := '0';
              AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFrameBag.FActiveAction), IndyUTF8Encoding(true));
              AContext.Connection.Disconnect;
            end;
          end;
      end;
    finally
      FFrameBag.laySellQR.Visible := false;
      ReloadBag;
    end;
end;

procedure TMainForm.btnKillClick(Sender: TObject);
begin
  MessageDlg('Ты умер в бою?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    var
      vQuery: TFDQuery;
    begin
      if (AResult = mrYes) then
      begin
        TimerZombi.Enabled := false;
        FKillType := ktWeapon;
        layZombTimer.Visible := false;
        FIsDead := false;
        Person.Health := 0;
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
  layPersonHealth.Visible := false;

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
  FFrameBag.CreateElements(true);

  FFrameBag.LoadBagElements;

  FFrameBag.SwitchStyle.IsChecked := Person.IsClassicBag;
  FFrameBag.BringToFront;

  Person.Cash := Person.Cash;
  Person.GroupId := Person.GroupId;
end;

procedure TMainForm.btnToIssuiesClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabIssueis;
  recSelect.Parent := imgBtnIssuies;
  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  layPersonHealth.Visible := true;
  FFrameIssuies.btnToActiveClick(nil);
  FFrameIssuies.ClearSelection;
  Person.GroupId := Person.GroupId;
end;

procedure TMainForm.btnToMapClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabMap;
  recSelect.Parent := imgBtnMap;
  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  layPersonHealth.Visible := true;
end;

procedure TMainForm.btnToPercsClick(Sender: TObject);
begin
  SetHealthProgress(FFramePercs.HealthProgress, Person.Health);
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;
  FFrameQRScanner.StopScan;
  StopDetector;
  layPersonHealth.Visible := false;
  Person.GroupId := Person.GroupId;
end;

procedure TMainForm.btnToQRScannerClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabQRScanner;
  recSelect.Parent := imgBtnQRScanner;
  FFrameQRScanner.StartScan;
  StopDetector;
  layPersonHealth.Visible := true;
  Person.GroupId := Person.GroupId;
end;

procedure TMainForm.StopDetector;
var
  I: Integer;
begin
  if Assigned(FFrameDetector) then
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
      if Assigned(Self) then
      begin
        ConnectToMerchatZone; // Поиск зоны торговли

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin

            if Assigned(FFrameLogin) then
            begin
              FFrameLogin.layBtn.Visible := FIsMerchantZone;
              FFrameLogin.labNotConnect.Visible := NOT FIsMerchantZone;
            end;

            if Assigned(FFrameBag) then
              FFrameBag.laySells.Visible := FIsMerchantZone;

            ActiveScaner(FIsMerchantZone);

            if Assigned(Person) and (Person.UserId <> -1) then
              TimerUpdateData.Enabled := FIsMerchantZone;
          end);
      end;
    end).Start;

{$ENDIF}
end;

procedure TMainForm.TimerUpdateDataTimer(Sender: TObject);
begin
  TTask.Run(
    procedure
    var
      vAnswer: TAnswer;
      vData: TData;
      vUserdata: TUserdata;
      vSQL: Unicodestring;
      I: Integer;
      vQuery: TFDQuery;
    begin
      try
        vData := TData.Create;
        vData.SQL := TList<Unicodestring>.Create;
        vData.SQL := GoGenericBaseData();

        vUserdata := TUserdata.Create;
        vUserdata.UserId := Person.UserId;
        vUserdata.Data := vData;

        vAnswer := TJSON.JsonToObject<TAnswer>(PostDataServer('api/update_data', TJSON.ObjectToJsonString(vUserdata)));
        {
          vData := TJSON.JsonToObject<TData>(vAnswer.Json);

          if vData.SQL.Count > 0 then
          begin
          for I := 0 to vData.SQL.Count - 1 do
          vSQL := vSQL + vData.SQL[I];

          ExeExec(vSQL, exExecute, vQuery);
          end; }
      finally
        FreeAndNil(vAnswer);
        FreeAndNil(vData);
      end;
    end);
end;

procedure TMainForm.TimerZombiTimer(Sender: TObject);
var
  TimeParts: TArray<string>;
  Hours, Minutes, Seconds: Integer;
  TotalSeconds: Integer;
begin
  if labZombTimer.Text = '00:00:00' then
  begin
    TimerZombi.Enabled := false;
    FKillType := ktStopZombi;
    layZombTimer.Visible := false;
  end
  else
  begin
    // Разбиваем текущее время из Label на часы, минуты, секунды
    TimeParts := labZombTimer.Text.Split([':']);

    if Length(TimeParts) = 3 then
    begin
      Hours := StrToIntDef(TimeParts[0], 0);
      Minutes := StrToIntDef(TimeParts[1], 0);
      Seconds := StrToIntDef(TimeParts[2], 0);

      // Переводим всё в секунды и уменьшаем на 1
      TotalSeconds := Hours * 3600 + Minutes * 60 + Seconds - 1;

      // Проверяем, не истекло ли время
      if TotalSeconds >= 0 then
      begin
        // Преобразуем обратно в часы, минуты, секунды
        Hours := TotalSeconds div 3600;
        Minutes := (TotalSeconds mod 3600) div 60;
        Seconds := TotalSeconds mod 60;

        labZombTimer.Text := Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
      end;
    end;
  end;
end;

end.
