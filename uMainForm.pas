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
  uFrameLogin, Classes.answer, OAuth2, Classes.Data, uGenericBaseData, Classes.userdata, uFrameSettings, classes.tehnicdata, System.DateUtils, FMX.Platform;

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
    MediaPlayerMenu: TMediaPlayer;
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
    layDeadGlow: TLayout;
    Image4: TImage;
    Image6: TImage;
    Image16: TImage;
    Image17: TImage;
    Image18: TImage;
    Image19: TImage;
    Image20: TImage;
    Image21: TImage;
    animBlood: TFloatAnimation;
    MediaPlayerZombi: TMediaPlayer;
    MediaPlayerNotification: TMediaPlayer;
    recNotification: TRectangle;
    animNotification: TFloatAnimation;
    ImgSettings: TImage;
    btnSettings: TSpeedButton;
    Image22: TImage;
    InnerGlowEffect8: TInnerGlowEffect;
    TabSettings: TTabItem;
    layTehnicReload: TLayout;
    Rectangle9: TRectangle;
    labReloadTimerTehnic: TLabel;
    InnerGlowEffect10: TInnerGlowEffect;
    MediaPlayerWorking: TMediaPlayer;
    Layout2: TLayout;
    Layout3: TLayout;
    layStopWorking: TLayout;
    Image3: TImage;
    btnStopWorking: TCornerButton;
    TimerTehnicReload: TTimer;
    layCheckWiFiGPS: TLayout;
    Rectangle1: TRectangle;
    labCheckWifiGPSSeconds: TLabel;
    TimerCheckWiFiGPS: TTimer;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Image23: TImage;
    btnCheckWiFiGPS: TCornerButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnToMapClick(Sender: TObject);
    procedure btnToPercsClick(Sender: TObject);
    procedure btnToBagClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnToQRScannerClick(Sender: TObject);
    procedure btnToIssuiesClick(Sender: TObject);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure timerScannerWifiMerchantTimer(Sender: TObject);
    procedure TimerUpdateDataTimer(Sender: TObject);
    procedure btnKillClick(Sender: TObject);
    procedure TimerZombiTimer(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnStopWorkingClick(Sender: TObject);
    procedure TimerTehnicReloadTimer(Sender: TObject);
    procedure TimerCheckWiFiGPSTimer(Sender: TObject);
    procedure btnCheckWiFiGPSClick(Sender: TObject);
  private

    procedure LoadArtefacts;

    procedure LoadBag;
    procedure LoadCritical;
    procedure CheckExistsArmorPSI;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;

  public
    { Public declarations }
    FFrameMap: TFrameMap;
    FFramePercs: TFramePercs;
    FFrameDetector: TFrameDetector;
    FFrameQRScanner: TFrameQRScanner;
    FFrameIssuies: TFrameIssuies;
    FFrameBag: TFrameBag;
    FFrameLogin: TFrameLogin;
    FFrameSettings: TFrameSettings;
    procedure CreateFrameLogin;
    procedure StartApp;
    procedure LoadIssuies;
    procedure StopDetector;
    procedure CreateBagFrame;
    procedure LoadPlaces;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Permissions;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

   TThread.Synchronize(nil, procedure
     var
       vBitmapFog: TBitmap;
     begin
       FFrameMap.ScrollBox.ViewportPosition := TPointF.Create(FFrameMap.MapLayout.Width, FFrameMap.MapLayout.Height);

       if FileExists(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png')) then
         DeleteFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png'));

         vBitmapFog:= TBitmap.Create;
         try
           vBitmapFog.Assign(FSurfaceFog);
           vBitmapFog.SaveToFile(System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'fog.png')) ;
         finally
           vBitmapFog.Free;
         end;
     end);

     Sleep(1000);
end;

function TMainForm.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  if AAppEvent = TApplicationEvent.EnteredBackground then
  begin
    Close;
  end;
  Result := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService,
     IInterface(AppEventService)) then
  begin
    AppEventService.SetApplicationEventHandler(HandleAppEvent);
  end;

  labZombTimer.TextSettings.Font.Family := 'montblancctt';

{$IF Defined(ANDROID) or Defined(IOS)}
  Self.FullScreen := TOSVersion.Check(11);
  FIsSendMarkers := false;
  FKillType := ktLive;
  MediaPlayerMenu.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'menu.mp3');
  MediaPlayerZombi.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'zombi.mp3');
  MediaPlayerNotification.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'notification.mp3');
  MediaPlayerWorking.FileName := System.IOUtils.TPath.Combine(GetUserAppPath, 'work.mp3');
{$ENDIF}
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
var
  vQuery: TFDQuery;
  vDistance : integer;
begin
  if (FKillType = ktLive) and (not Person.Master) then
  begin
    if not FFrameMap.ScanBaseSafeDead and not layCheckWiFiGPS.Visible then
      ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [9, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]), exExecute, vQuery);
  end;
end;

procedure TMainForm.LoadBag;
var
  vQuery: TFDQuery;
  vBagData: TBagData;
begin
  if Assigned(FBagList) then
    FBagList.Clear
  else
    FBagList := TList<TBagData>.Create;

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

procedure TMainForm.LoadIssuies;
var
  vQuery: TFDQuery;
  vIssueList: TIssueData;
begin

  if Assigned(FIssueList) then
    begin
      FIssueList.Clear;
    end
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
  AFormatSettings: TFormatSettings;
begin
  AFormatSettings.DateSeparator := '.';
  AFormatSettings.TimeSeparator := ':';
  AFormatSettings.ShortDateFormat := 'DD.MM.YYYY';
  AFormatSettings.LongTimeFormat := 'hh:nn:ss';

  if Assigned(FCritical) then
    FCritical.Clear
  else
    FCritical := TList<TCritical>.Create;

  ExeExec('select * from critical_issuies;', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    FCriticalItem.Name := vQuery.FieldByName('name').AsString;
    FCriticalItem.DateTimeStart := StrToDateTime(vQuery.FieldByName('datetime_start').AsString, AFormatSettings);
    FCriticalItem.DateTimeStop := StrToDateTime(vQuery.FieldByName('datetime_stop').AsString, AFormatSettings);
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

  ExeExec('select * from places where '','' || fractions ||'','' like ''%,'' || ''' + Person.GroupId.ToString +''' ||'',%'';', exActive, vQuery);
  try
    vQuery.First;

    while Not vQuery.Eof do
    begin
      vPlaceData.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
      vPlaceData.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
      vPlaceData.Name := vQuery.FieldByName('name').AsString;
      vPlaceData.Radius := vQuery.FieldByName('radius').AsInteger;
      vPlaceData.Fractions := vQuery.FieldByName('fractions').AsString;

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
  StartScanWiFi;
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
  LoadIssuies;
  LoadPlaces;
  LoadCritical;

  if Assigned(FFrameLogin) then
    FFrameLogin.Visible := false;

  TabControl.Repaint;

  TThread.Synchronize(nil,
  procedure
  begin
    if Assigned(FFrameMap) then
      FreeAndNil(FFrameMap);

    FFrameMap := TFrameMap.Create(TabMap);
    FFrameMap.Parent := TabMap;
    FFrameMap.timerCheckCritical.Enabled := true;
    FFrameMap.FDataUpdated := true;
  end);

  if Assigned(FFrameSettings) then
    FreeAndNil(FFrameSettings);

  FFrameSettings := TFrameSettings.Create(TabSettings);
  FFrameSettings.Parent := TabSettings;

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

  TimerUpdateData.Enabled := true;

  CheckExistsArmorPSI;

  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE',
                                         'android.permission.CHANGE_WIFI_STATE',
                                         'android.permission.ACCESS_FINE_LOCATION',
                                         'android.permission.NEARBY_WIFI_DEVICES',
                                         'android.permission.CHANGE_WIFI_MULTICAST_STATE',
                                         'android.permission.ACCESS_BACKGROUND_LOCATION',
                                         'android.permission.FOREGROUND_SERVICE',
                                         'android.permission.ACCESS_COARSE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
        begin
          {$IFDEF ANDROID}
            FFrameMap.LocationServiceChanged;
          {$ENDIF}
          FFrameMap.TimerSensor.Enabled := true;
        end
      else
        begin
          ShowMessage('Необходимы разрешения для сканирования Wi-Fi');
        end;
    end);
  {$ENDIF}
end;

procedure TMainForm.IdTCPServerExecute(AContext: TIdContext);
var
  vAnswerText: string;
  vSell: TSell;
  FDQuery: TFDQuery;
  vTehnic: TTehnicData;
  vConnectPerson: TPerson;
  vCostRestore: integer;
  vIsCorrectLevel : boolean;
  vSum: integer;

  procedure SendCorrectRestore;
  begin
    ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [11, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]), exExecute, FDQuery);
    FFramePercs.StartTimerTehnicPercReload(NOW());
    Person.Cash := Person.Cash + vCostRestore;

    AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFramePercs.FActiveAction), IndyUTF8Encoding(true));
    AContext.Connection.Disconnect;
  end;

  procedure SendInvalidLevelTehnic;
  begin
    FFramePercs.FActiveAction.SendType := stIncorrectLevelTehnic;
    FFramePercs.FActiveAction.JSONObject := '0';
    AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFramePercs.FActiveAction), IndyUTF8Encoding(true));
    AContext.Connection.Disconnect;
  end;
begin
  vAnswerText := AContext.Connection.Socket.ReadLn();
  vConnectPerson := TJSON.JsonToObject<TPerson>(vAnswerText);

  if Assigned(FFrameBag) and Assigned(FFrameBag.FActiveAction) then
    try
      case FFrameBag.FActiveAction.SendType of
        stSell:
          begin
            vSell := TSell.Create;

            try
              vSell := TJSON.JsonToObject<TSell>(FFrameBag.FActiveAction.JSONObject);

              if vConnectPerson.Cash - vSell.Cost >= 0 then
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
            finally
              vSell.Free;
            end;
          end;
      end;
    finally
      ReloadBag;
    end;

    if  Assigned(FFramePercs) and Assigned(FFramePercs.FActiveAction) then
    try
      case FFramePercs.FActiveAction.SendType of
        stMedic:     // Medic
          begin
            ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [10, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]), exExecute, FDQuery);
            FFramePercs.StartTimerMedicPercReload(NOW());

            AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFramePercs.FActiveAction), IndyUTF8Encoding(true));
            AContext.Connection.Disconnect;
          end;

        stTehnic:     // Tehnic
          begin
            vTehnic := TJSON.JsonToObject<TTehnicData>(FFramePercs.FActiveAction.JSONObject);

            if not vConnectPerson.IsDead then
            begin
              if vConnectPerson.IsRestoreWeapon then
              begin
                ExeExec(Format('select round(%s * cost) as cost from weapons where weapon_id = %d', [StringReplace((1 - vConnectPerson.WeaponHealth / 100).ToString, ',', '.', [rfReplaceAll]), vConnectPerson.WeaponId]), exActive, FDQuery);
                  try
                    vCostRestore := FDQuery.FieldByName('cost').AsInteger;
                  finally
                    FreeQueryAndConn(FDQuery);
                  end;

                vIsCorrectLevel := Person.LevelTehnic >= vConnectPerson.WeaponLevel;
              end
              else
                begin
                  ExeExec(Format('select round(%s * cost) as cost from armors where armor_id = %d', [StringReplace((1 - vConnectPerson.ArmorHealth / 100).ToString, ',', '.', [rfReplaceAll]), vConnectPerson.ArmorId]), exActive, FDQuery);
                  try
                    vCostRestore := FDQuery.FieldByName('cost').AsInteger;
                  finally
                    FreeQueryAndConn(FDQuery);
                  end;

                  vIsCorrectLevel := Person.LevelTehnic = 3;
                end;

              if vTehnic.IsFree then
                vCostRestore := 0;

              vSum := vConnectPerson.Cash - vCostRestore;
              if vSum >= 0 then
                begin
                  if vIsCorrectLevel then
                    SendCorrectRestore
                  else
                    SendInvalidLevelTehnic;
                end
              else
                begin
                  FFramePercs.FActiveAction.SendType := stCancelSell;
                  FFramePercs.FActiveAction.JSONObject := '0';
                  AContext.Connection.Socket.WriteLn(TJSON.ObjectToJsonString(FFramePercs.FActiveAction), IndyUTF8Encoding(true));
                  AContext.Connection.Disconnect;
                end;
            end
            else
              SendCorrectRestore;
          end;
      end;

    finally
      FreeAndNil(FFramePercs.FActiveAction);
      FFramePercs.layQR.Visible := false;
    end;
end;

procedure TMainForm.btnCheckWiFiGPSClick(Sender: TObject);
begin
  FFrameMap.TimerSensor.Enabled := true;
  layCheckWiFiGPS.Visible := false;
  TimerCheckWiFiGPS.Enabled := false;
end;

procedure TMainForm.btnKillClick(Sender: TObject);
begin
  MessageDlg('Ты умер в бою?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
      begin
        MediaPlayerZombi.CurrentTime := 0;
        MediaPlayerZombi.Stop;
        SetMediaVolume(100);
        TimerZombi.Enabled := false;
        FKillType := ktWeapon;
        layZombTimer.Visible := false;
        Person.IsDead := false;
        Person.Health := 0;
      end;
    end);
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  BtnClickMedia;
  TabControl.ActiveTab := TabSettings;
  recSelect.Parent := ImgSettings;
  StopDetector;
  layPersonHealth.Visible := false;
end;

procedure TMainForm.btnStopWorkingClick(Sender: TObject);
begin
  MessageDlg('Ты согласен прервать ремонт?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
      begin
        MediaPlayerWorking.CurrentTime := 0;
        MediaPlayerWorking.Stop;
        TimerTehnicReload.Enabled := false;
        layTehnicReload.Visible := false;
      end;
    end);
end;

procedure TMainForm.btnToBagClick(Sender: TObject);
begin
  BtnClickMedia;
  LoadBag;
  recSelect.Parent := imgBtnBag;

  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  layPersonHealth.Visible := false;
  CreateBagFrame;
end;

procedure TMainForm.CreateBagFrame;
begin
  TTask.Run(procedure
  begin
    TabBag.Visible := false;

    try
    while Assigned(FFrameBag) do
      begin
        FFrameBag.Parent := nil;
        FFrameBag.Visible := false;

        while Assigned(FFrameBag) do
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

      TThread.Synchronize(nil,
      procedure
      begin
        TabControl.ActiveTab := TabBag;
      end);
    finally
      TabBag.Visible := true;
    end;

  end);

end;

procedure TMainForm.btnToIssuiesClick(Sender: TObject);
begin
  BtnClickMedia;
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
  animNotification.Enabled := false;
  recNotification.Opacity := 0;
end;

procedure TMainForm.btnToMapClick(Sender: TObject);
begin
  BtnClickMedia;
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
  BtnClickMedia;

  if Self.Width <= 680 then
  begin
    FFramePercs.layLeftBlock.Scale.X := 0.9;
    FFramePercs.layLeftBlock.Scale.Y := 0.9;
  end
  else if Self.Width <= 500 then
  begin
    FFramePercs.layLeftBlock.Scale.X := 0.8;
    FFramePercs.layLeftBlock.Scale.Y := 0.8;
  end;

  SetHealthProgress(FFramePercs.HealthProgress, Person.Health);
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;
  FFrameQRScanner.StopScan;
  StopDetector;
  layPersonHealth.Visible := false;
  Person.GroupId := Person.GroupId;
  Person.ArmorHealth := Person.ArmorHealth;
  Person.WeaponHealth := Person.WeaponHealth;
  FFramePercs.layPercsUp.Visible := false;
  FFramePercs.laySelection.Visible := false;
end;

procedure TMainForm.btnToQRScannerClick(Sender: TObject);
begin
  BtnClickMedia;
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

procedure TMainForm.TimerCheckWiFiGPSTimer(Sender: TObject);
var
  Seconds: Integer;
  TotalSeconds: Integer;
begin
  if labCheckWifiGPSSeconds.Text = '00' then
  begin
    TimerCheckWiFiGPS.Enabled := false;
    layCheckWiFiGPS.Visible := false;
    SetMediaVolume(FVolume);

    FKillType := ktPSI;
    Person.Health := 0;
  end
  else
  begin
    Seconds := labCheckWifiGPSSeconds.Text.ToInteger;

    // уменьшаем на 1
    TotalSeconds := Seconds - 1;

    // Проверяем, не истекло ли время
    if TotalSeconds >= 0 then
    begin
      labCheckWifiGPSSeconds.Text := Format('%.2d', [TotalSeconds]);
    end;
  end;
end;

procedure TMainForm.timerScannerWifiMerchantTimer(Sender: TObject);
begin
{$IFDEF ANDROID}
  TTask.Run(
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
          end);
      end;
    end);

{$ENDIF}
end;

procedure TMainForm.TimerTehnicReloadTimer(Sender: TObject);
var
  TimeParts: TArray<string>;
  Minutes, Seconds: Integer;
  TotalSeconds: Integer;
begin

  if labReloadTimerTehnic.Text = '00:00' then
  begin
    MediaPlayerWorking.CurrentTime := 0;
    MediaPlayerWorking.Stop;
    TimerTehnicReload.Enabled := false;
    layTehnicReload.Visible := false;
    SetMediaVolume(FVolume);

    if FFramePercs.FIsMyRestore then
      FFramePercs.FinishMyRestore
    else
      FFramePercs.ShowQRTehnic;
  end
  else
  begin
    FFrameMap.timerCritical.Enabled := false;
    FFrameMap.MediaPlayerNotificationCritical.Stop;
    FFrameMap.MediaPlayerNotificationCritical.CurrentTime := 0;
    FFrameMap.MediaPlayerStartCritical.Stop;
    FFrameMap.MediaPlayerStartCritical.CurrentTime := 0;

    SetMediaVolume(100);
    MediaPlayerWorking.Play;

    if MediaPlayerWorking.CurrentTime = MediaPlayerWorking.Duration then
      MediaPlayerWorking.CurrentTime := 0;
    // Разбиваем текущее время из Label на часы, минуты, секунды
    TimeParts := labReloadTimerTehnic.Text.Split([':']);

    if Length(TimeParts) = 2 then
    begin
      Minutes := StrToIntDef(TimeParts[0], 0);
      Seconds := StrToIntDef(TimeParts[1], 0);

      // Переводим всё в секунды и уменьшаем на 1
      TotalSeconds := Minutes * 60 + Seconds - 1;

      // Проверяем, не истекло ли время
      if TotalSeconds >= 0 then
      begin
        // Преобразуем обратно в часы, минуты, секунды

        Minutes := (TotalSeconds mod 3600) div 60;
        Seconds := TotalSeconds mod 60;

        labReloadTimerTehnic.Text := Format('%.2d:%.2d', [Minutes, Seconds]);
      end;
    end;
  end;
end;

procedure TMainForm.TimerUpdateDataTimer(Sender: TObject);
begin
  if FFrameMap.FDataUpdated and FFrameMap.ScanBaseSafeDead then
    begin
      FFrameMap.FDataUpdated := false;
      StartScanWiFi;

    TTask.Run(
      procedure
      var
        vAnswer: TAnswer;
        vData: TData;
        vUserdata: TUserdata;
        vSQL: Unicodestring;
        vQuery: TFDQuery;
        I: integer;
      begin
        try
          vData := TData.Create;
          vData.SQL := TList<Unicodestring>.Create;
          vData.SQL := GoGenericBaseData();

          vUserdata := TUserdata.Create;
          vUserdata.UserId := Person.UserId;
          vUserdata.Data := vData;

          vAnswer := TJSON.JsonToObject<TAnswer>(PostDataServer('api/update_data', TJSON.ObjectToJsonString(vUserdata)));

          if Assigned(vAnswer) then
          begin
            try
              if vAnswer.Status = 'success' then
                begin
                  ExeExec('delete from life_log where action_date_time <> (select max(l.action_date_time) from life_log l where l.action_type_id = life_log.action_type_id); delete from notifications where is_owner = true;', exExecute, vQuery);
                end;

                vData := TJSON.JsonToObject<TData>(vAnswer.Json);

                if vData.SQL.Count > 0 then
                begin
                  for I := 0 to vData.SQL.Count - 1 do
                    vSQL := vSQL + vData.SQL[I];

                    ExeExec(vSQL, exExecute, vQuery);
                end;

                ReloadNotificationData;
                ReloadIssuies;
              finally
                FreeAndNil(vAnswer);
              end;
            end;
        finally
          FFrameMap.FDataUpdated := true;
          FreeAndNil(vData);
          FreeAndNil(vUserdata);

          if (TabControl.ActiveTab <> TabBag) and (TabControl.ActiveTab <> TabQRScanner) then
            DisconnectNetworks;
        end;
      end);
    end;
end;

procedure TMainForm.TimerZombiTimer(Sender: TObject);
var
  TimeParts: TArray<string>;
  Minutes, Seconds: Integer;
  TotalSeconds: Integer;
begin
  if labZombTimer.Text = '00:00' then
  begin
    MediaPlayerZombi.CurrentTime := 0;
    MediaPlayerZombi.Stop;
    TimerZombi.Enabled := false;
    FKillType := ktStopZombi;
    layZombTimer.Visible := false;
    SetMediaVolume(FVolume);
  end
  else
  begin
    SetMediaVolume(100);
    MediaPlayerZombi.Play;

    if MediaPlayerZombi.CurrentTime = MediaPlayerZombi.Duration then
      MediaPlayerZombi.CurrentTime := 0;
    // Разбиваем текущее время из Label на часы, минуты, секунды
    TimeParts := labZombTimer.Text.Split([':']);

    if Length(TimeParts) = 2 then
    begin
      Minutes := StrToIntDef(TimeParts[0], 0);
      Seconds := StrToIntDef(TimeParts[1], 0);

      // Переводим всё в секунды и уменьшаем на 1
      TotalSeconds := Minutes * 60 + Seconds - 1;

      // Проверяем, не истекло ли время
      if TotalSeconds >= 0 then
      begin
        // Преобразуем обратно в часы, минуты, секунды
        Minutes := (TotalSeconds mod 3600) div 60;
        Seconds := TotalSeconds mod 60;

        labZombTimer.Text := Format('%.2d:%.2d', [Minutes, Seconds]);
      end;
    end;
  end;
end;

procedure TMainForm.CheckExistsArmorPSI;
var
  AFormatSettings: TFormatSettings;
  vQuery: TFDQuery;
  vLastActionDateTime: TDateTime;
  vActionTypeID : integer;
  Minutes: integer;
  Seconds: integer;
begin
  AFormatSettings.DateSeparator := '.';
  AFormatSettings.TimeSeparator := ':';
  AFormatSettings.ShortDateFormat := 'DD.MM.YYYY';
  AFormatSettings.LongTimeFormat := 'hh:nn:ss';

  ExeExec('select strftime(''%d.%m.%Y %H:%M:%S'',action_date_time) as action_date_time from life_log where action_date_time = (select max(action_date_time) from life_log where action_type_id = 12);', exActive, vQuery);
   try
     if vQuery.RecordCount > 0 then
     begin
       vLastActionDateTime := StrToDateTime(vQuery.FieldByName('action_date_time').AsString, AFormatSettings);

       if (SecondsBetween(NOW(), vLastActionDateTime) < 3600) then
         begin
           Minutes := (3600 - SecondsBetween(NOW(), vLastActionDateTime) mod 3600) div 60;
           Seconds := 3600 - SecondsBetween(NOW(), vLastActionDateTime) mod 60;
           StartArmorPSI(Format('%.2d:%.2d', [Minutes, Seconds]));
         end;
     end;
   finally
     FreeQueryAndConn(vQuery);
   end;

end;

end.
