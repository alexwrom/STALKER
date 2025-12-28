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
  uFrameQRScaner, uFrameIssuies, uFrameBag, uFrameBagSection,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, // Для JString
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.Provider,
  Androidapi.Helpers, // Для StringToJString и JStringToString
  Androidapi.JNI.Os,
  FMX.Platform.Android,
{$ENDIF}
  uScanerWiFi, FMX.Ani, FMX.Effects;

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
    Image6: TImage;
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
    procedure FormCreate(Sender: TObject);
    procedure btnToMapClick(Sender: TObject);
    procedure btnToPercsClick(Sender: TObject);
    procedure btnToBagClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnToQRScannerClick(Sender: TObject);
    procedure btnToIssuiesClick(Sender: TObject);
  private

    procedure LoadArtefacts;
    procedure LoadPlaces;
    procedure LoadBag;

  public
    { Public declarations }
    FFrameMap: TFrameMap;
    FFramePercs: TFramePercs;
    FFrameDetector: TFrameDetector;
    FFrameQRScanner: TFrameQRScanner;
    FFrameIssuies: TFrameIssuies;
    FFrameBag: TFrameBag;
    FFrameBagSection: TFrameBagSection;
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Self.FullScreen := true;
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
    FBagList.Add(vBagData);
    vQuery.Next;
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

  ExeExec('select * from open_issuies where user_id = ' + Person.UserId.ToString + ';', exActive, vQuery);
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
  ExeExec('select * from arts;', exActive, vQuery);
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
begin
  Person := TPerson.Create;
  Person.UserId := 1; // Получить ID при логировании
  Person.GroupId := 1;
  Person.CountContener := -1;

  LoadArtefacts;
  LoadIsuies;
  LoadPlaces;

  FFrameMap := TFrameMap.Create(TabMap);
  FFrameMap.Parent := TabMap;

  FFramePercs := TFramePercs.Create(TabPercs);
  FFramePercs.Parent := TabPercs;

  FFrameDetector := TFrameDetector.Create(TabDetector);
  FFrameDetector.Parent := TabDetector;

  FFrameQRScanner := TFrameQRScanner.Create(TabQRScanner);
  FFrameQRScanner.Parent := TabQRScanner;

  FFrameIssuies := TFrameIssuies.Create(TabIssueis);
  FFrameIssuies.Parent := TabIssueis;

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

procedure TMainForm.btnToBagClick(Sender: TObject);
begin
  LoadBag;
  recSelect.Parent := imgBtnBag;
  TabControl.ActiveTab := TabBag;
  FFrameDetector.timerScannerArtefacts.Enabled := false;
  FFrameDetector.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := false;

  CreateBagFrame;
end;

procedure TMainForm.CreateBagFrame;
begin
  if Person.IsClassicBag then
  begin
    if not Assigned(FFrameBag) then
    begin
      FFrameBag := TFrameBag.Create(TabBag);
      FFrameBag.labCash.Text := Person.Cash.ToString;
      FFrameBag.LayBag.Height := Self.Height + 63;
    end;
    FFrameBag.CreateElements;
    FFrameBag.Parent := TabBag;
    FFrameBag.SwitchStyle.IsChecked := true;
    FFrameBag.BringToFront;
  end
  else
  begin
    if not Assigned(FFrameBagSection) then
    begin
      FFrameBagSection := TFrameBagSection.Create(TabBag);
      FFrameBagSection.labCash.Text := Person.Cash.ToString;
    end;

    FFrameBagSection.LoadBagElements;
    FFrameBagSection.SwitchStyle.IsChecked := false;
    FFrameBagSection.Parent := TabBag;
    FFrameBagSection.BringToFront;
  end;

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

end.
