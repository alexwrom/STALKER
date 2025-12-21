unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, System.Sensors,
  System.Sensors.Components, Math, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, uMapFrame, FMX.Objects, uPercs, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, uGlobal, uDetectorFrame,
  uQRScanerFrame,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, // Для JString
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.Provider,
  Androidapi.Helpers, // Для StringToJString и JStringToString
  Androidapi.JNI.Os,
  FMX.Platform.Android,
{$ENDIF}
  uScanerWiFi;

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
    imgBtnQRScanner: TImage;
    Image6: TImage;
    Image7: TImage;
    Image9: TImage;
    Image12: TImage;
    Image14: TImage;
    Image15: TImage;
    btnToMap: TSpeedButton;
    btnToPercs: TSpeedButton;
    btnToBag: TSpeedButton;
    btnToQRScanner: TSpeedButton;
    TabDetector: TTabItem;
    TabQRScanner: TTabItem;
    imgPersonHealth: TImage;
    HealthProgress: TRectangle;
    recSelect: TRectangle;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnToMapClick(Sender: TObject);
    procedure btnToPercsClick(Sender: TObject);
    procedure btnToBagClick(Sender: TObject);
    procedure btnToQRScannerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

    procedure LoadArtefacts;
    procedure LoadPlaces;

  public
    { Public declarations }
    FMapFrame: TMapFrame;
    FPercsFrame: TFramePercs;
    FDetectorFrame: TFrameDetector;
    FFrameQRScanner: TFrameQRScanner;
    procedure LoadIsuies;
    procedure StopDetector;
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
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Person := TPerson.Create;
  Person.UserId := 1; // Получить ID при логировании

  LoadArtefacts;
  LoadIsuies;
  LoadPlaces;

  FMapFrame := TMapFrame.Create(TabMap);
  FMapFrame.Parent := TabMap;

  FPercsFrame := TFramePercs.Create(TabPercs);
  FPercsFrame.Parent := TabPercs;

  FDetectorFrame := TFrameDetector.Create(TabDetector);
  FDetectorFrame.Parent := TabDetector;

  FFrameQRScanner := TFrameQRScanner.Create(TabQRScanner);
  FFrameQRScanner.Parent := TabQRScanner;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION',
    'android.permission.CHANGE_WIFI_STATE', 'android.permission.CAMERA'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        FMapFrame.LocationSensor.Active := true;
      end
      else
      begin
        Showmessage('Необходимы разрешения для сканирования Wi-Fi');
      end;
    end);
end;

procedure TMainForm.btnToBagClick(Sender: TObject);
begin
  recSelect.Parent := imgBtnBag;
  FDetectorFrame.timerScannerArtefacts.Enabled := false;
  FDetectorFrame.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := true;
end;

procedure TMainForm.btnToQRScannerClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabQRScanner;
  recSelect.Parent := imgBtnQRScanner;
  FFrameQRScanner.StartScan;
  StopDetector;
  imgPersonHealth.Visible := true;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FLocation.Latitude := 52.091127;
  FLocation.Longitude := 23.709416;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  FLocation.Latitude := 52.090365;
  FLocation.Longitude := 23.707528;
end;

procedure TMainForm.btnToMapClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabMap;
  recSelect.Parent := imgBtnMap;
  FDetectorFrame.timerScannerArtefacts.Enabled := false;
  FDetectorFrame.TimerSensor.Enabled := false;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := true;
end;

procedure TMainForm.btnToPercsClick(Sender: TObject);
begin
  SetHealthProgress(FPercsFrame.HealthProgress, Person.Health);
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;
  FFrameQRScanner.StopScan;
  StopDetector;
  imgPersonHealth.Visible := false;
end;

procedure TMainForm.StopDetector;
var
  I: Integer;
begin
  if FDetectorFrame.timerScannerArtefacts.Enabled then
    for I := 0 to 2 do
    begin
      sleep(100);
      FDetectorFrame.timerScannerArtefacts.Enabled := false;
      FDetectorFrame.TimerSensor.Enabled := false;
    end;
end;

end.
