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
    Timer1: TTimer;
    TabControl: TTabControl;
    TabMap: TTabItem;
    TabPercs: TTabItem;
    Image8: TImage;
    Image11: TImage;
    Image13: TImage;
    Image10: TImage;
    imgPersonHealth: TImage;
    HealthProgress: TRectangle;
    FDConn: TFDConnection;
    FDQuery: TFDQuery;
    layMenu: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    imgBtnMap: TImage;
    ImgBtnPercs: TImage;
    imgBtnBag: TImage;
    imgBtnDetector: TImage;
    Image6: TImage;
    Image7: TImage;
    Image9: TImage;
    Image12: TImage;
    Image14: TImage;
    Image15: TImage;
    recSelect: TRectangle;
    btnToMap: TSpeedButton;
    btnToPercs: TSpeedButton;
    btnToBag: TSpeedButton;
    btnToDetector: TSpeedButton;
    TabDetector: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure btnToMapClick(Sender: TObject);
    procedure btnToPercsClick(Sender: TObject);
    procedure btnToBagClick(Sender: TObject);
    procedure btnToDetectorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMapFrame: TMapFrame;
    FPercsFrame: TFramePercs;
    FDetectorFrame: TFrameDetector;

  public
    { Public declarations }
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

procedure TMainForm.FormShow(Sender: TObject);
var
  vQuery: TFDQuery;
  vArtefactData: TArtefactData;
  vIssueList: TIssueData;
begin
  Person := TPerson.Create;
  Person.UserId := 1; // Получить ID при логировании
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

  FIssueList:= TList<TIssueData>.Create;;
  ExeExec('select i.* from user_issuies ui join issuies i on ui.issue_id = i.issue_id where ui.user_id = ' + Person.UserId.ToString + ';', exActive, vQuery);
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
    FIssueList.Add(vIssueList);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);

  FMapFrame := TMapFrame.Create(TabMap);
  FMapFrame.Parent := TabMap;

  FPercsFrame := TFramePercs.Create(TabPercs);
  FPercsFrame.Parent := TabPercs;

  FDetectorFrame := TFrameDetector.Create(TabDetector);
  FDetectorFrame.Parent := TabDetector;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION',
    'android.permission.CHANGE_WIFI_STATE'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        FMapFrame.LocationSensor.Active := true;
      end
      else
      begin
        // lblStatus.Text := 'Необходимы разрешения для сканирования Wi-Fi';
      end;
    end);
end;

procedure TMainForm.btnToBagClick(Sender: TObject);
begin
  recSelect.Parent := imgBtnBag;
  FDetectorFrame.timerScannerArtefacts.Enabled := false;
  FDetectorFrame.TimerSensor.Enabled := false;
end;

procedure TMainForm.btnToDetectorClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabDetector;
  recSelect.Parent := imgBtnDetector;
  FDetectorFrame.timerScannerArtefacts.Enabled := true;
end;

procedure TMainForm.btnToMapClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabMap;
  recSelect.Parent := imgBtnMap;
  FDetectorFrame.timerScannerArtefacts.Enabled := false;
  FDetectorFrame.TimerSensor.Enabled := false;
end;

procedure TMainForm.btnToPercsClick(Sender: TObject);
var
  I: Integer;
begin
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;

  for I := 0 to 2 do
  begin
    sleep(100);
    FDetectorFrame.timerScannerArtefacts.Enabled := false;
    FDetectorFrame.TimerSensor.Enabled := false;
  end;
end;

end.
