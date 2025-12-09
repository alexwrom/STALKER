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
    lblStatus: TLabel;
    btnScan: TButton;
    ListBox1: TListBox;
    Timer1: TTimer;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    TabControl: TTabControl;
    TabItem1: TTabItem;
    TabMap: TTabItem;
    TabPercs: TTabItem;
    Image8: TImage;
    Image11: TImage;
    Image13: TImage;
    Image10: TImage;
    Image1: TImage;
    HealthProgress: TRectangle;
    FDConn: TFDConnection;
    FDQuery: TFDQuery;
    Layout4: TLayout;
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
  private
    FMapFrame: TMapFrame;
    FPercsFrame: TFramePercs;
    FDetectorFrame: TFrameDetector;
    procedure RequestBatteryOptimizationDisable;

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
  RequestBatteryOptimizationDisable;

  FNetworks := TList<TWiFiNetwork>.Create;
  lblStatus.Text := 'Готов к сканированию Wi-Fi сетей';

{$IF Defined(ANDROID) or Defined(IOS)}
  ListBox1.ItemHeight := 80;
  btnScan.Height := 50;
  btnScan.TextSettings.Font.Size := 16;
  lblStatus.Font.Size := 14;
  Self.FullScreen := true;
{$ENDIF}
  FMapFrame := TMapFrame.Create(TabMap);
  FMapFrame.Parent := TabMap;

  FPercsFrame := TFramePercs.Create(TabPercs);
  FPercsFrame.Parent := TabPercs;

  FDetectorFrame := TFrameDetector.Create(TabDetector);
  FDetectorFrame.Parent := TabDetector;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        FMapFrame.LocationSensor.Active := true;
        FMapFrame.LocationSensorSecond.Active := true;
        FMapFrame.LocationSensorThird.Active := true;
      end
      else
      begin
        // lblStatus.Text := 'Необходимы разрешения для сканирования Wi-Fi';
      end;
    end);
end;

procedure TMainForm.RequestBatteryOptimizationDisable;
var
  Intent: JIntent;
  Uri: Jnet_Uri;
begin
  // Самый надежный способ - открыть настройки приложения
  Intent := TJIntent.Create;

  // Формируем URI для нашего пакета
  Uri := TJnet_Uri.JavaClass.parse(
    StringToJString('package:' +
      JStringToString(TAndroidHelper.Context.getPackageName))
  );

  // ACTION_APPLICATION_DETAILS_SETTINGS открывает детальные настройки
  Intent.setAction(
    StringToJString('android.settings.APPLICATION_DETAILS_SETTINGS')
  );

  Intent.setData(Uri);
  Intent.addCategory(StringToJString('android.intent.category.DEFAULT'));

  TAndroidHelper.Activity.startActivity(Intent);
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
begin
  TabControl.ActiveTab := TabPercs;
  recSelect.Parent := ImgBtnPercs;
  FDetectorFrame.timerScannerArtefacts.Enabled := false;
  FDetectorFrame.TimerSensor.Enabled := false;
end;

end.
