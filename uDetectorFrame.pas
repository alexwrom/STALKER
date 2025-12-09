unit uDetectorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Ani, FMX.Media, uScanerWiFi,
  FMX.Controls.Presentation,  System.IOUtils, uGlobal;

type
  TFrameDetector = class(TFrame)
    Image9: TImage;
    imgDetector: TImage;
    recSignalOtklik: TRectangle;
    animDetectorOtklik: TFloatAnimation;
    TimerSensor: TTimer;
    MediaPlayer: TMediaPlayer;
    recBackForIndicator: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    FloatAnimation1: TFloatAnimation;
    timerScannerArtefacts: TTimer;
    Label1: TLabel;
    procedure TimerSensorTimer(Sender: TObject);
    procedure timerScannerArtefactsTimer(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AObject: TFmxObject);
    { Public declarations }
  end;

implementation

uses
  System.Permissions;
{$R *.fmx}

constructor TFrameDetector.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  animDetectorOtklik.Start;
  MediaPlayer.FileName := TPath.Combine(GetUserAppPath, 'detector.mp3');
end;

procedure TFrameDetector.TimerSensorTimer(Sender: TObject);
begin
  MediaPlayer.CurrentTime := 0;
  animDetectorOtklik.Start;
  FloatAnimation1.Start;
  MediaPlayer.Volume := 100;
  MediaPlayer.Play;
end;

procedure TFrameDetector.timerScannerArtefactsTimer(Sender: TObject);
begin
  // Запрос разрешений для Android
{$IFDEF ANDROID}
  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        TThread.CreateAnonymousThread(
          procedure
          begin
            ScanNetworks;

            TThread.Synchronize(nil,
              procedure
              var
                vScanArtDistance: integer;
              begin
                vScanArtDistance := Round(ScanDistanceToArtefacts(1));

                label1.Text := vScanArtDistance.ToString;

                if vScanArtDistance < 9999 then
                begin
                  case vScanArtDistance of
                    0 .. 2:
                      TimerSensor.Interval := 200;
                    3 .. 5:
                      TimerSensor.Interval := 1000;
                    6 .. 15:
                      TimerSensor.Interval := 2000;
                  end;
                  TimerSensor.Enabled := true;
                end
                else
                  TimerSensor.Enabled := false;
              end);
          end).Start;
      end
      else
      begin
         label1.Text := 'Необходимы разрешения для сканирования Wi-Fi';
      end;
    end);
{$ENDIF}
end;

end.
