unit uDetectorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Ani, FMX.Media, uScanerWiFi,
  FMX.Controls.Presentation, System.IOUtils, uGlobal, FMX.Layouts, Generics.Collections;

type
  TFrameDetector = class(TFrame)
    Image9: TImage;
    imgDetector: TImage;
    recSignalOtklik: TRectangle;
    animDetectorOtklik: TFloatAnimation;
    TimerSensor: TTimer;
    MediaPlayer: TMediaPlayer;
    recBackForIndicator: TRectangle;
    animLightOtklick: TFloatAnimation;
    timerScannerArtefacts: TTimer;
    GlowEffect1: TGlowEffect;
    layOtklik: TLayout;
    layBear: TLayout;
    Image1: TImage;
    GlowEffect2: TGlowEffect;
    pieBearSignal: TPie;
    cicleBackBear: TCircle;
    animDetectorBear: TFloatAnimation;
    animLightBear: TFloatAnimation;
    layVilka: TLayout;
    imgDetectorVilka: TImage;
    recBackForIndicatorVilka: TRectangle;
    labDisplayVilka: TLabel;
    procedure TimerSensorTimer(Sender: TObject);
    procedure timerScannerArtefactsTimer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FArtDistance: integer;
    FKoeff: TPointF;
    function ScanDistanceToArtefacts: double;
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
  FKoeff.Y := 0.2566;
  FKoeff.X := 0.5210;

  case Person.Detector.Level of
    1:
      begin
        layOtklik.Visible := true;
        layBear.Visible := false;
        layVilka.Visible := false;
      end;
    2:
      begin
        layOtklik.Visible := false;
        layBear.Visible := true;
        layVilka.Visible := false;
      end;
    3:
      begin
        layVilka.Visible := true;
        layBear.Visible := false;
        layOtklik.Visible := false;
      end;
  end;
{$IFDEF ANDROID}
  MediaPlayer.FileName := TPath.Combine(GetUserAppPath, 'detector_art.mp3');
  labDisplayVilka.TextSettings.Font.Family := 'lcd';
{$ENDIF}
end;

procedure TFrameDetector.TimerSensorTimer(Sender: TObject);
begin
  if Not FIsDead then
  begin
    FrameResize(nil);

    if FArtDistance <= Person.Detector.Radius then
    begin
      case Person.Detector.Level of
        1:
          begin
            MediaPlayer.CurrentTime := 0;
            animDetectorOtklik.Start;
            animLightOtklick.Start;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
        2:
          begin

            case FArtDistance of
              0 .. 2:
                begin
                  pieBearSignal.EndAngle := 360;
                  pieBearSignal.StartAngle := 0;
                end;
              3 .. 5:
                begin
                  pieBearSignal.EndAngle := -222;
                  pieBearSignal.StartAngle := 42;
                end;
              6 .. 9:
                begin
                  pieBearSignal.EndAngle := -186;
                  pieBearSignal.StartAngle := 6;
                end;
              10 .. 15:
                begin
                  pieBearSignal.EndAngle := -134;
                  pieBearSignal.StartAngle := -45;
                end;
            else
              begin
                pieBearSignal.EndAngle := -80;
                pieBearSignal.StartAngle := -99;
              end;
            end;

            MediaPlayer.CurrentTime := 0;
            animDetectorBear.Start;
            animLightBear.Start;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
        3:
          begin
            labDisplayVilka.Text := FArtDistance.ToString.PadLeft(4, '0');
            MediaPlayer.CurrentTime := 0;
            MediaPlayer.Volume := 100;
            MediaPlayer.Play;
          end;
      end;
    end;
  end;
end;

procedure TFrameDetector.FrameResize(Sender: TObject);
begin
  pieBearSignal.Height := Self.Height - 40;
  pieBearSignal.Width := pieBearSignal.Height;
  recBackForIndicatorVilka.Height := Self.Height * FKoeff.Y;
  recBackForIndicatorVilka.Width := recBackForIndicatorVilka.Height / FKoeff.X;
  recBackForIndicatorVilka.Margins.Left := 171 / (136 / recBackForIndicatorVilka.Height);
  recBackForIndicatorVilka.Margins.Bottom := 74 / (136 / recBackForIndicatorVilka.Height);
  labDisplayVilka.TextSettings.Font.Size := 120 / (136 / recBackForIndicatorVilka.Height);
end;

function TFrameDetector.ScanDistanceToArtefacts: double;
var
  I: integer;
  vDistance: double;
  vMinDistance: double;
begin
  vMinDistance := Person.Detector.Radius + 1;

  if FLocation.Latitude <> 0 then
    for I := 0 to FArtefactsList.Count - 1 do
    begin
      if FArtefactsList[I].Level <= Person.Detector.Level then
      begin
        vDistance := CalculateFastDistance(FLocation.Latitude, FLocation.Longitude, FArtefactsList[I].Coords.Latitude, FArtefactsList[I].Coords.Longitude);

        if (vDistance <= vMinDistance) then
          vMinDistance := vDistance;
      end;
    end;

  result := vMinDistance;
end;

procedure TFrameDetector.timerScannerArtefactsTimer(Sender: TObject);
begin
  // Запрос разрешений для Android
{$IFDEF ANDROID}
  if Not FIsDead then
  begin
    PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION', 'android.permission.ACCESS_COARSE_LOCATION'],
      procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
        begin
          TThread.CreateAnonymousThread(
            procedure
            begin
              // ScanNetworks;    // по WIFI

              TThread.Synchronize(nil,
                procedure
                begin
                  FArtDistance := Round(ScanDistanceToArtefacts);

                  if FArtDistance <= Person.Detector.Radius then
                  begin
                    case FArtDistance of
                      0 .. 2:
                        TimerSensor.Interval := 200;
                      3 .. 5:
                        TimerSensor.Interval := 800;
                      6 .. 9:
                        TimerSensor.Interval := 1500;
                      10 .. 15:
                        TimerSensor.Interval := 2200;
                    else
                      TimerSensor.Interval := 5000;
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
          // Label1.Text := 'Необходимы разрешения для сканирования Wi-Fi';
        end;
      end);
  end;
{$ENDIF}
end;

end.
