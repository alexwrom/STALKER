program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uMapFrame in 'uMapFrame.pas' {MapFrame: TFrame},
  uGlobal in 'uGlobal.pas',
  uPercs in 'uPercs.pas' {FramePercs: TFrame},
  uDetectorFrame in 'uDetectorFrame.pas' {FrameDetector: TFrame},
  uScanerWiFi in 'uScanerWiFi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
