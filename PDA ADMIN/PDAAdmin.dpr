program PDAAdmin;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uGlobal in 'uGlobal.pas',
  uFrameMap in 'uFrameMap.pas' {FrameMap: TFrame},
  classes.action in '..\classes\classes.action.pas',
  uScanerWiFi in '..\uScanerWiFi.pas',
  controls.item in '..\controls.item.pas',
  controls.listitem in '..\controls.listitem.pas',
  uGenericBaseData in 'uGenericBaseData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
