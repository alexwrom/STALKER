program StalkerAdmin;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uGlobal in 'uGlobal.pas',
  classes.action in '..\classes\classes.action.pas',
  DelphiZXIngQRCode in '..\..\ZXing.Delphi-3.10.0\DelphiZXingQRCode-master\Source\DelphiZXIngQRCode.pas',
  classes.send in '..\classes\classes.send.pas',
  controls.item in 'controls.item.pas',
  controls.listitem in 'controls.listitem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
