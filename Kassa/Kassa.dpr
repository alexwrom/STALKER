program Kassa;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  DelphiZXIngQRCode in '..\..\ZXing.Delphi-3.10.0\DelphiZXingQRCode-master\Source\DelphiZXIngQRCode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
