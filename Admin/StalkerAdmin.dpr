program StalkerAdmin;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uGlobal in 'uGlobal.pas',
  classes.action in '..\classes\classes.action.pas',
  uGenericBaseData in 'uGenericBaseData.pas',
  DelphiZXIngQRCode in '..\..\ZXing.Delphi-3.10.0\DelphiZXingQRCode-master\Source\DelphiZXIngQRCode.pas',
  classes.send in '..\classes\classes.send.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
