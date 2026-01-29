program ServerYouClicker;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uApiServer in 'units\uApiServer.pas',
  uGlobal in 'units\uGlobal.pas',
  uGenericBaseData in 'units\uGenericBaseData.pas',
  classes.data in '..\classes\classes.data.pas',
  classes.user in '..\classes\classes.user.pas',
  classes.userdata in '..\classes\classes.userdata.pas';

var
  ApiServer: TApiServer;

begin
  try
    WriteLn('S.T.A.L.K.E.R. API server');

    ApiServer := TApiServer.Create;
    try
      ApiServer.Start(2026); // Запускаем сервер на порту 2026

      FullData := GoGenericBaseData(-1);
      WriteLn('Full data is loaded');
      AdminData := GoGenericBaseData(-1, True);
      WriteLn('Admin data is loaded');
      ReadLn;
    finally
      ApiServer.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
