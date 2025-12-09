unit uScanerWiFi;

interface

uses System.SysUtils, System.Sensors, uGlobal, System.Classes, Math,
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.ComObj, Winapi.ActiveX, System.RegularExpressions,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os,
  Androidapi.JNI.Net,
{$ENDIF}
{$IFDEF IOS}
  iOSapi.Foundation, iOSapi.CoreWLAN, Macapi.Helpers,
{$ENDIF}
  System.Sensors.Components, Generics.Collections;



function ScanDistanceToArtefacts(ALevel: integer): double;
procedure ScanNetworks;
{$IFDEF MSWINDOWS}
function ScanWindowsNetworks: TList<TWiFiNetwork>;
procedure ExecuteCommand(const Cmd, Params: string; Output: TStringList);
{$ENDIF}
{$IFDEF ANDROID}
function ScanAndroidNetworks: TList<TWiFiNetwork>;
function CalculateDistanceInMeters(Lat1, Lon1, Lat2, Lon2: double): double;
function CalculateWifiDistance(rssi: Integer; frequency: Integer = 2412): double;
{$ENDIF}
{$IFDEF IOS}
function ScanIOSNetworks: TList<TWiFiNetwork>;
{$ENDIF}

var
  FNetworks: TList<TWiFiNetwork>;

implementation

// Сканмруем ближайший артефакт и возвращаем к ниму дистанцию
function ScanDistanceToArtefacts(ALevel: integer): double;
var
  Net: TWiFiNetwork;
  vMinDist: double;
begin
    vMinDist := 0;

    for Net in FNetworks do
    begin
      vMinDist := MinValue([vMinDist, Net.Distance]);
    end;

    Result := vMinDist;
end;

procedure ScanNetworks;
var
  Networks: TList<TWiFiNetwork>;
begin
  Networks := nil;

  try
{$IFDEF MSWINDOWS}
    Networks := ScanWindowsNetworks;
{$ENDIF}
{$IFDEF ANDROID}
    Networks := ScanAndroidNetworks;
{$ENDIF}
{$IFDEF IOS}
    Networks := ScanIOSNetworks;
{$ENDIF}
    if Networks = nil then
      Networks := TList<TWiFiNetwork>.Create;

  except
    on E: Exception do
    begin
      if Networks = nil then
        Networks := TList<TWiFiNetwork>.Create;
    end;
  end;

  TThread.Synchronize(nil,
    procedure
    begin
      FNetworks.Free;
      FNetworks := Networks;
    end);
end;

{$IFDEF ANDROID}

function CalculateDistanceInMeters(Lat1, Lon1, Lat2, Lon2: double): double;
const
  EARTH_RADIUS_METERS = 6371000; // Радиус Земли в метрах
var
  dLat, dLon, a, c: double;
begin
  dLat := DegToRad(Lat2 - Lat1);
  dLon := DegToRad(Lon2 - Lon1);
  a := Sin(dLat / 2) * Sin(dLat / 2) + Cos(DegToRad(Lat1)) * Cos(DegToRad(Lat2)) * Sin(dLon / 2) * Sin(dLon / 2);
  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));
  Result := EARTH_RADIUS_METERS * c;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}

function ScanWindowsNetworks: TList<TWiFiNetwork>;
var
  Output: TStringList;
  i: Integer;
  Line, SSID: string;
  CurrentNetwork: TWiFiNetwork;
  InNetwork: Boolean;
begin
  Result := TList<TWiFiNetwork>.Create;
  Output := TStringList.Create;
  try
    // Запускаем netsh для получения списка Wi-Fi сетей
    TThread.Synchronize(nil,
      procedure
      begin
        ExecuteCommand('netsh', 'wlan show networks mode=bssid', Output);
      end);

    InNetwork := False;
    SSID := '';

    for i := 0 to Output.Count - 1 do
    begin
      Line := Output[i].Trim;

      if Line.StartsWith('SSID') and not Line.Contains('BSSID') then
      begin
        if InNetwork and (SSID <> '') then
        begin
          CurrentNetwork.SSID := SSID;
          Result.Add(CurrentNetwork);
        end;

        InNetwork := true;
        SSID := Line.Substring(Line.IndexOf(':') + 1).Trim;
        FillChar(CurrentNetwork, SizeOf(CurrentNetwork), 0);
      end
      else if Line.StartsWith('BSSID') then
      begin
        CurrentNetwork.BSSID := Copy(Line.Substring(Line.IndexOf(':') + 1).Trim, 1, 17);
      end
    end;

    // Добавляем последнюю сеть
    if InNetwork and (SSID <> '') then
    begin
      CurrentNetwork.SSID := SSID;
      Result.Add(CurrentNetwork);
    end;

  finally
    Output.Free;
  end;
end;

// Вспомогательная процедура для выполнения команд (добавьте в private секцию)
procedure ExecuteCommand(const Cmd, Params: string; Output: TStringList);
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Buffer: array [0 .. 255] of AnsiChar;
  BytesRead: DWORD;
  CommandLine: string;
begin
  Security.nLength := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := true;
  Security.lpSecurityDescriptor := nil;

  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    try
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdOutput := WritePipe;
      StartupInfo.hStdError := WritePipe;

      CommandLine := Cmd + ' ' + Params;

      if CreateProcess(nil, PChar(CommandLine), @Security, @Security, true, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
      begin
        CloseHandle(WritePipe);

        repeat
          BytesRead := 0;
          if ReadFile(ReadPipe, Buffer, 255, BytesRead, nil) and (BytesRead > 0) then
          begin
            Buffer[BytesRead] := #0;
            Output.Add(string(AnsiString(Buffer)));
          end;
        until BytesRead = 0;

        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
      end;
    finally
      CloseHandle(ReadPipe);
    end;
  end;
end;
{$ENDIF}
{$IFDEF ANDROID}

function CalculateWifiDistance(rssi: Integer; frequency: Integer = 2412): double;
const
  // Мощность передатчика на расстоянии 1 метр (зависит от устройства)
  MEASURED_POWER_2GHZ = -40; // Для 2.4 GHz
  MEASURED_POWER_5GHZ = -45; // Для 5 GHz
begin
  try
    // Определяем мощность передатчика в зависимости от частоты
    var
      measuredPower: Integer;
    if (frequency >= 2400) and (frequency <= 2500) then
      measuredPower := MEASURED_POWER_2GHZ // 2.4 GHz
    else if (frequency >= 4900) and (frequency <= 5900) then
      measuredPower := MEASURED_POWER_5GHZ // 5 GHz
    else
      measuredPower := -42; // По умолчанию

    // Коэффициент затухания (зависит от среды)
    var
      n: double;
    if frequency >= 5000 then
      n := 3.3 // 5 GHz быстрее затухает
    else
      n := 3.0; // 2.4 GHz

    // Формула расчета расстояния с учетом частоты
    Result := Power(10, (measuredPower - rssi) / (10 * n));

  except
    Result := -1; // Ошибка расчета
  end;
end;

function ScanAndroidNetworks: TList<TWiFiNetwork>;
var
  WiFiManager: JWifiManager;
  ScanResults: JList;
  i: Integer;
  Network: TWiFiNetwork;
  ScanResult: JScanResult;
  SSID, BSSID: string;
  Freq, rssi: Integer;
begin
  Result := TList<TWiFiNetwork>.Create;

  try
    WiFiManager := TJWifiManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE));

    if (WiFiManager <> nil) and WiFiManager.isWifiEnabled then
    begin
      WiFiManager.startScan;

      ScanResults := WiFiManager.getScanResults;

      if ScanResults <> nil then
      begin
        for i := 0 to ScanResults.Size - 1 do
        begin
          ScanResult := TJScanResult.Wrap(ScanResults.get(i));

          if ScanResult <> nil then
          begin
            SSID := JStringToString(ScanResult.SSID);
            BSSID := JStringToString(ScanResult.BSSID);
            Freq := ScanResult.frequency;
            rssi := ScanResult.level;
            // Пропускаем скрытые сети
            if (SSID = '') or (SSID = '<unknown ssid>') then
              Continue;

            Network.SSID := SSID;
            Network.BSSID := BSSID;
            Network.Distance := CalculateWifiDistance(rssi, Freq);
            Result.Add(Network);
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      // В случае ошибки возвращаем пустой список
    end;
  end;
end;
{$ENDIF}
{$IFDEF IOS}

function ScanIOSNetworks: TList<TWiFiNetwork>;
begin
  Result := TList<TWiFiNetwork>.Create;

  // На iOS прямой доступ к сканированию Wi-Fi ограничен
  // Можно получить только информацию о текущей подключенной сети
  try
    // Этот код будет работать только для подключенной сети
    // Для полного сканирования нужно использовать private API или NetworkExtension framework
    // что требует специальных разрешений от Apple

  except
    on E: Exception do
    begin
      // iOS ограничивает доступ к Wi-Fi сканированию
    end;
  end;
end;
{$ENDIF}

end.
