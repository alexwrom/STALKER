unit uScanerWiFi;

interface

uses System.SysUtils, System.Sensors, uGlobal, System.Classes, Math,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os,
  Androidapi.JNI.Net,
{$ENDIF}

  System.Sensors.Components, Generics.Collections;



function ScanDistanceToArtefacts(ALevel: integer): double;
procedure ScanNetworks;

{$IFDEF ANDROID}
function ScanAndroidNetworks: TList<TWiFiNetwork>;
function CalculateDistanceInMeters(Lat1, Lon1, Lat2, Lon2: double): double;
function CalculateWifiDistance(rssi: Integer; frequency: Integer = 2412): double;
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
    vMinDist := 10000;

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
{$IFDEF ANDROID}
    Networks := ScanAndroidNetworks;
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

end.
