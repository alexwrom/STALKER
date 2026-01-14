unit uScanerWiFi;

interface

uses System.SysUtils, System.Sensors, uGlobal, System.Classes, Math,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os,
  Androidapi.JNI.Net,
{$ENDIF}
  System.Sensors.Components, Generics.Collections, FMX.Dialogs, Permissions, System.Types;

function ScanDistanceToArtefacts(ALevel: Integer): double;
procedure ScanNetworks;

{$IFDEF ANDROID}
function ScanAndroidNetworks: TList<TWiFiNetwork>;
function CalculateDistanceInMeters(Lat1, Lon1, Lat2, Lon2: double): double;
function CalculateWifiDistance(rssi: Integer; frequency: Integer = 2412): double;
procedure ConnectToMerchatZone;
function GetMyIP: string;
{$ENDIF}

var
  FNetworks: TList<TWiFiNetwork>;
{$IFDEF ANDROID}
  WiFiManager: JWiFiManager;
{$ENDIF}

const
  MERCHANT_WIFI = 'Merchant';

implementation

function convertor(ip: Integer): string;
begin
  Result := Format('%d.%d.%d.%d', [ip and $FF, ip shr 8 and $FF, ip shr 16 and $FF, ip shr 24 and $FF])
end;

// Сканмруем ближайший артефакт и возвращаем к ниму дистанцию
function ScanDistanceToArtefacts(ALevel: Integer): double;
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

{$IFDEF ANDROID}

function GetMyIP: string;
var
  vInfo: JWifiInfo;
begin
  if (WiFiManager <> nil) and WiFiManager.isWifiEnabled and (FNetworks.Count > 0) then
  begin
    vInfo := WiFiManager.getConnectionInfo;
    Result := convertor(vInfo.getIpAddress);
  end;
end;
{$ENDIF}

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

function IsMechantZone: boolean;
var
  vInfo: JWifiInfo;
  i: Integer;
begin
  ScanNetworks;

  Result := False;
  try
    if (WiFiManager <> nil) and WiFiManager.isWifiEnabled then
    begin
      for i := 0 to FNetworks.Count - 1 do
        if FNetworks[i].SSID = MERCHANT_WIFI then
        begin
          vInfo := WiFiManager.getConnectionInfo;
          Result := (JStringToString(vInfo.getSSID) = '"' + MERCHANT_WIFI + '"') and (GetMyIP <> '0.0.0.0');
        end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;

end;

function ScanAndroidNetworks: TList<TWiFiNetwork>;
var
  ScanResults: JList;
  i: Integer;
  Network: TWiFiNetwork;
  ScanResult: JScanResult;
  SSID, BSSID: string;
  Freq, rssi: Integer;
  // Intent: JIntent;
begin
  Result := TList<TWiFiNetwork>.Create;
  try
    if TOSVersion.Check(10) then
      WiFiManager := TJWifiManager.Wrap((TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE) as ILocalObject).GetObjectID)
    else
      WiFiManager := TJWifiManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE));

    if (WiFiManager <> nil) and WiFiManager.isWifiEnabled then
    begin
      // вызов меню WIFI

      // Intent := TJIntent.Create;
      // Intent.setAction(TJContext.JavaClass..ACTION_WIFI_SETTINGS);
      // TAndroidHelper.Activity.startActivity(Intent);

      WiFiManager.startScan;

      // Сканирование сетей
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
            // Network.Distance := CalculateWifiDistance(rssi, Freq);
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

function CreateWifiConfiguration(const ASSID: string): JWifiConfiguration;
var
  Config: JWifiConfiguration;
begin
  Config := TJWifiConfiguration.JavaClass.init;
  Config.SSID := StringToJString('"' + ASSID + '"'); // SSID в кавычках
  Config.preSharedKey := StringToJString('"12345678"');
  Result := Config;
end;

function ConnectToNetworkAndroid10Plus: boolean;
begin
  Result := False;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.CHANGE_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    var
      ConnectivityManager: JConnectivityManager;
      WifiNetworkSpecifier: JWifiNetworkSpecifier;
      NetworkRequestBuilder: JNetworkRequest_Builder;
      NetworkRequest: JNetworkRequest;
      NetworkCallback: JConnectivityManager_NetworkCallback;
      Service: JObject;
      Context: JContext;
      FCurrentNetworkRequest: JNetworkRequest;
      WifiNetworkSpecifierBuilder: JWifiNetworkSpecifier_Builder;
      FNetworkCallback: JConnectivityManager_NetworkCallback;
      Suggestion: JWifiNetworkSuggestion;
      SuggestionsList: JArrayList;
      Status: Integer;
      Intent: JIntent;
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
        try // Создаем предложение сети (WifiNetworkSuggestion)
          Suggestion := TJWifiNetworkSuggestion_Builder.JavaClass.init.setSsid(StringToJString(MERCHANT_WIFI)).setWpa2Passphrase(StringToJString('12345678')).setIsAppInteractionRequired(False).setIsHiddenSsid(False).build;

          // Создаем список предложений
          SuggestionsList := TJArrayList.JavaClass.init;
          SuggestionsList.Add(Suggestion);

          // Добавляем предложения в систему
          Status := WiFiManager.addNetworkSuggestions(JList(SuggestionsList));

          // Получаем ConnectivityManager
          Context := TAndroidHelper.Context;
          Service := Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE);

          if true then // Assigned(Service) then
          begin
            ConnectivityManager := TJConnectivityManager.Wrap((Service as ILocalObject).GetObjectID);
            // Создаем спецификацию Wi-Fi сети
            WifiNetworkSpecifierBuilder := TJWifiNetworkSpecifier_Builder.JavaClass.init;

            // Устанавливаем SSID
            WifiNetworkSpecifierBuilder.setSsid(StringToJString(MERCHANT_WIFI));

            // Устанавливаем пароль для WPA2
            WifiNetworkSpecifierBuilder.setWpa2Passphrase(StringToJString('12345678'));

            WifiNetworkSpecifier := WifiNetworkSpecifierBuilder.build;

            // Ключевое исправление: НЕ удаляем NET_CAPABILITY_INTERNET
            NetworkRequestBuilder := TJNetworkRequest_Builder.JavaClass.init;

            // Указываем, что нам нужен Wi-Fi транспорт
            NetworkRequestBuilder.addTransportType(TJNetworkCapabilities.JavaClass.TRANSPORT_WIFI);

            // Добавляем возможность выбора сети пользователем
            NetworkRequestBuilder.addCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_INTERNET);

            // Указываем, что запрос должен оставаться активным
            NetworkRequestBuilder.setNetworkSpecifier(TJNetworkSpecifier.Wrap((WifiNetworkSpecifier as ILocalObject).GetObjectID));

            NetworkRequest := NetworkRequestBuilder.build;

            FNetworkCallback := TJConnectivityManager_NetworkCallback.JavaClass.init;

            // Сохраняем текущий запрос
            FCurrentNetworkRequest := NetworkRequest;

            // Запрашиваем подключение с высоким приоритетом
            ConnectivityManager.requestNetwork(NetworkRequest, FNetworkCallback, 1000);

            Intent := TJIntent.JavaClass.init(TJWifiManager.JavaClass.SCAN_RESULTS_AVAILABLE_ACTION);

            // Отправляем broadcast
            TAndroidHelper.Context.sendBroadcast(Intent);
          end

        except
        end;
      end
      else
      begin
        ShowMessage('Необходимы разрешения для сканирования Wi-Fi');
      end;
    end);

end;

function ConnectToNetwork: boolean;
begin
  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.CHANGE_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    var
      i, j: Integer;
      vList: JList;
      NetId: Integer;
      ExistingConfig: JWifiConfiguration;
      Config: JWifiConfiguration;
    begin
      try
        for j := 0 to FNetworks.Count - 1 do
          if FNetworks[j].SSID = MERCHANT_WIFI then
          begin
            if (WiFiManager <> nil) and WiFiManager.isWifiEnabled then
            begin
              // Ищем существующую конфигурацию
              vList := WiFiManager.getConfiguredNetworks;
              NetId := -1;

              if Assigned(vList) then
              begin
                for i := 0 to vList.Size - 1 do
                begin
                  ExistingConfig := TJWifiConfiguration.Wrap(vList.get(i));
                  if JStringToString(ExistingConfig.SSID).Contains(MERCHANT_WIFI) then
                  begin
                    NetId := ExistingConfig.networkId;
                    Break;
                  end;
                end;
              end;

              if NetId = -1 then
              begin
                // Создаем новую конфигурацию (предполагаем WPA2)
                Config := CreateWifiConfiguration(MERCHANT_WIFI);
                NetId := WiFiManager.addNetwork(Config);
              end;

              if NetId <> -1 then
              begin
                // Отключаем от текущей сети
                WiFiManager.disconnect;

                // Подключаемся к выбранной сети
                if WiFiManager.enableNetwork(NetId, true) then
                begin
                  WiFiManager.reconnect;
                  sleep(2000);
                end;
              end;

            end;
            Break;
          end;
      except
      end;
    end);
end;

procedure ConnectToMerchatZone;
var
  i, j: Integer;
  vList: JList;
  NetId: Integer;
  ExistingConfig: JWifiConfiguration;
  Config: JWifiConfiguration;
begin
  FIsMerchantZone := IsMechantZone;

  if not FIsMerchantZone then
  begin
    if TOSVersion.Check(10) then
      ConnectToNetworkAndroid10Plus
    else
      ConnectToNetwork;
  end;
end;
{$ENDIF}

end.
