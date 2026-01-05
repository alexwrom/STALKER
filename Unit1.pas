unit unit1;

interface

uses
  System.SysUtils,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  Androidapi.JNI.Os,  Androidapi.JNI, FMX.Dialogs,
  Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText;

type
  TSimpleHotspot = class
  private
    FWiFiManager: JWifiManager;
    FReservation: JWifiManager_LocalOnlyHotspotReservation;

    // Используем анонимный метод для callback
    class procedure HotspotCallback(
      reservation: JWifiManager_LocalOnlyHotspotReservation;
      stopped: Boolean;
      errorCode: Integer); static;

  public
    constructor Create;
    procedure Start;
    procedure Stop;

    property Reservation: JWifiManager_LocalOnlyHotspotReservation read FReservation;
  end;

implementation

{ TSimpleHotspot }

constructor TSimpleHotspot.Create;
var
  WifiService: JObject;
begin
  inherited Create;

  WifiService := TAndroidHelper.Context.getSystemService(
    TJContext.JavaClass.WIFI_SERVICE
  );

  if Assigned(WifiService) then
    FWiFiManager := TJWifiManager.Wrap((WifiService as ILocalObject).GetObjectID);
end;

procedure TSimpleHotspot.Start;
var
  CallbackObj: JObject;
begin
  if not Assigned(FWiFiManager) then
    Exit;

  Stop;

  try

    // Запускаем hotspot (упрощенный вызов)
    var JavaCallback := TJWifiManager_LocalOnlyHotspotCallback.Wrap(CallbackObj);
    FWiFiManager.getwTSimpleHotspotstartLocalOnlyHotspot(JavaCallback., nil);

  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TSimpleHotspot.Stop;
begin
  if Assigned(FReservation) then
  begin
    FReservation.close;
    FReservation := nil;
  end;
end;

class procedure TSimpleHotspot.HotspotCallback(
  reservation: JWifiManager_LocalOnlyHotspotReservation;
  stopped: Boolean;
  errorCode: Integer);
begin
  // Это статический метод для callback'а
  // В реальности нужно хранить ссылку на экземпляр
end;

end.
