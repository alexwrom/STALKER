unit uLocationListener;

interface

uses
  FMX.Forms, Permissions,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.Location,
  Androidapi.JNI.Net,
{$ENDIF}
  System.Sensors;
{$IFDEF ANDROID}

type

  TLocationListener = class(TJavaLocal, JLocationListener)
  public
    procedure onFlushComplete(requestCode: integer); cdecl;
    procedure onLocationChanged(Location: JLocation); overload; cdecl;
    procedure onLocationChanged(locations: JList); overload; cdecl;
    procedure onProviderDisabled(provider: JString); cdecl;
    procedure onProviderEnabled(provider: JString); cdecl;
    procedure onStatusChanged(provider: JString; status: integer; extras: JBundle); cdecl;
  end;

var
  locationListener: TLocationListener;
  FLocationManager: JLocationManager;
  FServiceLocation: TLocationCoord2D;
{$ENDIF}

implementation

uses uMainForm;
{ TLocationListener }
{$IFDEF ANDROID}

procedure TLocationListener.onFlushComplete(requestCode: integer);
begin

end;

procedure TLocationListener.onLocationChanged(Location: JLocation);
begin
  if Assigned(MainForm.FFrameMap) then
    MainForm.FFrameMap.LocationisChanged(Location);
end;

procedure TLocationListener.onLocationChanged(locations: JList);
begin

end;

procedure TLocationListener.onProviderDisabled(provider: JString);
begin

end;

procedure TLocationListener.onProviderEnabled(provider: JString);
begin

end;

procedure TLocationListener.onStatusChanged(provider: JString; status: integer; extras: JBundle);
begin

end;
{$ENDIF}

end.
