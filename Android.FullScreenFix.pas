unit Android.FullScreenFix;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.Helpers, System.SysUtils;

type
  TAndroidFullScreen = class
  public
    procedure EnterFullScreen;
    procedure ExitFullScreen;
  end;

implementation

uses
  FMX.Platform.Android,
  Androidapi.JNI.JavaTypes,
  System.Classes, FMX.Types;

procedure TAndroidFullScreen.EnterFullScreen;
var
  Activity: JActivity;
  Window: JWindow;
  View: JView;
  Flags: Integer;
begin
  TThread.Queue(nil,
    procedure
    begin
      Activity := TAndroidHelper.Activity;
      if Activity = nil then Exit;

      Window := Activity.getWindow;
      if Window = nil then Exit;

      View := Window.getDecorView;
      if View = nil then Exit;

      Flags := TJView.JavaClass.SYSTEM_UI_FLAG_HIDE_NAVIGATION or
               TJView.JavaClass.SYSTEM_UI_FLAG_FULLSCREEN or
               TJView.JavaClass.SYSTEM_UI_FLAG_IMMERSIVE_STICKY;

      Flags := Flags or TJView.JavaClass.SYSTEM_UI_FLAG_LOW_PROFILE;

      View.setSystemUiVisibility(Flags);
    end);
end;

procedure TAndroidFullScreen.ExitFullScreen;
var
  Activity: JActivity;
  View: JView;
begin
  TThread.Queue(nil,
    procedure
    begin
      Activity := TAndroidHelper.Activity;
      if Activity = nil then Exit;
      View := Activity.getWindow.getDecorView;
      if View <> nil then
        View.setSystemUiVisibility(0);
    end);
end;

end.
