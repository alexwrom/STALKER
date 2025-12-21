unit uQRScanerFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Objects,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager, FMX.Platform, Permissions, FMX.Controls.Presentation,
  uGlobal;

type
  TFrameQRScanner = class(TFrame)
    Camera: TCameraComponent;
    imgCamera: TImage;
    Image9: TImage;
    procedure Button1Click(Sender: TObject);
  private
    fScanInProgress: Boolean;
    fFrameTake: Integer;
    fScanBitmap: TBitmap;
    procedure CameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure ParseImage;
    procedure CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
    { Private declarations }
  public
    { Public declarations }
    procedure StartScan;
    procedure StopScan;
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

procedure TFrameQRScanner.Button1Click(Sender: TObject);
begin
  GenerateQRCode('Пробное создание', 200, imgCamera);
end;

procedure TFrameQRScanner.CameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    Camera.Active := false;
    Camera.Active := True;
  end
  else
    TDialogService.ShowMessage('Нужны разрешения на использование камеры')
end;

procedure TFrameQRScanner.StartScan;
begin
  fFrameTake := 0;
  fScanBitmap := nil;
  Camera.OnSampleBufferReady := CameraSampleBufferReady;
  PermissionsService.RequestPermissions(['android.permission.CAMERA'], CameraPermissionRequestResult);
end;

procedure TFrameQRScanner.StopScan;
begin
  Camera.Active := false;
end;

procedure TFrameQRScanner.CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      Camera.SampleBufferToBitmap(imgCamera.Bitmap, True);

      if (fScanInProgress) then
      begin
        exit;
      end;

      inc(fFrameTake);
      if (fFrameTake mod 4 <> 0) then
      begin
        exit;
      end;

      if Assigned(fScanBitmap) then
        FreeAndNil(fScanBitmap);

      fScanBitmap := TBitmap.Create();
      fScanBitmap.Assign(imgCamera.Bitmap);

      ParseImage();
    end);

end;

destructor TFrameQRScanner.Destroy;
begin
  if Assigned(fScanBitmap) then
    FreeAndNil(fScanBitmap);

  inherited Destroy;
end;

procedure TFrameQRScanner.ParseImage();
begin

  TThread.CreateAnonymousThread(
    procedure
    var
      ReadResult: TReadResult;
      ScanManager: TScanManager;

    begin
      fScanInProgress := True;
      ScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);

      try

        try
          ReadResult := ScanManager.Scan(fScanBitmap);
        except
        end;

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if (ReadResult <> nil) then
            begin
              ShowMessage(ReadResult.Text);
            end;
          end);

      finally
        if ReadResult <> nil then
          FreeAndNil(ReadResult);

        ScanManager.Free;
        fScanInProgress := false;
      end;

    end).Start();

end;

end.
