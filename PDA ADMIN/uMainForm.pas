unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, System.Sensors,
  System.Sensors.Components, Math, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Objects, uFrameMap, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, uGlobal,
  Classes.action, uScanerWiFi,
  Rest.Json, IdGlobal, StrUtils, Threading, System.IOUtils,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, // Для JString
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.Provider,
  Androidapi.Helpers, // Для StringToJString и JStringToString
  Androidapi.JNI.Os,
  FMX.Platform.Android,
{$ENDIF}
  FMX.Ani, FMX.Effects, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdTCPConnection, IdTCPClient, FMX.Edit, FMX.Media,
  Controls.listitem, Controls.item, FMX.EditBox, FMX.SpinBox, uGenericBaseData, OAuth2, Classes.Data, Classes.answer, FMX.DateTimeCtrls;

type

  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabMap: TTabItem;
    Image8: TImage;
    Image11: TImage;
    Image10: TImage;
    layDamage: TLayout;
    AniIndicator1: TAniIndicator;
    recLoading: TRectangle;
    Label3: TLabel;
    Label4: TLabel;
    recSkin: TRectangle;
    ProgressBar: TProgressBar;
    layEnterName: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    imgBottom: TImage;
    imgTop: TImage;
    Image1: TImage;
    Image3: TImage;
    Rectangle4: TRectangle;
    InnerGlowEffect9: TInnerGlowEffect;
    layBtn: TLayout;
    Image4: TImage;
    btnConfirmName: TCornerButton;
    labSTALKER: TLabel;
    InnerGlowEffect1: TInnerGlowEffect;
    Label2: TLabel;
    labNotConnect: TLabel;
    timerScannerWifiMerchant: TTimer;
    Image13: TImage;
    layMenu: TLayout;
    recSkin1: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    GridPanelLayout1: TGridPanelLayout;
    imgBtnMap: TImage;
    btnAddArt: TSpeedButton;
    Image9: TImage;
    InnerGlowEffect5: TInnerGlowEffect;
    ImgBtnPercs: TImage;
    btnAddAnomaly: TSpeedButton;
    Image12: TImage;
    InnerGlowEffect6: TInnerGlowEffect;
    imgBtnBag: TImage;
    btnAddPlace: TSpeedButton;
    Image14: TImage;
    InnerGlowEffect2: TInnerGlowEffect;
    Image7: TImage;
    layAddArt: TLayout;
    Rectangle5: TRectangle;
    btnCloseArts: TSpeedButton;
    Layout2: TLayout;
    Rectangle6: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    Image2: TImage;
    Image5: TImage;
    Image6: TImage;
    Image15: TImage;
    recListArts: TRectangle;
    InnerGlowEffect7: TInnerGlowEffect;
    Image17: TImage;
    btnAddArtSave: TCornerButton;
    Label1: TLabel;
    recSelectArt: TRectangle;
    layAddAnomaly: TLayout;
    Rectangle1: TRectangle;
    btnCloseAddAnomalies: TSpeedButton;
    Layout3: TLayout;
    Rectangle2: TRectangle;
    InnerGlowEffect8: TInnerGlowEffect;
    Image16: TImage;
    Image18: TImage;
    Image19: TImage;
    Image20: TImage;
    Rectangle3: TRectangle;
    InnerGlowEffect10: TInnerGlowEffect;
    Image21: TImage;
    btnAnomalySave: TCornerButton;
    Label5: TLabel;
    Rectangle7: TRectangle;
    cbAnomalyType: TComboBox;
    lbRadiation: TListBoxItem;
    lbElectro: TListBoxItem;
    lbPhisic: TListBoxItem;
    lbFire: TListBoxItem;
    lbChimishe: TListBoxItem;
    lbPSI: TListBoxItem;
    Label6: TLabel;
    sbRadius: TSpinBox;
    Label7: TLabel;
    sbPower: TSpinBox;
    layAddPlace: TLayout;
    Rectangle9: TRectangle;
    btnCloseLayPlaces: TSpeedButton;
    Layout4: TLayout;
    Rectangle10: TRectangle;
    InnerGlowEffect11: TInnerGlowEffect;
    Image22: TImage;
    Image23: TImage;
    Image24: TImage;
    Image25: TImage;
    Rectangle11: TRectangle;
    InnerGlowEffect12: TInnerGlowEffect;
    Image26: TImage;
    btnAddPlacesSave: TCornerButton;
    Label8: TLabel;
    Rectangle12: TRectangle;
    cbPlaceType: TComboBox;
    lbBase: TListBoxItem;
    lbSafe: TListBoxItem;
    Label9: TLabel;
    Label10: TLabel;
    sbRadiusPlace: TSpinBox;
    ePlaceName: TEdit;
    imgDownload: TImage;
    btnDownload: TSpeedButton;
    Image28: TImage;
    InnerGlowEffect13: TInnerGlowEffect;
    imgBtnSend: TImage;
    btnSendToServer: TSpeedButton;
    Image30: TImage;
    InnerGlowEffect14: TInnerGlowEffect;
    imgClear: TImage;
    btnClear: TSpeedButton;
    Image29: TImage;
    InnerGlowEffect15: TInnerGlowEffect;
    ckHideInDatetime: TCheckBox;
    Layout1: TLayout;
    teTimeHide: TTimeEdit;
    deDateHide: TDateEdit;
    Label11: TLabel;
    GridPanelLayout2: TGridPanelLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    cbBand8: TCheckBox;
    cbMonolit5: TCheckBox;
    cbDolg3: TCheckBox;
    cbStalker1: TCheckBox;
    cbArmy4: TCheckBox;
    cbNaem6: TCheckBox;
    cbNebo7: TCheckBox;
    cbUchenie9: TCheckBox;
    cbTorg10: TCheckBox;
    cbSvoboda2: TCheckBox;
    VertScrollBox1: TVertScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConfirmNameClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure timerScannerWifiMerchantTimer(Sender: TObject);
    procedure btnAddArtSaveClick(Sender: TObject);
    procedure btnAddArtClick(Sender: TObject);
    procedure btnCloseArtsClick(Sender: TObject);
    procedure btnAnomalySaveClick(Sender: TObject);
    procedure btnCloseAddAnomaliesClick(Sender: TObject);
    procedure btnAddAnomalyClick(Sender: TObject);
    procedure btnAddPlacesSaveClick(Sender: TObject);
    procedure btnCloseLayPlacesClick(Sender: TObject);
    procedure btnAddPlaceClick(Sender: TObject);
    procedure ePlaceNameChangeTracking(Sender: TObject);
    procedure btnSendToServerClick(Sender: TObject);
    procedure ePlaceNameChange(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private

    procedure LoadArtefacts;
    procedure GetImage(AHex: UnicodeString);
    procedure ItemArtClick(FTagObject: TObject; FItem: TFMXObject);
    procedure GetServerData;
    procedure GetMap;
    procedure GetData;

  public
    { Public declarations }
    FFrameMap: TFrameMap;
    FControlListArt: TControlListItem;
    procedure StartApp;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Permissions;

procedure TMainForm.btnAddAnomalyClick(Sender: TObject);
begin
  layAddAnomaly.Visible := true;
  FCoords := FLocation;
end;

procedure TMainForm.btnAddArtClick(Sender: TObject);
begin
  layAddArt.Visible := true;
  recSelectArt.Parent := nil;
  FCoords := FLocation;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  MessageDlg('Желаете стереть все аномалии, артефакты и локации с карты?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    var
      vQuery: TFDQuery;
    begin
      if (AResult = mrYes) then
      begin
        ExeExec('delete from arts_to_map; delete from anomalies; delete from places;', exExecute, vQuery);
        StartApp;
      end;
    end);
end;

procedure TMainForm.btnCloseAddAnomaliesClick(Sender: TObject);
begin
  layAddAnomaly.Visible := false;
end;

procedure TMainForm.btnCloseArtsClick(Sender: TObject);
begin
  layAddArt.Visible := false;
end;

procedure TMainForm.btnCloseLayPlacesClick(Sender: TObject);
begin
  layAddPlace.Visible := false;
end;

procedure TMainForm.btnConfirmNameClick(Sender: TObject);
begin
  layEnterName.Visible := false;
  recLoading.Visible := true;
  GetServerData;
end;

procedure TMainForm.btnDownloadClick(Sender: TObject);
begin
  MessageDlg('Загружая данные аномалий, артефактов и локаций вы потеряете текущий прогресс. Продолжить загрузку?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
      begin
        recLoading.Visible := true;

        TTask.Run(
          procedure
          begin
            try
              GetData;
            finally
              recLoading.Visible := false;
              StartApp;
            end;
          end);
      end;
    end);
end;

procedure TMainForm.GetServerData;
begin
  TTask.Run(
    procedure
    begin
      try
        GetData;
        GetMap;
      finally
        recLoading.Visible := false;
        StartApp;
      end;
    end);
end;

procedure TMainForm.GetData;
var
  AData: TData;
  AAnswer: TAnswer;
  vSQL: UnicodeString;
  I: Integer;
  vQuery: TFDQuery;
begin
  ProgressBar.Value := 0;
  try
    AAnswer := TJSON.JsonToObject<TAnswer>(GetDataServer('api/get_data_admin'));
    AData := TJSON.JsonToObject<TData>(AAnswer.Json);

    TThread.Synchronize(nil,
      procedure
      begin
        ProgressBar.Max := AData.SQL.Count;
      end);

    for I := 0 to AData.SQL.Count - 1 do
    begin
      vSQL := vSQL + AData.SQL[I];

      TThread.Synchronize(nil,
        procedure
        begin
          ProgressBar.Value := ProgressBar.Value + 1;
        end);
    end;

    ExeExec(vSQL, exExecute, vQuery);
  finally
    FreeAndNil(AAnswer);
    FreeAndNil(AData);
  end;
end;

procedure TMainForm.GetMap;
var
  AAnswer: TAnswer;
begin
  try
    AAnswer := TJSON.JsonToObject<TAnswer>(GetDataServer('api/get_map'));
    GetImage(AAnswer.Json);
  finally
    FreeAndNil(AAnswer);
  end;
end;

procedure TMainForm.btnSendToServerClick(Sender: TObject);
begin
  MessageDlg('Все существующие аномалии, расположение артефактов и локации на сервере будут утеряны. Сделайте при необходимости бэкап данных. Продолжить отправку?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
      begin
        TTask.Run(
          procedure
          var
            AAnswer: TAnswer;
            AData: TData;
          begin
            try
              try
                AData := TData.Create;
                AData.SQL := Tlist<UnicodeString>.Create;
                AData.SQL := GoGenericBaseData();

                AAnswer := TJSON.JsonToObject<TAnswer>(PostDataServer('api/upload_data_from_admin', TJSON.ObjectToJsonString(AData)));

                if AAnswer.Status = 'success' then
                  TThread.Synchronize(nil,
                    procedure
                    begin
                      Showmessage('Данные отправлены успешно')
                    end)
                else
                  TThread.Synchronize(nil,
                    procedure
                    begin
                      Showmessage('Ошибка записи данных на стороне сервера');
                    end);
              finally
                FreeAndNil(AAnswer);
                FreeAndNil(AData);
              end;
            except
              TThread.Synchronize(nil,
                procedure
                begin
                  Showmessage('Ошибка отправки данных. Повторите позже');
                end);
            end;
          end);
      end;
    end);
end;

procedure TMainForm.ePlaceNameChange(Sender: TObject);
begin
  btnAddPlacesSave.Enabled := ePlaceName.Text <> '';
end;

procedure TMainForm.ePlaceNameChangeTracking(Sender: TObject);
begin
  btnAddPlacesSave.Enabled := ePlaceName.Text <> '';
end;

procedure TMainForm.btnAddArtSaveClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  if layAddArt.Tag <> 0 then // Update
    ExeExec(Format('update arts_to_map set art_id = %d where art_to_map_id = %d;', [btnAddArtSave.Tag, layAddArt.Tag]), exExecute, vQuery)
  else
    ExeExec(Format('insert into arts_to_map (art_id, lat, lon) values (%d, %s, %s);', [btnAddArtSave.Tag, StringReplace(FCoords.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FCoords.Longitude.ToString, ',', '.', [rfReplaceAll])]),
      exExecute, vQuery);

  layAddArt.Tag := 0;
  FFrameMap.LoadArtefactsToMap;
  FFrameMap.UpdateArts;
  layAddArt.Visible := false;
end;

procedure TMainForm.btnAddPlaceClick(Sender: TObject);
begin
  layAddPlace.Visible := true;
  ePlaceName.Text := '';
  FCoords := FLocation;
end;

procedure TMainForm.btnAddPlacesSaveClick(Sender: TObject);
var
  vQuery: TFDQuery;

  function GetFractions: string;
  begin
    if cbBand8.IsChecked then
      Result := '8';

    if cbMonolit5.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '5';

    if cbDolg3.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '3';

    if cbStalker1.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '1';

    if cbArmy4.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '4';

    if cbNaem6.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '6';

    if cbNebo7.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '7';

    if cbUchenie9.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '9';

    if cbTorg10.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '10';

    if cbSvoboda2.IsChecked then
      Result := Result + IfThen(Result = '','',',') + '2';
  end;
begin
  if layAddPlace.Tag <> 0 then // Update
    ExeExec(Format('update places set radius = %d, name = %s, type = %s, fractions = %s  where place_id = %d;', [Round(sbRadiusPlace.Value), QuotedStr(ePlaceName.Text), QuotedStr(IfThen(cbPlaceType.Selected.Tag = 0, 'mtBase', 'mtSafe')), QuotedStr(GetFractions),  layAddPlace.Tag]),
      exExecute, vQuery)
  else
    ExeExec(Format('insert into places (radius, name, type, lat, lon, fractions) values (%d, %s, %s, %s, %s, %s);', [Round(sbRadiusPlace.Value), QuotedStr(ePlaceName.Text), QuotedStr(IfThen(cbPlaceType.Selected.Tag = 0, 'mtBase', 'mtSafe')),
      StringReplace(FCoords.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FCoords.Longitude.ToString, ',', '.', [rfReplaceAll]), QuotedStr(GetFractions)]), exExecute, vQuery);

  layAddPlace.Tag := 0;
  FFrameMap.LoadPlaces;
  FFrameMap.UpdateBaseSafeDead;
  cbBand8.IsChecked := false;
  cbMonolit5.IsChecked  := false;
  cbDolg3.IsChecked  := false;
  cbStalker1.IsChecked := false;
  cbArmy4.IsChecked := false;
  cbNaem6.IsChecked := false;
  cbNebo7.IsChecked := false;
  cbUchenie9.IsChecked := false;
  cbTorg10.IsChecked := false;
  cbSvoboda2.IsChecked := false;
  layAddPlace.Visible := false;
end;

procedure TMainForm.btnAnomalySaveClick(Sender: TObject);
var
  vQuery: TFDQuery;

  function GetDateTimeHide: string;
  var
    AFormatSettings: TFormatSettings;
  begin
    AFormatSettings.DateSeparator := '-';
    AFormatSettings.TimeSeparator := ':';
    AFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
    AFormatSettings.LongTimeFormat := 'hh:nn:ss';

    Result := '1899-01-01 00:00:00';

    if ckHideInDatetime.IsChecked then
      Result := DateToStr(deDateHide.Date, AFormatSettings) + ' ' + TimeToStr(teTimeHide.Time, AFormatSettings);
  end;
begin
  if layAddAnomaly.Tag <> 0 then // Update
    ExeExec(Format('update anomalies set radius = %d, power = %d,anomaly_type_id = %d, hide_in_datetime = %s, datetime_hide = %s  where anomaly_id = %d;', [Round(sbRadius.Value), Round(sbPower.Value), cbAnomalyType.Selected.Tag, ckHideInDatetime.IsChecked.ToString(), QuotedStr(GetDateTimeHide),  layAddAnomaly.Tag]), exExecute, vQuery)
  else
    ExeExec(Format('insert into anomalies (radius, power, anomaly_type_id, lat, lon, hide_in_datetime, datetime_hide) values (%d, %d, %d, %s, %s, %s, %s);', [Round(sbRadius.Value), Round(sbPower.Value), cbAnomalyType.Selected.Tag, StringReplace(FCoords.Latitude.ToString, ',', '.',
      [rfReplaceAll]), StringReplace(FCoords.Longitude.ToString, ',', '.', [rfReplaceAll]), ckHideInDatetime.IsChecked.ToString(), QuotedStr(GetDateTimeHide)]), exExecute, vQuery);

  layAddAnomaly.Tag := 0;
  FFrameMap.LoadAnomalies;
  FFrameMap.UpdateAnomalies;
  layAddAnomaly.Visible := false;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  timerScannerWifiMerchant.Enabled := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Self.FullScreen := true;
{$ENDIF}
end;

procedure TMainForm.LoadArtefacts;
var
  vQuery: TFDQuery;
  listitem: TControlItem;
begin
  ExeExec('select * from arts a;', exActive, vQuery);
  try
    vQuery.First;

    if FControlListArt <> nil then
    begin
      try
        FControlListArt.Clear;
        FControlListArt.Visible := false;
        FControlListArt.Parent := nil;
        FreeAndNil(FControlListArt);
        FControlListArt := nil;
      finally
      end;
    end;

    FControlListArt := TControlListItem.Create(recListArts);

    while Not vQuery.Eof do
    begin
      listitem := FControlListArt.AddItem(false);

      with listitem do
      begin
        CenterText := vQuery.FieldByName('art_name').AsString;
        listitem.Height := 75;
        OnItemClick := ItemArtClick;
        Tag := vQuery.FieldByName('art_id').AsInteger;
        TagObject := listitem;
        ShowHint := false;

        GetImage.Assign(vQuery.FieldByName('icon'));
        labCenterText.TextSettings.FontColor := TAlphaColors.Darkgray;
      end;

      vQuery.Next;
    end;

    FControlListArt.PrepareForPaint;
    FControlListArt.Repaint;

  finally
    FreeQueryAndConn(vQuery);
  end;
end;

procedure TMainForm.ItemArtClick(FTagObject: TObject; FItem: TFMXObject);
begin
  recSelectArt.Parent := (FItem as TControlItem).rcBackground;
  recSelectArt.Align := TAlignLayout.Contents;
  recSelectArt.BringToFront;
  recSelectArt.Visible := true;

  btnAddArtSave.Tag := (FItem as TControlItem).Tag;
  btnAddArtSave.Enabled := true;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  vMapExists: boolean;
  FDQuery: TFDQuery;
begin
  ExeExec('select count(1) as cnt from game_data;', exActive, FDQuery);
  vMapExists := FDQuery.FieldByName('cnt').AsInteger > 0;
  FreeQueryAndConn(FDQuery);

  if vMapExists then
  begin
    StartApp;
  end
  else
  begin
    layEnterName.Visible := true;
  end;
{$IFDEF ANDROID}
  PermissionsService.RequestPermissions(['android.permission.WRITE_EXTERNAL_STORAGE'], nil);
{$ENDIF}
end;

procedure TMainForm.StartApp;
begin
  LoadArtefacts;

  while Assigned(FFrameMap) do
  begin
    FFrameMap.Parent := nil;
    FreeAndNil(FFrameMap);
  end;

  FFrameMap := TFrameMap.Create(TabMap);
  FFrameMap.Parent := TabMap;

  PermissionsService.RequestPermissions(['android.permission.ACCESS_WIFI_STATE', 'android.permission.CHANGE_WIFI_STATE', 'android.permission.ACCESS_FINE_LOCATION'],
    procedure(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(GrantResults) > 0) and (GrantResults[0] = TPermissionStatus.Granted) then
      begin
{$IFDEF ANDROID}
        FFrameMap.LocationServiceChanged;
        FFrameMap.btnMyLocationClick(nil);
{$ENDIF}
      end
      else
      begin
        Showmessage('Необходимы разрешения для сканирования Wi-Fi');
      end;
    end);
end;

procedure TMainForm.timerScannerWifiMerchantTimer(Sender: TObject);
begin
{$IFDEF ANDROID}
  TThread.CreateAnonymousThread(
    procedure
    begin

      ConnectToMerchatZone; // Поиск зоны торговли

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          layBtn.Visible := FIsMerchantZone;
          labNotConnect.Visible := NOT FIsMerchantZone;
          imgBtnSend.Visible := FIsMerchantZone;
          imgDownload.Visible := FIsMerchantZone;
        end);
    end).Start;
{$ENDIF}
end;

procedure TMainForm.GetImage(AHex: UnicodeString);
var
  Stream: TMemoryStream;
  I: Integer;
  ByteValue: Byte;
  Bytes: TBytes;
  HexByte: UnicodeString;
  vPath: string;
begin
  SetLength(Bytes, Length(AHex) div 2);
  // Конвертируем шестнадцатеричную строку в байты
  for I := 0 to (Length(AHex) div 2) - 1 do
  begin
    HexByte := Copy(AHex, I * 2 + 1, 2);
    try
      ByteValue := StrToInt('$' + HexByte);
      Bytes[I] := ByteValue;
    except
    end;
  end;

  // Создаем поток из байт
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(Bytes[0], Length(Bytes));
    Stream.Position := 0;
    vPath := System.IOUtils.TPath.Combine(TPath.GetDocumentsPath, 'map_image.png');
    if FileExists(vPath) then
      TFile.Delete(vPath);

    Stream.SaveToFile(vPath);
  finally
    FreeAndNil(Stream);
  end;
end;

end.
