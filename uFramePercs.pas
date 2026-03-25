unit uFramePercs;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Effects, FMX.Objects, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListBox, FMX.ListView, Generics.Collections,
  uGlobal,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, StrUtils,
  FireDAC.Comp.Client, Math, System.DateUtils, System.TimeSpan, classes.action, classes.send, Rest.Json, uScanerWiFi, classes.medicdata;

type
  TFramePercs = class(TFrame)
    layLeftBlock: TLayout;
    LeftRicghtBlock: TLayout;
    recSkin: TRectangle;
    layDetector: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    laySlot1: TLayout;
    ImgGlass1: TImage;
    laySlot2: TLayout;
    ImgGlass2: TImage;
    laySlot3: TLayout;
    ImgGlass3: TImage;
    laySlot4: TLayout;
    ImgGlass4: TImage;
    ImgParameter: TImage;
    laySlot5: TLayout;
    ImgGlass5: TImage;
    RadiationArmor: TRectangle;
    ChimisheArmor: TRectangle;
    ElectroArmor: TRectangle;
    PsiArmor: TRectangle;
    PhisicArmor: TRectangle;
    FireArmor: TRectangle;
    Layout9: TLayout;
    DetectorList: TImageList;
    btnOpenDetector: TSpeedButton;
    imgDetector: TImage;
    ImgSlot1: TImage;
    ImgSlot2: TImage;
    ImgSlot3: TImage;
    ImgSlot4: TImage;
    ImgSlot5: TImage;
    Layout8: TLayout;
    imgWeaponIcon: TImage;
    btnWeaponInfo: TSpeedButton;
    Layout11: TLayout;
    imgArmorIcon: TImage;
    layArts: TLayout;
    Image22: TImage;
    GlassList: TImageList;
    layInfo: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    ImgBottom: TImage;
    ImgTop: TImage;
    Image13: TImage;
    Image8: TImage;
    imgPercs: TImage;
    infoRadiation: TRectangle;
    infoChimishe: TRectangle;
    infoElectro: TRectangle;
    infoPsi: TRectangle;
    infoPhisic: TRectangle;
    infoFire: TRectangle;
    btnSlot1Info: TSpeedButton;
    btnSlot2Info: TSpeedButton;
    btnSlot3Info: TSpeedButton;
    btnSlot4Info: TSpeedButton;
    btnSlot5Info: TSpeedButton;
    ImgArmorHealth: TImage;
    ArmorHealthProgress: TRectangle;
    Layout1: TLayout;
    infoLabPsi: TLabel;
    infoLabChimishe: TLabel;
    infoLabElectro: TLabel;
    infoLabradiation: TLabel;
    infoLabPhisic: TLabel;
    infoLabFire: TLabel;
    labFireArmor: TLabel;
    labElectroArmor: TLabel;
    labChimisheArmor: TLabel;
    labPhisicArmor: TLabel;
    labPsiArmor: TLabel;
    labRadiationArmor: TLabel;
    Image5: TImage;
    Image7: TImage;
    Image14: TImage;
    btnArmorInfo: TSpeedButton;
    InnerGlowEffect3: TInnerGlowEffect;
    recDetector: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    recArmor: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    recWeapon: TRectangle;
    Rectangle5: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    layClearArmorWeapon: TLayout;
    Image1: TImage;
    btnClearArmorWeapon: TCornerButton;
    layRestoreArmorWeapon: TLayout;
    Image2: TImage;
    btnRestoreArmorWeapon: TCornerButton;
    layClearArt: TLayout;
    Image9: TImage;
    btnClearArt: TCornerButton;
    recChimisheFull: TRectangle;
    recElectroFull: TRectangle;
    recFireFull: TRectangle;
    recPhisicFull: TRectangle;
    recPsiFull: TRectangle;
    recRadiationFull: TRectangle;
    infoChimisheFullArmor: TRectangle;
    infoElectroFullArmor: TRectangle;
    infoFireFullArmor: TRectangle;
    infoRadiationFullArmor: TRectangle;
    infoPhisicFullArmor: TRectangle;
    infoPsiFullArmor: TRectangle;
    recSkin1: TRectangle;
    InnerGlowEffect5: TInnerGlowEffect;
    btnCloseInfo: TSpeedButton;
    layParameter: TLayout;
    recBack: TRectangle;
    layPercs: TLayout;
    Rectangle1: TRectangle;
    recHealth: TRectangle;
    HealthProgress: TRectangle;
    ImgHealth: TImage;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    WeaponHealthProgress: TRectangle;
    Image3: TImage;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    layPercsUp: TLayout;
    Layout14: TLayout;
    recSkin2: TRectangle;
    InnerGlowEffect7: TInnerGlowEffect;
    Image6: TImage;
    Image11: TImage;
    Image12: TImage;
    Image15: TImage;
    Rectangle10: TRectangle;
    InnerGlowEffect8: TInnerGlowEffect;
    btnClosePercsUp: TSpeedButton;
    GridPanelLayout3: TGridPanelLayout;
    layTehnic3: TLayout;
    Image17: TImage;
    btnTehnic3: TSpeedButton;
    layMedic3: TLayout;
    Image19: TImage;
    btnMedic3: TSpeedButton;
    layTehnic2: TLayout;
    Image20: TImage;
    btnTehnic2: TSpeedButton;
    layMedic2: TLayout;
    Image21: TImage;
    btnMedic2: TSpeedButton;
    layTehnic1: TLayout;
    Image23: TImage;
    btnTehnic1: TSpeedButton;
    layMedic1: TLayout;
    Image24: TImage;
    btnMedic1: TSpeedButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Layout2: TLayout;
    Rectangle6: TRectangle;
    InnerGlowEffect6: TInnerGlowEffect;
    Layout7: TLayout;
    Image10: TImage;
    btnOpenPercs: TSpeedButton;
    Image16: TImage;
    Image4: TImage;
    reсBack: TRectangle;
    TimerPercReload: TTimer;
    layMedicReload: TLayout;
    Rectangle7: TRectangle;
    labReloadTimerMedic: TLabel;
    InnerGlowEffect9: TInnerGlowEffect;
    layTehnicReload: TLayout;
    Rectangle9: TRectangle;
    labReloadTimerTehnic: TLabel;
    InnerGlowEffect10: TInnerGlowEffect;
    laySelection: TLayout;
    Rectangle11: TRectangle;
    btnCloseSelection: TSpeedButton;
    laySelectionBody: TLayout;
    Rectangle12: TRectangle;
    InnerGlowEffect11: TInnerGlowEffect;
    Image18: TImage;
    Image25: TImage;
    Image26: TImage;
    Image27: TImage;
    Rectangle13: TRectangle;
    InnerGlowEffect12: TInnerGlowEffect;
    layBtnMedicMyself: TLayout;
    Image28: TImage;
    btnMedicMyself: TCornerButton;
    layBtnRestore: TLayout;
    Image29: TImage;
    btnHelp: TCornerButton;
    layQR: TLayout;
    recBackQR: TRectangle;
    imgQR: TImage;
    btnCloseQR: TSpeedButton;
    Layout3: TLayout;
    layBtnMedic: TLayout;
    Image30: TImage;
    btnMedic: TCornerButton;
    procedure btnInfoClick(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnArmorInfoClick(Sender: TObject);
    procedure btnOpenDetectorClick(Sender: TObject);
    procedure btnClearArmorWeaponClick(Sender: TObject);
    procedure btnClearArtClick(Sender: TObject);
    procedure btnWeaponInfoClick(Sender: TObject);
    procedure btnClosePercsUpClick(Sender: TObject);
    procedure btnOpenPercsClick(Sender: TObject);
    procedure btnMedic1Click(Sender: TObject);
    procedure TimerPercReloadTimer(Sender: TObject);
    procedure btnMedic2Click(Sender: TObject);
    procedure btnMedic3Click(Sender: TObject);
    procedure btnMedicMyselfClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnCloseSelectionClick(Sender: TObject);
    procedure btnRestoreArmorWeaponClick(Sender: TObject);
    procedure btnCloseQRClick(Sender: TObject);
    procedure btnMedicClick(Sender: TObject);
    procedure btnTehnic1Click(Sender: TObject);
  private
    { Private declarations }
    FArtsList: TList<TPerc>;
    procedure ReloadArmor;

    function GetTimeDec(ASeconds: integer): string;

  public
    FActiveAction: TAction;
    procedure StartTimerTehnicPercReload(ALastActionDateTime: TDateTime);
    procedure StartTimerMedicPercReload(ALastActionDateTime: TDateTime);
    procedure ReloadArts;
    procedure ReloadPercs;
    procedure SetChimisheArmor(Value: integer);
    procedure SetElectroArmor(Value: integer);
    procedure SetFireArmor(Value: integer);
    procedure SetPhisicArmor(Value: integer);
    procedure SetPsiArmor(Value: integer);
    procedure SetRadiationArmor(Value: integer);
    procedure SetDetector(ID, Radius, Level: integer);
    constructor Create(AObject: TFmxObject);
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFramePercs.btnArmorInfoClick(Sender: TObject);
begin
  BtnClickMedia;
  infoRadiation.Width := infoRadiation.Tag * FArmorPerc.RadiationArmor / 100;
  infoChimishe.Width := infoChimishe.Tag * FArmorPerc.ChimisheArmor / 100;
  infoElectro.Width := infoElectro.Tag * FArmorPerc.ElectroArmor / 100;
  infoPsi.Width := infoPsi.Tag * FArmorPerc.PsiArmor / 100;
  infoPhisic.Width := infoPhisic.Tag * FArmorPerc.PhisicArmor / 100;
  infoFire.Width := infoFire.Tag * FArmorPerc.FireArmor / 100;

  infoRadiationFullArmor.Width := infoRadiation.Tag * (FArmorPerc.RadiationArmor / Person.ArmorHealth);
  infoChimisheFullArmor.Width := infoChimishe.Tag * (FArmorPerc.ChimisheArmor / Person.ArmorHealth);
  infoElectroFullArmor.Width := infoElectro.Tag * (FArmorPerc.ElectroArmor / Person.ArmorHealth);
  infoPsiFullArmor.Width := infoPsi.Tag * (FArmorPerc.PsiArmor / Person.ArmorHealth);
  infoPhisicFullArmor.Width := infoPhisic.Tag * (FArmorPerc.PhisicArmor / Person.ArmorHealth);
  infoFireFullArmor.Width := infoFire.Tag * (FArmorPerc.FireArmor / Person.ArmorHealth);

  infoLabradiation.Text := IfThen(FArmorPerc.RadiationArmor = 0, '', FArmorPerc.RadiationArmor.ToString + ' %');
  infoLabChimishe.Text := IfThen(FArmorPerc.ChimisheArmor = 0, '', FArmorPerc.ChimisheArmor.ToString + ' %');
  infoLabElectro.Text := IfThen(FArmorPerc.ElectroArmor = 0, '', FArmorPerc.ElectroArmor.ToString + ' %');
  infoLabPsi.Text := IfThen(FArmorPerc.PsiArmor = 0, '', FArmorPerc.PsiArmor.ToString + ' %');
  infoLabPhisic.Text := IfThen(FArmorPerc.PhisicArmor = 0, '', FArmorPerc.PhisicArmor.ToString + ' %');
  infoLabFire.Text := IfThen(FArmorPerc.FireArmor = 0, '', FArmorPerc.FireArmor.ToString + ' %');

  layClearArmorWeapon.Visible := true;
  layClearArmorWeapon.Enabled := Person.ArmorId <> 0;
  layRestoreArmorWeapon.Visible := true;
  btnRestoreArmorWeapon.Enabled := (Person.ArmorHealth < 100) and (Person.ArmorId <> 0);
  layClearArt.Visible := false;
  layPanel.Height := ImgBottom.Height + ImgTop.Height + layClearArmorWeapon.Height + layRestoreArmorWeapon.Height + imgPercs.Height;
  layInfo.Tag := 0;
  layPercs.Visible := true;
  layInfo.Visible := true;
end;

procedure TFramePercs.btnClearArmorWeaponClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  BtnClickMedia;
  if layInfo.Tag = 0 then
  begin
    ExeExec('insert into bag (table_name, row_id, health) select ''arts'', art_id, 100 from belt;', exExecute, vQuery);
    ExeExec('update users set armor_id = NULL, armor_health = 0;', exExecute, vQuery);
    ExeExec('delete from belt;', exExecute, vQuery);
    ExeExec('insert into bag (table_name, row_id, health) values (''armors'',' + Person.ArmorId.ToString + ',' + Person.ArmorHealth.ToString + ');', exExecute, vQuery);
  end
  else
  begin
    ExeExec('update users set weapon_id = NULL, weapon_health = 0;', exExecute, vQuery);
    ExeExec('insert into bag (table_name, row_id, health) values (''weapons'',' + Person.WeaponId.ToString + ',' + Person.WeaponHealth.ToString + ');', exExecute, vQuery);
  end;
  ReloadPercs;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnClearArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  BtnClickMedia;
  ExeExec('delete from belt where slot = ' + layInfo.Tag.ToString + ';', exExecute, vQuery);
  ExeExec('update belt set slot = slot - 1 where slot > ' + layInfo.Tag.ToString + ';', exExecute, vQuery);
  ExeExec('insert into bag (table_name, row_id, health) values (''arts'',' + FArtsList[layInfo.Tag - 1].ID.ToString + ', 100);', exExecute, vQuery);
  ReloadPercs;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnCloseInfoClick(Sender: TObject);
begin
  BtnClickMedia;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnClosePercsUpClick(Sender: TObject);
begin
  BtnClickMedia;
  layPercsUp.Visible := false;
end;

procedure TFramePercs.btnCloseQRClick(Sender: TObject);
begin
 layQR.Visible := false;
end;

procedure TFramePercs.btnCloseSelectionClick(Sender: TObject);
begin
  BtnClickMedia;
  laySelection.Visible := false;
end;

procedure TFramePercs.btnHelpClick(Sender: TObject);
var
  vQuery: TFDQuery;
  vSend: TSend;
  vStrSend: string;
  AMedic : TMedicData;
begin
  BtnClickMedia;
  layInfo.Visible := False;
  layQR.Visible := true;

  if not Assigned(FActiveAction) then
    FActiveAction := TAction.Create;

  FActiveAction.SendType := stMedic;

  AMedic := TMedicData.Create;
  try
    AMedic.Health := 30;
    AMedic.IsRestore := true;

    FActiveAction.JSONObject := TJson.ObjectToJsonString(AMedic);
  finally
    AMedic.Free;
  end;

  vSend := TSend.Create;
{$IFDEF ANDROID}
  vSend.Ip := GetMyIP;
{$ENDIF}
  vStrSend := TJson.ObjectToJsonString(vSend);
  vStrSend := StringReplace(vStrSend,'"code":"",','',[]);
  vStrSend := StringReplace(vStrSend,',"marker":null','',[]);
  GenerateQRCode(vStrSend, imgQR);
  laySelection.Visible := false;
end;

procedure TFramePercs.btnInfoClick(Sender: TObject);
begin
  BtnClickMedia;
  if FArtsList.Count >= (Sender as TSpeedButton).Tag then
  begin
    infoRadiation.Width := infoRadiation.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].RadiationArmor / 100;
    infoChimishe.Width := infoChimishe.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].ChimisheArmor / 100;
    infoElectro.Width := infoElectro.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].ElectroArmor / 100;
    infoPsi.Width := infoPsi.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].PsiArmor / 100;
    infoPhisic.Width := infoPhisic.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].PhisicArmor / 100;
    infoFire.Width := infoFire.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].FireArmor / 100;

    infoLabradiation.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].RadiationArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].RadiationArmor.ToString + ' %');
    infoLabChimishe.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].ChimisheArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].ChimisheArmor.ToString + ' %');
    infoLabElectro.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].ElectroArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].ElectroArmor.ToString + ' %');
    infoLabPsi.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].PsiArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].PsiArmor.ToString + ' %');
    infoLabPhisic.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].PhisicArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].PhisicArmor.ToString + ' %');
    infoLabFire.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag - 1].FireArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag - 1].FireArmor.ToString + ' %');

    infoRadiationFullArmor.Width := 0;
    infoChimisheFullArmor.Width := 0;
    infoElectroFullArmor.Width := 0;
    infoPsiFullArmor.Width := 0;
    infoPhisicFullArmor.Width := 0;
    infoFireFullArmor.Width := 0;

    layInfo.Tag := (Sender as TSpeedButton).Tag;
    layClearArt.Visible := true;
    layClearArmorWeapon.Visible := false;
    layRestoreArmorWeapon.Visible := false;
    layPercs.Visible := true;
    layPanel.Height := ImgBottom.Height + ImgTop.Height + layClearArt.Height + imgPercs.Height;
    layInfo.Visible := true;
  end;
end;

procedure TFramePercs.btnMedic1Click(Sender: TObject);
begin
  if layMedic1.Opacity = 1 then
  begin
    BtnClickMedia;
    layBtnRestore.Visible := false;
    laySelectionBody.Height := Image18.Height + Image25.Height + layBtnMedicMyself.Height + layBtnMedic.Height;
    laySelection.Visible := true;
  end;
end;

procedure TFramePercs.btnMedic2Click(Sender: TObject);
begin
  if layMedic2.Opacity = 1 then
  begin
    BtnClickMedia;
    layBtnRestore.Visible := false;
    laySelectionBody.Height := Image18.Height + Image25.Height + layBtnMedicMyself.Height + layBtnMedic.Height;
    laySelection.Visible := true;
  end;
end;

procedure TFramePercs.btnMedic3Click(Sender: TObject);
begin
if layMedic3.Opacity = 1 then
  begin
    BtnClickMedia;
    layBtnRestore.Visible := true;
    laySelectionBody.Height := Image18.Height + Image25.Height + layBtnMedicMyself.Height + layBtnRestore.Height + layBtnMedic.Height;
    laySelection.Visible := true;
  end;
end;

procedure TFramePercs.btnMedicClick(Sender: TObject);
var
  vSend: TSend;
  vStrSend: string;
  AMedic : TMedicData;
begin
  BtnClickMedia;
  layInfo.Visible := False;
  layQR.Visible := true;

  if not Assigned(FActiveAction) then
    FActiveAction := TAction.Create;

  FActiveAction.SendType := stMedic;

  AMedic := TMedicData.Create;
  try
     AMedic.IsRestore := false;

    case Person.LevelMedic of
      1: AMedic.Health := 30;
      2: AMedic.Health := 60;
      3: AMedic.Health := 100;
    end;

    FActiveAction.JSONObject := TJson.ObjectToJsonString(AMedic);
  finally
    AMedic.Free;
  end;

  vSend := TSend.Create;
{$IFDEF ANDROID}
  vSend.Ip := GetMyIP;
{$ENDIF}
  vStrSend := TJson.ObjectToJsonString(vSend);
  vStrSend := StringReplace(vStrSend,'"code":"",','',[]);
  vStrSend := StringReplace(vStrSend,',"marker":null','',[]);
  GenerateQRCode(vStrSend, imgQR);
  laySelection.Visible := false;
end;

procedure TFramePercs.btnMedicMyselfClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  BtnClickMedia;
  ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [10, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]),
      exExecute, vQuery);

  case Person.LevelMedic of
    1: Person.Health := 30;
    2: Person.Health := 60;
    3: Person.Health := 100;
  end;

  StartTimerMedicPercReload(NOW());
  laySelection.Visible := false;
end;

procedure TFramePercs.btnOpenDetectorClick(Sender: TObject);
begin
  BtnClickMedia;
  GoToDetector;
end;

function TFramePercs.GetTimeDec(ASeconds: integer): string;
var
  TotalSeconds: Int64;
  TimeSpan: TTimeSpan;
  Hours: integer;
  Minutes: integer;
  Seconds: integer;
begin
   TotalSeconds := ASeconds;

   // Создаем TTimeSpan
   TimeSpan := TTimeSpan.FromSeconds(TotalSeconds);

   // Получаем компоненты времени
   Hours := TimeSpan.Hours;
   Minutes := TimeSpan.Minutes;
   Seconds := TimeSpan.Seconds;

   Result := Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
end;

procedure TFramePercs.btnOpenPercsClick(Sender: TObject);
var
  vQuery: TFDQuery;
  vLastActionDateTime: TDateTime;
begin
  BtnClickMedia;
  layPercsUp.Visible := true;

  // Medic
  layMedic1.Opacity := 0.2;
  layMedic2.Opacity := 0.2;
  layMedic3.Opacity := 0.2;

  if Person.LevelMedic > 0 then
  begin
    (Self.FindComponent('layMedic' + Person.LevelMedic.ToString) as TLayout).Opacity := 1;
    layMedicReload.Parent := Self.FindComponent('layMedic' + Person.LevelMedic.ToString) as TLayout;

    ExeExec('select action_date_time from life_log where action_type_id = 10 order by action_date_time desc;', exActive, vQuery);

    if vQuery.RecordCount > 0 then
      begin
        vQuery.First;
        vLastActionDateTime := vQuery.FieldByName('action_date_time').AsDateTime;

        if  SecondsBetween(NOW(), vLastActionDateTime) > 30 * 60 then
          begin
            labReloadTimerMedic.Text := '00:00:00';
            layMedicReload.Visible := false;
            TimerPercReload.Enabled := false;
          end
          else
           begin
             StartTimerMedicPercReload(vLastActionDateTime);
           end;

      end;
  end;

  // Tehnic
  layTehnic1.Opacity := 0.2;
  layTehnic2.Opacity := 0.2;
  layTehnic3.Opacity := 0.2;

  if Person.LevelTehnic > 0 then
  begin
    (Self.FindComponent('layTehnic' + Person.LevelTehnic.ToString) as TLayout).Opacity := 1;
    layTehnicReload.Parent := Self.FindComponent('layTehnic' + Person.LevelTehnic.ToString) as TLayout;

    ExeExec('select action_date_time from life_log where action_type_id = 11 order by action_date_time desc;', exActive, vQuery);

    if vQuery.RecordCount > 0 then
      begin
        vQuery.First;
        vLastActionDateTime := vQuery.FieldByName('action_date_time').AsDateTime;

        if  SecondsBetween(NOW(), vLastActionDateTime) > 60 * 60 then
          begin
            labReloadTimerTehnic.Text := '00:00:00';
            layTehnicReload.Visible := false;
            TimerPercReload.Enabled := TimerPercReload.Enabled;
          end
          else
           begin
             StartTimerTehnicPercReload(vLastActionDateTime);
           end;
      end;
  end;
end;

procedure TFramePercs.StartTimerMedicPercReload(ALastActionDateTime: TDateTime);
begin
  layMedicReload.Visible := true;
  labReloadTimerMedic.Text := GetTimeDec(1 * 30 * 60 - SecondsBetween(NOW(), ALastActionDateTime));
  TimerPercReload.Enabled := true;
end;

procedure TFramePercs.StartTimerTehnicPercReload(ALastActionDateTime: TDateTime);
begin
  layTehnicReload.Visible := true;
  labReloadTimerTehnic.Text := GetTimeDec(1 * 60 * 60 - SecondsBetween(NOW(), ALastActionDateTime));
  TimerPercReload.Enabled := true;
end;

procedure TFramePercs.btnRestoreArmorWeaponClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  BtnClickMedia;

  if layInfo.Tag = 0 then
  begin
   if Person.LevelTehnic = 3 then
     begin
       ExeExec('update users set armor_health = 100;', exExecute, vQuery);
       Person.ArmorHealth := 100;

       ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [11, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]),
              exExecute, vQuery);

        StartTimerTehnicPercReload(NOW());
     end
   else
     // Здесь вызов сканера для ремонта
  end
  else
  begin
    if Person.WeaponLevel <= Person.LevelTehnic then
     begin
       Person.WeaponHealth := 100;
       ExeExec('update users set weapon_health = 100;', exExecute, vQuery);
       ExeExec(Format('insert into life_log (action_type_id, lat, lon) values (%d, %s, %s);', [11, StringReplace(FLocation.Latitude.ToString, ',', '.', [rfReplaceAll]), StringReplace(FLocation.Longitude.ToString, ',', '.', [rfReplaceAll])]),
              exExecute, vQuery);

       StartTimerTehnicPercReload(NOW());
     end
    else
     // Здесь вызов сканера для ремонта
  end;

  ReloadPercs;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnTehnic1Click(Sender: TObject);
var
  vSend: TSend;
  vStrSend: string;
begin
  BtnClickMedia;
  layQR.Visible := true;

  if not Assigned(FActiveAction) then
    FActiveAction := TAction.Create;

  FActiveAction.SendType := stTehnic;
  FActiveAction.JSONObject := Person.LevelTehnic.ToString;

  vSend := TSend.Create;
{$IFDEF ANDROID}
  vSend.Ip := GetMyIP;
{$ENDIF}
  vStrSend := TJson.ObjectToJsonString(vSend);
  vStrSend := StringReplace(vStrSend,'"code":"",','',[]);
  vStrSend := StringReplace(vStrSend,',"marker":null','',[]);
  GenerateQRCode(vStrSend, imgQR);
end;

procedure TFramePercs.btnWeaponInfoClick(Sender: TObject);
begin
  BtnClickMedia;
  layInfo.Tag := 1;
  layPercs.Visible := false;
  layClearArt.Visible := false;
  layClearArmorWeapon.Visible := true;
  layClearArmorWeapon.Enabled := Person.WeaponId <> 0;
  layRestoreArmorWeapon.Visible := true;
  btnRestoreArmorWeapon.Enabled := (Person.WeaponHealth < 100) and (Person.WeaponId <> 0);
  layPanel.Height := layClearArmorWeapon.Height + layRestoreArmorWeapon.Height + ImgBottom.Height + ImgTop.Height;
  layInfo.Visible := true;
end;

constructor TFramePercs.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  FArtsList := TList<TPerc>.Create;

  labRadiationArmor.TextSettings.Font.Family := 'lcd';
  labPsiArmor.TextSettings.Font.Family := 'lcd';
  labPhisicArmor.TextSettings.Font.Family := 'lcd';
  labFireArmor.TextSettings.Font.Family := 'lcd';
  labElectroArmor.TextSettings.Font.Family := 'lcd';
  labChimisheArmor.TextSettings.Font.Family := 'lcd';

  infoLabradiation.TextSettings.Font.Family := 'lcd';
  infoLabPsi.TextSettings.Font.Family := 'lcd';
  infoLabPhisic.TextSettings.Font.Family := 'lcd';
  infoLabFire.TextSettings.Font.Family := 'lcd';
  infoLabElectro.TextSettings.Font.Family := 'lcd';
  infoLabChimishe.TextSettings.Font.Family := 'lcd';

  btnCloseInfo.TextSettings.Font.Family := 'lcd';
  labReloadTimerMedic.TextSettings.Font.Family := 'montblancctt';
  labReloadTimerTehnic.TextSettings.Font.Family := 'montblancctt';

  ReloadPercs;
end;

procedure TFramePercs.ReloadPercs;
var
  vQuery: TFDQuery;
begin
  ReloadArmor;     // nickname,
  ExeExec('select   health, armor_health, weapon_health, weapon_icon, detector_id, level, radius, chimishe, electro, fire, phisic, psi, radiation, cash, armor_id, weapon_id, is_classic_bag, weapon_level, level_medic, level_tehnic from user_info;', exActive, vQuery);
  //Person.UserName := vQuery.FieldByName('nickname').AsString;
  Person.Health := vQuery.FieldByName('health').AsFloat;
  Person.Cash := vQuery.FieldByName('cash').AsInteger;
  Person.ArmorId := vQuery.FieldByName('armor_id').AsInteger;
  Person.IsClassicBag := vQuery.FieldByName('is_classic_bag').AsBoolean;
  Person.ArmorHealth := vQuery.FieldByName('armor_health').AsFloat;
  Person.WeaponId := vQuery.FieldByName('weapon_id').AsInteger;
  Person.WeaponLevel := vQuery.FieldByName('weapon_level').AsInteger;
  Person.WeaponHealth := vQuery.FieldByName('weapon_health').AsFloat;
  SetChimisheArmor(vQuery.FieldByName('chimishe').AsInteger);
  SetElectroArmor(vQuery.FieldByName('electro').AsInteger);
  SetFireArmor(vQuery.FieldByName('fire').AsInteger);
  SetPhisicArmor(vQuery.FieldByName('phisic').AsInteger);
  SetPsiArmor(vQuery.FieldByName('psi').AsInteger);
  SetRadiationArmor(vQuery.FieldByName('radiation').AsInteger);
  imgWeaponIcon.Bitmap.Assign(vQuery.FieldByName('weapon_icon'));
  SetDetector(vQuery.FieldByName('detector_id').AsInteger, vQuery.FieldByName('radius').AsInteger, vQuery.FieldByName('level').AsInteger);
  Person.LevelMedic := vQuery.FieldByName('level_medic').AsInteger;
  Person.LevelTehnic := vQuery.FieldByName('level_tehnic').AsInteger;

  FreeQueryAndConn(vQuery);
  ReloadArts;
end;

procedure TFramePercs.ReloadArmor;
var
  vQuery: TFDQuery;
begin
  ExeExec('select * from armors_data;', exActive, vQuery);

  if vQuery.RecordCount <> 0 then
  begin
    imgArmorIcon.Bitmap.Assign(vQuery.FieldByName('icon'));
    FArmorPerc.PhisicArmor := vQuery.FieldByName('phisic').AsInteger;
    FArmorPerc.RadiationArmor := vQuery.FieldByName('radiation').AsInteger;
    FArmorPerc.ElectroArmor := vQuery.FieldByName('electro').AsInteger;
    FArmorPerc.FireArmor := vQuery.FieldByName('fire').AsInteger;
    FArmorPerc.PsiArmor := vQuery.FieldByName('psi').AsInteger;
    FArmorPerc.ChimisheArmor := vQuery.FieldByName('chimishe').AsInteger;
    Person.CountContener := vQuery.FieldByName('count_slots').AsInteger;
  end
  else
  begin
    imgArmorIcon.Bitmap := nil;
    FArmorPerc.PhisicArmor := 0;
    FArmorPerc.RadiationArmor := 0;
    FArmorPerc.ElectroArmor := 0;
    FArmorPerc.FireArmor := 0;
    FArmorPerc.PsiArmor := 0;
    FArmorPerc.ChimisheArmor := 0;
    Person.CountContener := 0;
  end;
  FreeQueryAndConn(vQuery);
end;

procedure TFramePercs.ReloadArts;
var
  vQuery: TFDQuery;
  vPerc: TPerc;
  i: integer;
  vSlot: integer;
  vQuery2: TFDQuery;
begin
  FArtsList.Clear;

  for i := 1 to 5 do
  begin
    (FindComponent('imgGlass' + i.ToString) as TImage).Bitmap.Assign(GlassList.Source[0].MultiResBitmap[0].Bitmap);
    (FindComponent('imgSlot' + i.ToString) as TImage).Bitmap := nil;
    (FindComponent('btnSlot' + i.ToString + 'Info') as TSpeedButton).Visible := false;
  end;

  for i := 1 to Person.CountContener do
    (FindComponent('imgGlass' + i.ToString) as TImage).Bitmap := nil;

  ExeExec('select a.chimishe, a.electro, a.fire, a.phisic, a.psi, a.radiation, a.icon, ub.slot, a.art_id from belt ub join arts a on ub.art_id = a.art_id order by slot;', exActive, vQuery);

  while not vQuery.Eof do
  begin
    vSlot := vQuery.FieldByName('slot').AsInteger;

    if vSlot <= Person.CountContener then
    begin
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).Visible := true;
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).Tag := vSlot;
      (FindComponent('imgGlass' + vSlot.ToString) as TImage).Bitmap.Assign(GlassList.Source[1].MultiResBitmap[0].Bitmap);
      (FindComponent('imgSlot' + vSlot.ToString) as TImage).Bitmap.Assign(vQuery.FieldByName('icon'));
      vPerc.ID := vQuery.FieldByName('art_id').AsInteger;
      vPerc.PhisicArmor := vQuery.FieldByName('phisic').AsInteger;
      vPerc.RadiationArmor := vQuery.FieldByName('radiation').AsInteger;
      vPerc.ElectroArmor := vQuery.FieldByName('electro').AsInteger;
      vPerc.FireArmor := vQuery.FieldByName('fire').AsInteger;
      vPerc.PsiArmor := vQuery.FieldByName('psi').AsInteger;
      vPerc.ChimisheArmor := vQuery.FieldByName('chimishe').AsInteger;
      FArtsList.Add(vPerc);
    end
    else
      ExeExec('delete from belt where slot = ' + vSlot.ToString + ';', exExecute, vQuery2);

    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFramePercs.TimerPercReloadTimer(Sender: TObject);
var
  TimeParts: TArray<string>;
  Hours, Minutes, Seconds: Integer;
  TotalSeconds: Integer;

  function GetTimeDec(ATimeStr: string): string;
  begin
    // Разбиваем текущее время из Label на часы, минуты, секунды
    TimeParts := ATimeStr.Split([':']);

    if Length(TimeParts) = 3 then
    begin
      Hours := StrToIntDef(TimeParts[0], 0);
      Minutes := StrToIntDef(TimeParts[1], 0);
      Seconds := StrToIntDef(TimeParts[2], 0);

      // Переводим всё в секунды и уменьшаем на 1
      TotalSeconds := Hours * 3600 + Minutes * 60 + Seconds - 1;

      // Проверяем, не истекло ли время
      if TotalSeconds >= 0 then
      begin
        // Преобразуем обратно в часы, минуты, секунды
        Hours := TotalSeconds div 3600;
        Minutes := (TotalSeconds mod 3600) div 60;
        Seconds := TotalSeconds mod 60;

        Result := Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
      end;
    end;
  end;
begin

  if labReloadTimerMedic.Text = '00:00:00' then
    layMedicReload.Visible := false
  else
    labReloadTimerMedic.Text := GetTimeDec(labReloadTimerMedic.Text);

  if labReloadTimerTehnic.Text = '00:00:00' then
    layTehnicReload.Visible := false
  else
    labReloadTimerTehnic.Text := GetTimeDec(labReloadTimerTehnic.Text);

  TimerPercReload.Enabled := layMedicReload.Visible or layTehnicReload.Visible;
end;

procedure TFramePercs.SetDetector(ID, Radius, Level: integer);
var
  vDetector: TDetector;
begin
  vDetector.Radius := Radius;
  vDetector.Level := Level;
  Person.Detector := vDetector;

  imgDetector.Bitmap.Assign(DetectorList.Source[ID].MultiResBitmap[0].Bitmap);
end;

procedure TFramePercs.SetPsiArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.PsiArmor := Value;
  PsiArmor.Width := Value * PsiArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.PsiArmor + (FArmorPerc.PsiArmor / Person.ArmorHealth * 100);
  recPsiFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recPsiFull.Tag / 100;

  if Value = 0 then
    labPsiArmor.Text := ''
  else
    labPsiArmor.Text := Value.ToString + '%';

end;

procedure TFramePercs.SetFireArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.FireArmor := Value;
  FireArmor.Width := Value * FireArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.FireArmor + (FArmorPerc.FireArmor / Person.ArmorHealth * 100);
  recFireFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recFireFull.Tag / 100;

  if Value = 0 then
    labFireArmor.Text := ''
  else
    labFireArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetElectroArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.ElectroArmor := Value;
  ElectroArmor.Width := Value * ElectroArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.ElectroArmor + (FArmorPerc.ElectroArmor / Person.ArmorHealth * 100);
  recElectroFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recElectroFull.Tag / 100;

  if Value = 0 then
    labElectroArmor.Text := ''
  else
    labElectroArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetChimisheArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.ChimisheArmor := Value;
  ChimisheArmor.Width := Value * ChimisheArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.ChimisheArmor + (FArmorPerc.ChimisheArmor / Person.ArmorHealth * 100);
  recChimisheFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recChimisheFull.Tag / 100;

  if Value = 0 then
    labChimisheArmor.Text := ''
  else
    labChimisheArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetPhisicArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.PhisicArmor := Value;
  PhisicArmor.Width := Value * PhisicArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.PhisicArmor + (FArmorPerc.PhisicArmor / Person.ArmorHealth * 100);
  recPhisicFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recPhisicFull.Tag / 100;

  if Value = 0 then
    labPhisicArmor.Text := ''
  else
    labPhisicArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetRadiationArmor(Value: integer);
var
  vFullValue: single;
begin
  Person.RadiationArmor := Value;
  RadiationArmor.Width := Value * RadiationArmor.Tag / 100;
  vFullValue := Value - FArmorPerc.RadiationArmor + (FArmorPerc.RadiationArmor / Person.ArmorHealth * 100);
  recRadiationFull.Width := IfThen(vFullValue > 100, 100, vFullValue) * recRadiationFull.Tag / 100;

  if Value = 0 then
    labRadiationArmor.Text := ''
  else
    labRadiationArmor.Text := Value.ToString + '%';
end;

end.
