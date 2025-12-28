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
  FireDAC.Comp.Client;

type
  TFramePercs = class(TFrame)
    layLeftBlock: TLayout;
    LeftRicghtBlock: TLayout;
    Rectangle1: TRectangle;
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
    Layout3: TLayout;
    Image10: TImage;
    Image11: TImage;
    Image13: TImage;
    Image8: TImage;
    Layout4: TLayout;
    Image3: TImage;
    Image4: TImage;
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
    btnCloseInfo: TCornerButton;
    ImgArmorHealth: TImage;
    ArmorHealthProgress: TRectangle;
    Layout1: TLayout;
    Layout2: TLayout;
    Image6: TImage;
    WeaponHealthProgress: TRectangle;
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
    imgPersonHealth: TImage;
    HealthProgress: TRectangle;
    Image5: TImage;
    Image7: TImage;
    Image14: TImage;
    btnArmorInfo: TSpeedButton;
    InnerGlowEffect3: TInnerGlowEffect;
    Rectangle2: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    Rectangle3: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    layClearArmor: TLayout;
    Image1: TImage;
    btnClearArmor: TCornerButton;
    layRestoreArmor: TLayout;
    Image2: TImage;
    btnRestoreArmor: TCornerButton;
    layClearArt: TLayout;
    Image9: TImage;
    btnClearArt: TCornerButton;
    procedure btnInfoClick(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnArmorInfoClick(Sender: TObject);
    procedure btnOpenDetectorClick(Sender: TObject);
    procedure btnClearArmorClick(Sender: TObject);
    procedure btnClearArtClick(Sender: TObject);
  private
    { Private declarations }
    FArtsList: TList<TPerc>;
    FArmorPerc: TPerc;
    procedure ReloadArmor;

  public
    procedure ReloadArts;
    procedure ReloadPercs;
    procedure SetArmorHealth(Value: Double);
    procedure SetWeaponHealth(Value: Double);
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
  infoRadiation.Width := infoRadiation.Tag * FArmorPerc.RadiationArmor / 100;
  infoChimishe.Width := infoChimishe.Tag * FArmorPerc.ChimisheArmor / 100;
  infoElectro.Width := infoElectro.Tag * FArmorPerc.ElectroArmor / 100;
  infoPsi.Width := infoPsi.Tag * FArmorPerc.PsiArmor / 100;
  infoPhisic.Width := infoPhisic.Tag * FArmorPerc.PhisicArmor / 100;
  infoFire.Width := infoFire.Tag * FArmorPerc.FireArmor / 100;

  infoLabradiation.Text := IfThen(FArmorPerc.RadiationArmor = 0, '', FArmorPerc.RadiationArmor.ToString + ' %');
  infoLabChimishe.Text := IfThen(FArmorPerc.ChimisheArmor = 0, '', FArmorPerc.ChimisheArmor.ToString + ' %');
  infoLabElectro.Text := IfThen(FArmorPerc.ElectroArmor = 0, '', FArmorPerc.ElectroArmor.ToString + ' %');
  infoLabPsi.Text := IfThen(FArmorPerc.PsiArmor = 0, '', FArmorPerc.PsiArmor.ToString + ' %');
  infoLabPhisic.Text := IfThen(FArmorPerc.PhisicArmor = 0, '', FArmorPerc.PhisicArmor.ToString + ' %');
  infoLabFire.Text := IfThen(FArmorPerc.FireArmor = 0, '', FArmorPerc.FireArmor.ToString + ' %');

  layClearArmor.Visible := true;
  layRestoreArmor.Visible := true;
  btnRestoreArmor.Enabled := Person.ArmorHealth < 100;
  layClearArt.Visible := false;
  layInfo.Visible := true;
end;

procedure TFramePercs.btnClearArmorClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  ExeExec('insert into bag (table_name, row_id, health) select ''arts'', art_id, 100 from user_belt;', exExecute, vQuery);
  ExeExec('update users set armor_id = NULL, armor_health = 0 where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
  ExeExec('delete from user_belt where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
  ExeExec('insert into bag (table_name, row_id, health) values (''armors'',' + Person.ArmorId.ToString + ',' + Person.ArmorHealth.ToString + ');', exExecute, vQuery);
  ReloadPercs;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnClearArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  ExeExec('delete from user_belt where slot = ' + layInfo.Tag.ToString + ' and user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
  ExeExec('insert into bag (table_name, row_id, health) values (''arts'',' + FArtsList[layInfo.Tag -1].ID.ToString + ', 100);', exExecute, vQuery);
  ReloadPercs;
  layInfo.Visible := false;
end;

procedure TFramePercs.btnCloseInfoClick(Sender: TObject);
begin
  layInfo.Visible := false;
end;

procedure TFramePercs.btnInfoClick(Sender: TObject);
begin
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
    layInfo.Tag := (Sender as TSpeedButton).Tag;
    layClearArt.Visible := true;
    layClearArmor.Visible := false;
    layRestoreArmor.Visible := false;
    layInfo.Visible := true;
  end;
end;

procedure TFramePercs.btnOpenDetectorClick(Sender: TObject);
begin
  GoToDetector;
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
  ReloadPercs;
end;

procedure TFramePercs.ReloadPercs;
var
  vQuery: TFDQuery;
begin
  ExeExec('select health, armor_health, weapon_health, weapon_icon, detector_id, level, radius, chimishe, electro, fire, phisic, psi, radiation, cash, armor_id, weapon_id, is_classic_bag  from user_info where user_id = ' + Person.UserId.ToString +
    ';', exActive, vQuery);
  Person.Health := vQuery.FieldByName('health').AsFloat;
  Person.Cash := vQuery.FieldByName('cash').AsInteger;
  Person.ArmorId := vQuery.FieldByName('armor_id').AsInteger;
  Person.IsClassicBag := vQuery.FieldByName('is_classic_bag').AsBoolean;
  SetArmorHealth(vQuery.FieldByName('armor_health').AsFloat);
  Person.WeaponId := vQuery.FieldByName('weapon_id').AsInteger;
  SetWeaponHealth(vQuery.FieldByName('weapon_health').AsFloat);
  SetChimisheArmor(vQuery.FieldByName('chimishe').AsInteger);
  SetElectroArmor(vQuery.FieldByName('electro').AsInteger);
  SetFireArmor(vQuery.FieldByName('fire').AsInteger);
  SetPhisicArmor(vQuery.FieldByName('phisic').AsInteger);
  SetPsiArmor(vQuery.FieldByName('psi').AsInteger);
  SetRadiationArmor(vQuery.FieldByName('radiation').AsInteger);
  imgWeaponIcon.Bitmap.Assign(vQuery.FieldByName('weapon_icon'));
  SetDetector(vQuery.FieldByName('detector_id').AsInteger, vQuery.FieldByName('radius').AsInteger, vQuery.FieldByName('level').AsInteger);

  FreeQueryAndConn(vQuery);
  ReloadArts;
end;

procedure TFramePercs.ReloadArmor;
var
  vQuery: TFDQuery;
begin
  FArtsList.Clear;
  ExeExec('select * from armors_data where user_id = ' + Person.UserId.ToString + ';', exActive, vQuery);

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
  ReloadArmor;

  for i := 1 to 5 do
  begin
    (FindComponent('imgGlass' + i.ToString) as TImage).Bitmap.Assign(GlassList.Source[0].MultiResBitmap[0].Bitmap);
    (FindComponent('imgSlot' + i.ToString) as TImage).Bitmap := nil;
    (FindComponent('btnSlot' + i.ToString + 'Info') as TSpeedButton).Visible := false;
  end;

  for i := 1 to Person.CountContener do
    (FindComponent('imgGlass' + i.ToString) as TImage).Bitmap := nil;

  ExeExec('select a.chimishe, a.electro, a.fire, a.phisic, a.psi, a.radiation, a.icon, ub.slot, a.art_id from arts a join user_belt ub on ub.art_id = a.art_id where user_id = ' + Person.UserId.ToString + ' order by slot;', exActive, vQuery);

  while not vQuery.Eof do
  begin
    vSlot := vQuery.FieldByName('slot').AsInteger;

    if vSlot <= Person.CountContener then
    begin
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).Visible := true;
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).Tag := vSlot;

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
      ExeExec('delete from user_belt where slot = ' + vSlot.ToString + ' and user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery2);

    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFramePercs.SetArmorHealth(Value: Double);
begin
  ArmorHealthProgress.Width := Value * ArmorHealthProgress.Tag / 100;
  Person.ArmorHealth := Value;

  if Value < 33 then
    ArmorHealthProgress.Fill.Color := cCriticalColor
  else if Value < 66 then
    ArmorHealthProgress.Fill.Color := cNormalColor
  else
    ArmorHealthProgress.Fill.Color := cFullColor;
end;

procedure TFramePercs.SetWeaponHealth(Value: Double);
begin
  WeaponHealthProgress.Width := Value * WeaponHealthProgress.Tag / 100;
  Person.WeaponHealth := Value;

  if Value < 33 then
    WeaponHealthProgress.Fill.Color := cCriticalColor
  else if Value < 66 then
    WeaponHealthProgress.Fill.Color := cNormalColor
  else
    WeaponHealthProgress.Fill.Color := cFullColor;
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
begin
  Person.PsiArmor := Value;
  PsiArmor.Width := Value * PsiArmor.Tag / 100;

  if Value = 0 then
    labPsiArmor.Text := ''
  else
    labPsiArmor.Text := Value.ToString + '%';

end;

procedure TFramePercs.SetFireArmor(Value: integer);
begin
  Person.FireArmor := Value;
  FireArmor.Width := Value * FireArmor.Tag / 100;

  if Value = 0 then
    labFireArmor.Text := ''
  else
    labFireArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetElectroArmor(Value: integer);
begin
  Person.ElectroArmor := Value;
  ElectroArmor.Width := Value * ElectroArmor.Tag / 100;

  if Value = 0 then
    labElectroArmor.Text := ''
  else
    labElectroArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetChimisheArmor(Value: integer);
begin
  Person.ChimisheArmor := Value;
  ChimisheArmor.Width := Value * ChimisheArmor.Tag / 100;

  if Value = 0 then
    labChimisheArmor.Text := ''
  else
    labChimisheArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetPhisicArmor(Value: integer);
begin
  Person.PhisicArmor := Value;
  PhisicArmor.Width := Value * PhisicArmor.Tag / 100;

  if Value = 0 then
    labPhisicArmor.Text := ''
  else
    labPhisicArmor.Text := Value.ToString + '%';
end;

procedure TFramePercs.SetRadiationArmor(Value: integer);
begin
  Person.RadiationArmor := Value;
  RadiationArmor.Width := Value * RadiationArmor.Tag / 100;

  if Value = 0 then
    labRadiationArmor.Text := ''
  else
    labRadiationArmor.Text := Value.ToString + '%';
end;

end.
