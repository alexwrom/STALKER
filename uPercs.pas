unit uPercs;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Effects, FMX.Objects, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListBox, FMX.ListView, Generics.Collections, uGlobal,
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
    Image9: TImage;
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
    Image15: TImage;
    Image16: TImage;
    Layout9: TLayout;
    Image20: TImage;
    DetectorList: TImageList;
    btnOpenDetector: TSpeedButton;
    imgDetector: TImage;
    ImgSlot1: TImage;
    ImgSlot2: TImage;
    ImgSlot3: TImage;
    ImgSlot4: TImage;
    ImgSlot5: TImage;
    Layout8: TLayout;
    Image1: TImage;
    imgWeaponIcon: TImage;
    btnWeaponInfo: TSpeedButton;
    Image19: TImage;
    Layout11: TLayout;
    Image12: TImage;
    imgArmorIcon: TImage;
    btnArmorInfo: TSpeedButton;
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
    Image2: TImage;
    Image5: TImage;
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
    procedure btnInfoClick(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnArmorInfoClick(Sender: TObject);
    procedure btnOpenDetectorClick(Sender: TObject);
  private
    { Private declarations }
    FArtsList: TList<TPerc>;
    FArmorPerc: TPerc;
    procedure ReloadArts;
    procedure ReloadPercs;
  public

    procedure SetArmorHealth(Value: integer);
    procedure SetWeaponHealth(Value: integer);
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

  infoLabRadiation.Text := IfThen(FArmorPerc.RadiationArmor = 0, '', FArmorPerc.RadiationArmor.ToString + ' %');
    infoLabChimishe.Text := IfThen(FArmorPerc.ChimisheArmor = 0, '', FArmorPerc.ChimisheArmor.ToString + ' %');
    infoLabElectro.Text := IfThen(FArmorPerc.ElectroArmor = 0, '', FArmorPerc.ElectroArmor.ToString + ' %');
    infoLabPsi.Text := IfThen(FArmorPerc.PsiArmor = 0, '', FArmorPerc.PsiArmor.ToString + ' %');
    infoLabPhisic.Text := IfThen(FArmorPerc.PhisicArmor = 0, '', FArmorPerc.PhisicArmor.ToString + ' %');
    infoLabFire.Text := IfThen(FArmorPerc.FireArmor = 0, '', FArmorPerc.FireArmor.ToString + ' %');
  layInfo.Visible := true;
end;

procedure TFramePercs.btnCloseInfoClick(Sender: TObject);
begin
  layInfo.Visible := false;
end;

procedure TFramePercs.btnInfoClick(Sender: TObject);
begin
  if FArtsList.Count >= (Sender as TSpeedButton).Tag + 1 then
  begin
    infoRadiation.Width := infoRadiation.Tag * FArtsList[(Sender as TSpeedButton).Tag].RadiationArmor / 100;
    infoChimishe.Width := infoChimishe.Tag * FArtsList[(Sender as TSpeedButton).Tag].ChimisheArmor / 100;
    infoElectro.Width := infoElectro.Tag * FArtsList[(Sender as TSpeedButton).Tag].ElectroArmor / 100;
    infoPsi.Width := infoPsi.Tag * FArtsList[(Sender as TSpeedButton).Tag].PsiArmor / 100;
    infoPhisic.Width := infoPhisic.Tag * FArtsList[(Sender as TSpeedButton).Tag].PhisicArmor / 100;
    infoFire.Width := infoFire.Tag * FArtsList[(Sender as TSpeedButton).Tag].FireArmor / 100;

    infoLabRadiation.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].RadiationArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].RadiationArmor.ToString + ' %');
    infoLabChimishe.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].ChimisheArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].ChimisheArmor.ToString + ' %');
    infoLabElectro.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].ElectroArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].ElectroArmor.ToString + ' %');
    infoLabPsi.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].PsiArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].PsiArmor.ToString + ' %');
    infoLabPhisic.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].PhisicArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].PhisicArmor.ToString + ' %');
    infoLabFire.Text := IfThen(FArtsList[(Sender as TSpeedButton).Tag].FireArmor = 0, '', FArtsList[(Sender as TSpeedButton).Tag].FireArmor.ToString + ' %');
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
  ReloadPercs;
end;

procedure TFramePercs.ReloadPercs;
var
  vQuery: TFDQuery;
begin
  ExeExec('select health, armor_health, weapon_health, weapon_icon, detector_id, level, radius, chimishe, electro, fire, phisic, psi, radiation from user_info where user_id = ' + Person.UserId.ToString + ';',
    exActive, vQuery);
  Person.Health := vQuery.FieldByName('health').AsInteger;
  SetArmorHealth(vQuery.FieldByName('armor_health').AsInteger);
  SetWeaponHealth(vQuery.FieldByName('weapon_health').AsInteger);
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

procedure TFramePercs.ReloadArts;
var
  vQuery: TFDQuery;
  vPerc: TPerc;
  i: integer;
  vSlot: integer;
begin
  FArtsList.Clear;
  ExeExec('select * from armors_data where user_id = ' + Person.UserId.ToString + ';', exActive, vQuery);
  imgArmorIcon.Bitmap.Assign(vQuery.FieldByName('icon'));
  FArmorPerc.PhisicArmor := vQuery.FieldByName('phisic').AsInteger;
  FArmorPerc.RadiationArmor := vQuery.FieldByName('radiation').AsInteger;
  FArmorPerc.ElectroArmor := vQuery.FieldByName('electro').AsInteger;
  FArmorPerc.FireArmor := vQuery.FieldByName('fire').AsInteger;
  FArmorPerc.PsiArmor := vQuery.FieldByName('psi').AsInteger;
  FArmorPerc.ChimisheArmor := vQuery.FieldByName('chimishe').AsInteger;

  for i := 1 to 5 do
  begin
    (FindComponent('imgGlass' + i.toString) as TImage).Bitmap.Assign(GlassList.Source[5].MultiResBitmap[0].Bitmap);
    (FindComponent('imgSlot' + i.toString) as TImage).Bitmap := nil;
    (FindComponent('btnSlot' + i.toString + 'Info') as TSpeedButton).Visible := false;
  end;

  for i := 1 to vQuery.FieldByName('count_slots').AsInteger do
    (FindComponent('imgGlass' + i.toString) as TImage).Bitmap.Assign(GlassList.Source[i - 1].MultiResBitmap[0].Bitmap);

  ExeExec('select a.chimishe, a.electro, a.fire, a.phisic, a.psi, a.radiation, a.icon, ub.slot from arts a join user_belt ub on ub.art_id = a.art_id where user_id = ' + Person.UserId.ToString + ' order by slot;',
    exActive, vQuery);

  while not vQuery.Eof do
  begin
    vSlot := vQuery.FieldByName('slot').AsInteger;
    (FindComponent('imgGlass' + vSlot.toString) as TImage).Bitmap.Assign(GlassList.Source[vSlot - 1].MultiResBitmap[0].Bitmap);
    (FindComponent('btnSlot' + vSlot.toString + 'Info') as TSpeedButton).Visible := true;
    (FindComponent('btnSlot' + vSlot.toString + 'Info') as TSpeedButton).Tag := vSlot - 1;

    (FindComponent('imgSlot' + vSlot.toString) as TImage).Bitmap.Assign(vQuery.FieldByName('icon'));
    vPerc.PhisicArmor := vQuery.FieldByName('phisic').AsInteger;
    vPerc.RadiationArmor := vQuery.FieldByName('radiation').AsInteger;
    vPerc.ElectroArmor := vQuery.FieldByName('electro').AsInteger;
    vPerc.FireArmor := vQuery.FieldByName('fire').AsInteger;
    vPerc.PsiArmor := vQuery.FieldByName('psi').AsInteger;
    vPerc.ChimisheArmor := vQuery.FieldByName('chimishe').AsInteger;
    FArtsList.Add(vPerc);

    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFramePercs.SetArmorHealth(Value: integer);
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

procedure TFramePercs.SetWeaponHealth(Value: integer);
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
  PsiArmor.Width := Value * PsiArmor.Tag / 100;

  if Value = 0 then
    labPsiArmor.Text := ''
  else
    labPsiArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

procedure TFramePercs.SetFireArmor(Value: integer);
begin
  FireArmor.Width := Value * FireArmor.Tag / 100;

   if Value = 0 then
    labFireArmor.Text := ''
  else
    labFireArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

procedure TFramePercs.SetElectroArmor(Value: integer);
begin
  ElectroArmor.Width := Value * ElectroArmor.Tag / 100;

  if Value = 0 then
    labElectroArmor.Text := ''
  else
    labElectroArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

procedure TFramePercs.SetChimisheArmor(Value: integer);
begin
  ChimisheArmor.Width := Value * ChimisheArmor.Tag / 100;

  if Value = 0 then
    labChimisheArmor.Text := ''
  else
    labChimisheArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

procedure TFramePercs.SetPhisicArmor(Value: integer);
begin
  PhisicArmor.Width := Value * PhisicArmor.Tag / 100;

  if Value = 0 then
    labPhisicArmor.Text := ''
  else
    labPhisicArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

procedure TFramePercs.SetRadiationArmor(Value: integer);
begin
  RadiationArmor.Width := Value * RadiationArmor.Tag / 100;

  if Value = 0 then
    labRadiationArmor.Text := ''
  else
    labRadiationArmor.Text := Value.ToString + '%';

  Person.WeaponHealth := Value;
end;

end.
