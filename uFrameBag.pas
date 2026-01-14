unit uFrameBag;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uGlobal, System.ImageList, FMX.ImgList, Generics.Collections,
  FMX.Controls.Presentation, FMX.Effects, StrUtils, FireDAC.Comp.Client, uScanerWiFi, Classes.send, Classes.sell,
  Rest.JSON, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdGlobal, System.Net.URLClient, Classes.action, FMX.TabControl;

type
  TFrameBag = class(TFrame)
    VertScrollBox: TVertScrollBox;
    imgList63x63: TImageList;
    imgList63x126: TImageList;
    imgList126x189: TImageList;
    imgList189x126: TImageList;
    layTopBorder: TLayout;
    flBackground: TFlowLayout;
    flElements: TFlowLayout;
    layBag: TLayout;
    layInfo: TLayout;
    recBackground: TRectangle;
    layPanel: TLayout;
    imgBottom: TImage;
    imgTop: TImage;
    Image13: TImage;
    Image8: TImage;
    imgPercsInfo: TImage;
    infoRadiation: TRectangle;
    infoChimishe: TRectangle;
    infoElectro: TRectangle;
    infoPsi: TRectangle;
    infoPhisic: TRectangle;
    infoFire: TRectangle;
    infoLabChimishe: TLabel;
    infoLabElectro: TLabel;
    infoLabradiation: TLabel;
    infoLabPhisic: TLabel;
    infoLabFire: TLabel;
    infoLabPsi: TLabel;
    Image7: TImage;
    Label4: TLabel;
    SwitchStyle: TSwitch;
    recCash: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    labCash: TLabel;
    Label1: TLabel;
    recCountSlots: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    Image4: TImage;
    Label2: TLabel;
    labCountSlots: TLabel;
    recHealth: TRectangle;
    ImgHealth: TImage;
    HealthProgress: TRectangle;
    recHealthRestore: TRectangle;
    InnerGlowEffect6: TInnerGlowEffect;
    Image5: TImage;
    Label3: TLabel;
    labHealthRestore: TLabel;
    layChangeSlot: TLayout;
    Rectangle3: TRectangle;
    Layout3: TLayout;
    Image9: TImage;
    Image12: TImage;
    Image14: TImage;
    Image15: TImage;
    ImgPercsArt: TImage;
    recRadiationArt: TRectangle;
    recChimisheArt: TRectangle;
    recElectroArt: TRectangle;
    recPsiArt: TRectangle;
    recPhisicArt: TRectangle;
    recFireArt: TRectangle;
    Rectangle12: TRectangle;
    InnerGlowEffect7: TInnerGlowEffect;
    Layout8: TLayout;
    Image20: TImage;
    btnChooseArt: TCornerButton;
    Rectangle16: TRectangle;
    InnerGlowEffect10: TInnerGlowEffect;
    layArts: TLayout;
    Image22: TImage;
    GridPanelLayout1: TGridPanelLayout;
    laySlot1: TLayout;
    ImgGlass1: TImage;
    ImgSlot1: TImage;
    btnSlot1Info: TSpeedButton;
    laySlot2: TLayout;
    ImgGlass2: TImage;
    ImgSlot2: TImage;
    btnSlot2Info: TSpeedButton;
    laySlot3: TLayout;
    ImgGlass3: TImage;
    ImgSlot3: TImage;
    btnSlot3Info: TSpeedButton;
    laySlot4: TLayout;
    ImgGlass4: TImage;
    ImgSlot4: TImage;
    btnSlot4Info: TSpeedButton;
    laySlot5: TLayout;
    ImgGlass5: TImage;
    ImgSlot5: TImage;
    btnSlot5Info: TSpeedButton;
    Rectangle13: TRectangle;
    InnerGlowEffect8: TInnerGlowEffect;
    imgArt: TImage;
    Label12: TLabel;
    labArtName: TLabel;
    recChimisheChangeArt: TRectangle;
    recElectroChangeArt: TRectangle;
    recFireChangeArt: TRectangle;
    recPsiChangeArt: TRectangle;
    recPhisicChangeArt: TRectangle;
    recRadiationChangeArt: TRectangle;
    GlassList: TImageList;
    igfSelect: TInnerGlowEffect;
    infoChimisheFullArmor: TRectangle;
    infoElectroFullArmor: TRectangle;
    infoFireFullArmor: TRectangle;
    infoPhisicFullArmor: TRectangle;
    infoPsiFullArmor: TRectangle;
    infoRadiationFullArmor: TRectangle;
    Layout1: TLayout;
    Rectangle5: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    layAddArmor: TLayout;
    Image1: TImage;
    btnAddArmor: TCornerButton;
    layUse: TLayout;
    Image2: TImage;
    btnUse: TCornerButton;
    layAddArt: TLayout;
    Image6: TImage;
    btnAddArt: TCornerButton;
    laySells: TLayout;
    Image18: TImage;
    bntSells: TCornerButton;
    recCost: TRectangle;
    InnerGlowEffect12: TInnerGlowEffect;
    labElementCost: TLabel;
    Label6: TLabel;
    TabControl: TTabControl;
    TabClassic: TTabItem;
    TabSection: TTabItem;
    InnerGlowEffect13: TInnerGlowEffect;
    VertScrollBox2: TVertScrollBox;
    Image10: TImage;
    Image11: TImage;
    Layout4: TLayout;
    Image19: TImage;
    Layout6: TLayout;
    Image21: TImage;
    layWeaponLayouts: TLayout;
    Layout7: TLayout;
    flWeaponsBackgroud: TFlowLayout;
    HorzScrollBox1: THorzScrollBox;
    flWeapons: TFlowLayout;
    layArmorsLayuots: TLayout;
    Layout10: TLayout;
    flArmorsBackground: TFlowLayout;
    HorzScrollBox2: THorzScrollBox;
    flArmors: TFlowLayout;
    layTop: TLayout;
    layTopLeft: TLayout;
    Layout11: TLayout;
    Image23: TImage;
    layMedicalLayouts: TLayout;
    Layout12: TLayout;
    flMedicalBackground: TFlowLayout;
    HorzScrollBox4: THorzScrollBox;
    flMedical: TFlowLayout;
    layTopClient: TLayout;
    layArtefacsLayouts: TLayout;
    Layout13: TLayout;
    flArtsBackground: TFlowLayout;
    HorzScrollBox3: THorzScrollBox;
    flArts: TFlowLayout;
    Layout14: TLayout;
    Image24: TImage;
    Image26: TImage;
    recSkin: TRectangle;
    recSkin1: TRectangle;
    InnerGlowEffect14: TInnerGlowEffect;
    recSkin2: TRectangle;
    Layout2: TLayout;
    Rectangle2: TRectangle;
    InnerGlowEffect15: TInnerGlowEffect;
    Image29: TImage;
    recSkin3: TRectangle;
    recSkin4: TRectangle;
    Layout15: TLayout;
    Image30: TImage;
    laySellQR: TLayout;
    recSkin5: TRectangle;
    InnerGlowEffect16: TInnerGlowEffect;
    imgQR: TImage;
    btnCloseChangeSlot: TSpeedButton;
    btnCloseInfo: TSpeedButton;
    btnCloseQR: TSpeedButton;
    InnerGlowEffect9: TInnerGlowEffect;
    InnerGlowEffect11: TInnerGlowEffect;
    InnerGlowEffect17: TInnerGlowEffect;
    Layout5: TLayout;
    Layout9: TLayout;
    Layout16: TLayout;
    recBackArt: TRectangle;
    layPercs: TLayout;
    recBackInfo: TRectangle;
    procedure SwitchStyleSwitch(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnAddArmorClick(Sender: TObject);
    procedure btnUseClick(Sender: TObject);
    procedure btnAddArtClick(Sender: TObject);
    procedure btnCloseChangeSlotClick(Sender: TObject);
    procedure btnChooseArtClick(Sender: TObject);
    procedure bntSellsClick(Sender: TObject);
    procedure btnCloseQRClick(Sender: TObject);
  private
    FArtsList: TList<TPerc>;

    procedure CreateFreeCell(ALayout: TFlowLayout);
    procedure OnClickElement(Sender: TObject);
    procedure SetLayBagHeight;
    procedure ReloadArts;
    procedure btnArtClick(Sender: TObject);

    procedure CreateBagBackgroundSection;
    procedure ClearElementsSection;
    procedure Compare(AOld, AChange: TRectangle);

    { Private declarations }
  public
    { Public declarations }
    FActiveAction: TAction;
    procedure LoadBagElements;
    procedure CreateElements(AIsClassicBag: boolean);
    procedure CreateBagBackground;
    constructor Create(AObject: TFmxObject);
  end;

implementation

{$R *.fmx}

constructor TFrameBag.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  labCash.TextSettings.Font.Family := 'lcd';
  infoLabChimishe.TextSettings.Font.Family := 'lcd';
  infoLabElectro.TextSettings.Font.Family := 'lcd';
  infoLabFire.TextSettings.Font.Family := 'lcd';
  infoLabPhisic.TextSettings.Font.Family := 'lcd';
  infoLabPsi.TextSettings.Font.Family := 'lcd';
  infoLabradiation.TextSettings.Font.Family := 'lcd';
  labElementCost.TextSettings.Font.Family := 'lcd';

  FArtsList := TList<TPerc>.Create;
end;

procedure TFrameBag.CreateBagBackground;
var
  i: integer;
begin
  for i := 1 to Trunc(layBag.Width / 63) * Trunc(layBag.Height / 63) do
  begin
    CreateFreeCell(flBackground);
  end;
end;

procedure TFrameBag.SetLayBagHeight;
var
  vMaxHeight: Single;
  i: integer;
begin
  vMaxHeight := 0;

  for i := 0 to flElements.ControlsCount - 1 do
    if (flElements.Controls[i] is TImage) then
      if (flElements.Controls[i] as TImage).Position.Y + (flElements.Controls[i] as TImage).Height > vMaxHeight then
        vMaxHeight := (flElements.Controls[i] as TImage).Position.Y + (flElements.Controls[i] as TImage).Height;

  if vMaxHeight > VertScrollBox.Height then
    layBag.Height := vMaxHeight
  else
    layBag.Height := VertScrollBox.Height;
end;

procedure TFrameBag.SwitchStyleSwitch(Sender: TObject);
begin
  Person.IsClassicBag := SwitchStyle.IsChecked;

  if Person.IsClassicBag then
    TabControl.ActiveTab := TabClassic
  else
    TabControl.ActiveTab := TabSection;
end;

procedure TFrameBag.LoadBagElements;
begin
  CreateBagBackgroundSection;
  CreateElements(False);
end;

procedure TFrameBag.CreateBagBackgroundSection;
var
  i: integer;
begin
  if flMedicalBackground.ChildrenCount = 0 then
  begin
    for i := 1 to Round(flWeaponsBackgroud.Width / 63) * 2 do
    begin
      CreateFreeCell(flWeaponsBackgroud);
    end;

    for i := 1 to Round(flArmorsBackground.Width / 63) * 3 do
    begin
      CreateFreeCell(flArmorsBackground);
    end;

    for i := 1 to Round(flArtsBackground.Width / 63) do
    begin
      CreateFreeCell(flArtsBackground);
    end;

    for i := 1 to 3 do
    begin
      CreateFreeCell(flMedicalBackground);
    end;
  end;
end;

procedure TFrameBag.CreateFreeCell(ALayout: TFlowLayout);
begin
  with TImage.Create(ALayout) do
  begin
    Parent := ALayout;
    Width := 63;
    Height := 63;
    Bitmap.Assign(imgList63x63.Source[1].MultiResBitmap[0].Bitmap);
    HitTest := False;
    Opacity := 0.95;
  end;
end;

procedure TFrameBag.ClearElementsSection;
var
  i: integer;
begin
  for i := 0 to flMedical.ChildrenCount - 1 do
  begin
    (flMedical.Children[0] as TImage).Visible := False;
    FreeAndNil(flMedical.Children[0]);
  end;

  for i := 0 to flWeapons.ChildrenCount - 1 do
  begin
    (flWeapons.Children[0] as TImage).Visible := False;
    FreeAndNil(flWeapons.Children[0]);
  end;

  for i := 0 to flArmors.ChildrenCount - 1 do
  begin
    (flArmors.Children[0] as TImage).Visible := False;
    FreeAndNil(flArmors.Children[0]);
  end;

  for i := 0 to flArts.ChildrenCount - 1 do
  begin
    (flArts.Children[0] as TImage).Visible := False;
    FreeAndNil(flArts.Children[0]);
  end;
end;

procedure TFrameBag.bntSellsClick(Sender: TObject);
var
  vSend: TSend;
  vSell: TSell;
begin
  layInfo.Visible := False;
  laySellQR.Visible := true;

  if not Assigned(FActiveAction) then
    FActiveAction := TAction.Create;

  FActiveAction.SendType := stSell;
  vSell := TSell.Create;
  vSell.TableName := FBagList[layInfo.Tag].TableName;
  vSell.RowID := FBagList[layInfo.Tag].RowID;
  vSell.Health := FBagList[layInfo.Tag].Health;
  vSell.Cost := FBagList[layInfo.Tag].Cost;

  FActiveAction.JSONObject := TJson.ObjectToJsonString(vSell);

  vSend := TSend.Create;
{$IFDEF ANDROID}
  vSend.Ip := GetMyIP;
{$ENDIF}
  GenerateQRCode(TJson.ObjectToJsonString(vSend), imgQR);
end;

procedure TFrameBag.btnAddArmorClick(Sender: TObject);
var
  vIndex: integer;
  vQuery: TFDQuery;
begin
  vIndex := layInfo.Tag;
  case FBagList[vIndex].BagType of
    btArmor:
      begin
        if Person.ArmorId <> 0 then
          ExeExec('insert into bag (table_name, row_id, health) values (''armors'',' + Person.ArmorId.ToString + ',' + Person.ArmorHealth.ToString + ');', exExecute, vQuery);

        ExeExec('update users set armor_id = ' + FBagList[vIndex].RowID.ToString + ', armor_health = ' + FBagList[vIndex].Health.ToString + ' where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
        ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''armors'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' and health = ' + FBagList[vIndex].Health.ToString + ' limit 1);', exExecute, vQuery);

        ExeExec('insert into bag (table_name, row_id, health) select ''arts'', art_id, 100 from belt where slot > ' + labCountSlots.Text + ';', exExecute, vQuery);
        ExeExec('delete from belt where slot > ' + labCountSlots.Text + ';', exExecute, vQuery);
      end;
    btWeapon:
      begin
        if Person.WeaponId <> 0 then
          ExeExec('insert into bag (table_name, row_id, health) values (''weapons'',' + Person.WeaponId.ToString + ',' + Person.WeaponHealth.ToString + ');', exExecute, vQuery);

        ExeExec('update users set weapon_id = ' + FBagList[vIndex].RowID.ToString + ', weapon_health = ' + FBagList[vIndex].Health.ToString + ' where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
        ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''weapons'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' and health = ' + FBagList[vIndex].Health.ToString + ' limit 1);', exExecute, vQuery);
      end;
  end;
  layInfo.Visible := False;
  ReloadPercs;
  try
    ReloadBag;
  finally
  end;
end;

procedure TFrameBag.btnAddArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
  vIndex: integer;
begin
  vIndex := layInfo.Tag;

  if IsFullBelt then
  begin
    imgArt.Bitmap.Assign(FBagList[vIndex].Icon);
    ExeExec('select * from arts where art_id = ' + FBagList[vIndex].RowID.ToString + ';', exActive, vQuery);

    labArtName.Text := vQuery.FieldByName('art_name').AsString;
    recPsiArt.Width := recPsiArt.Tag * vQuery.FieldByName('psi').AsFloat / 100;
    recPhisicArt.Width := recPhisicArt.Tag * vQuery.FieldByName('phisic').AsFloat / 100;
    recFireArt.Width := recFireArt.Tag * vQuery.FieldByName('fire').AsFloat / 100;
    recRadiationArt.Width := recRadiationArt.Tag * vQuery.FieldByName('radiation').AsFloat / 100;
    recChimisheArt.Width := recChimisheArt.Tag * vQuery.FieldByName('chimishe').AsFloat / 100;
    recElectroArt.Width := recElectroArt.Tag * vQuery.FieldByName('electro').AsFloat / 100;
    FreeQueryAndConn(vQuery);

    recRadiationChangeArt.Width := 0;
    recChimisheChangeArt.Width := 0;
    recElectroChangeArt.Width := 0;
    recPsiChangeArt.Width := 0;
    recPhisicChangeArt.Width := 0;
    recFireChangeArt.Width := 0;

    ImgPercsArt.BringToFront;

    ReloadArts;
    layChangeSlot.Visible := true;
  end
  else
  begin
    ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''arts'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' limit 1);', exExecute, vQuery);
    ExeExec('insert into belt (art_id, slot) values (' + FBagList[vIndex].RowID.ToString + ', (select count(1) + 1 from belt));', exExecute, vQuery);
    ReloadPercs;
    ReloadBag;
  end;
  layInfo.Visible := False;
end;

procedure TFrameBag.ReloadArts;
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
    (FindComponent('btnSlot' + i.ToString + 'Info') as TSpeedButton).Visible := False;
  end;

  for i := 1 to Person.CountContener do
    (FindComponent('imgGlass' + i.ToString) as TImage).Bitmap := nil;

  ExeExec('select a.chimishe, a.electro, a.fire, a.phisic, a.psi, a.radiation, a.icon, ub.slot, a.art_id from arts a join belt ub on ub.art_id = a.art_id order by slot;', exActive, vQuery);

  while not vQuery.Eof do
  begin
    vSlot := vQuery.FieldByName('slot').AsInteger;

    if vSlot <= Person.CountContener then
    begin
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).Visible := true;
      (FindComponent('btnSlot' + vSlot.ToString + 'Info') as TSpeedButton).OnClick := btnArtClick;
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

procedure TFrameBag.Compare(AOld, AChange: TRectangle);
begin
  if AChange.Width > AOld.Width then
  begin
    AOld.BringToFront;
    AChange.Fill.Color := cWorseColor;
    AOld.Fill.Color := cEgualColor;
  end
  else
  begin
    AChange.BringToFront;
    AChange.Fill.Color := cEgualColor;
    AOld.Fill.Color := cBetterColor;
  end
end;

procedure TFrameBag.btnArtClick(Sender: TObject);
begin
  if FArtsList.Count >= (Sender as TSpeedButton).Tag then
  begin
    igfSelect.Enabled := true;
    igfSelect.Parent := (FindComponent('imgGlass' + (Sender as TSpeedButton).Tag.ToString) as TImage);
    igfSelect.Tag := (Sender as TSpeedButton).Tag;
    recRadiationChangeArt.Width := recRadiationChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].RadiationArmor / 100;
    recChimisheChangeArt.Width := recChimisheChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].ChimisheArmor / 100;
    recElectroChangeArt.Width := recElectroChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].ElectroArmor / 100;
    recPsiChangeArt.Width := recPsiChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].PsiArmor / 100;
    recPhisicChangeArt.Width := recPhisicChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].PhisicArmor / 100;
    recFireChangeArt.Width := recFireChangeArt.Tag * FArtsList[(Sender as TSpeedButton).Tag - 1].FireArmor / 100;

    Compare(recRadiationArt, recRadiationChangeArt);
    Compare(recChimisheArt, recChimisheChangeArt);
    Compare(recElectroArt, recElectroChangeArt);
    Compare(recPsiArt, recPsiChangeArt);
    Compare(recPhisicArt, recPhisicChangeArt);
    Compare(recFireArt, recFireChangeArt);

    ImgPercsArt.BringToFront;

    btnChooseArt.Enabled := true;
  end;
end;

procedure TFrameBag.btnChooseArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  btnChooseArt.Enabled := False;
  ExeExec('update belt set art_id = ' + FBagList[layInfo.Tag].RowID.ToString + ' where slot = ' + igfSelect.Tag.ToString + ';', exExecute, vQuery);
  ExeExec('insert into bag (table_name, row_id, health) values (''arts'',' + FArtsList[igfSelect.Tag - 1].ID.ToString + ', 100);', exExecute, vQuery);
  ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''arts'' and row_id = ' + FBagList[layInfo.Tag].RowID.ToString + ' limit 1);', exExecute, vQuery);
  ReloadPercs;
  ReloadBag;
end;

procedure TFrameBag.btnCloseChangeSlotClick(Sender: TObject);
begin
  igfSelect.Parent := nil;
  layChangeSlot.Visible := False;
end;

procedure TFrameBag.btnCloseInfoClick(Sender: TObject);
begin
  layInfo.Visible := False;
end;

procedure TFrameBag.btnUseClick(Sender: TObject);
var
  vIndex: integer;
  vQuery: TFDQuery;
begin
  vIndex := layInfo.Tag;
  case FBagList[vIndex].BagType of
    btMedical:
      begin
        Person.Health := Person.Health + FBagList[vIndex].HealthRestore;
        ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''medical'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' limit 1);', exExecute, vQuery);
      end;
  end;

  layInfo.Visible := False;
  ReloadPercs;
  ReloadBag;
end;

procedure TFrameBag.btnCloseQRClick(Sender: TObject);
begin
  laySellQR.Visible := False;
end;

procedure TFrameBag.CreateElements(AIsClassicBag: boolean);
var
  i: integer;
  vImgBack, vImgElement: TImage;
  vLabCount: TLabel;
  vBtn: TSpeedButton;
  vLayHealth: TLayout;
  vCircleHealth: TCircle;

  procedure SetFill;
  begin
    if FBagList[i].Health < 33 then
      vCircleHealth.Fill.Color := cCriticalColor
    else if FBagList[i].Health < 66 then
      vCircleHealth.Fill.Color := cNormalColor
    else
      vCircleHealth.Fill.Color := cFullColor;
  end;

begin
  if NOT AIsClassicBag then
    ClearElementsSection;

  if Assigned(FBagList) and (FBagList.Count > 0) then
    for i := 0 to FBagList.Count - 1 do
    begin
      vImgBack := TImage.Create(nil);
      with vImgBack do
      begin
        if AIsClassicBag then
          Parent := flElements;

        BringToFront;

        case FBagList[i].BagType of
          btMedical:
            begin
              if NOT AIsClassicBag then
                Parent := flMedical;

              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btArt:
            begin
              if NOT AIsClassicBag then
              begin
                Parent := flArts;
                flArts.Width := flArts.ChildrenCount * 63;
              end;

              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btArmor:
            begin
              if NOT AIsClassicBag then
              begin
                Parent := flArmors;
                flArmors.Width := flArmors.ChildrenCount * 126;
              end;

              Width := 126;
              Height := 189;
              Bitmap.Assign(imgList126x189.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btWeapon:
            begin
              if NOT AIsClassicBag then
              begin
                Parent := flWeapons;
                flWeapons.Width := flWeapons.ChildrenCount * 189;
              end;

              Width := 189;
              Height := 126;
              Bitmap.Assign(imgList189x126.Source[0].MultiResBitmap[0].Bitmap);
            end;
        end;

        Opacity := 0.95;
        HitTest := False;
      end;

      vImgElement := TImage.Create(nil);
      with vImgElement do
      begin
        Parent := vImgBack;
        Align := TAlignLayout.Contents;
        Margins.Left := 5;
        Margins.Top := 5;
        Margins.Bottom := 5;
        Margins.Right := 5;
        Bitmap.Assign(FBagList[i].Icon);
        HitTest := False;
      end;

      vLabCount := TLabel.Create(nil);
      with vLabCount do
      begin
        Align := TAlignLayout.Contents;
        Parent := vImgElement;
        StyledSettings := [];
        TextSettings.Font.Family := 'YouTube Sans Dark';
        TextSettings.Font.Size := 16;
        TextSettings.Font.Style := [TFontStyle.fsBold];
        TextSettings.HorzAlign := TtextAlign.Trailing;
        TextSettings.VertAlign := TtextAlign.Trailing;
        TextSettings.FontColor := TAlphaColors.Darkgray;

        if FBagList[i].Count > 1 then
          Text := FBagList[i].Count.ToString
        else
          Text := '';

        Margins.Bottom := 5;
        Margins.Top := 5;
        Margins.Left := 5;
        Margins.Right := 5;
        HitTest := False;
      end;

      vBtn := TSpeedButton.Create(nil);
      with vBtn do
      begin
        Parent := vLabCount;
        Align := TAlignLayout.Client;
        StyleLookup := 'transparentcirclebuttonstyle';
        OnClick := OnClickElement;
        Tag := i;
      end;

      case FBagList[i].BagType of
        btArmor, btWeapon:
          begin
            vLayHealth := TLayout.Create(nil);
            with vLayHealth do
            begin
              Parent := vImgElement;
              Align := TAlignLayout.Bottom;
              Margins.Left := 5;
              Margins.Bottom := 5;
              Height := 20;
              HitTest := False;
              BringToFront;
            end;

            vCircleHealth := TCircle.Create(nil);
            with vCircleHealth do
            begin
              Parent := vLayHealth;
              Align := TAlignLayout.Left;
              SetFill;
              Width := vLayHealth.Height;
              HitTest := False;
              Opacity := 0.6;
            end;
          end;
      end;

      if AIsClassicBag then
        SetLayBagHeight;
    end;

  if AIsClassicBag then
    if flBackground.ChildrenCount = 0 then
      CreateBagBackground;
end;

procedure TFrameBag.OnClickElement(Sender: TObject);
var
  vIndex: integer;
begin
  vIndex := (Sender as TSpeedButton).Tag;
  case FBagList[vIndex].BagType of
    btMedical:
      begin
        recHealth.Visible := False;
        labHealthRestore.Text := FBagList[vIndex].HealthRestore.ToString;
        labElementCost.Text := Format('%.0n', [FBagList[vIndex].Cost]);
        recCountSlots.Visible := False;
        layInfo.Tag := (Sender as TSpeedButton).Tag;
        recHealthRestore.Visible := true;
        layPercs.Visible := False;
        layUse.Visible := true;
        layAddArt.Visible := False;
        layAddArmor.Visible := False;
        layPanel.Height := layUse.Height + laySells.Height + imgBottom.Height + imgTop.Height;
        layInfo.Visible := true;
      end;

    btArt:
      begin
        labElementCost.Text := Format('%.0n', [FBagList[vIndex].Cost]);
        infoRadiation.Width := infoRadiation.Tag * FBagList[vIndex].Percs.RadiationArmor / 100;
        infoChimishe.Width := infoChimishe.Tag * FBagList[vIndex].Percs.ChimisheArmor / 100;
        infoElectro.Width := infoElectro.Tag * FBagList[vIndex].Percs.ElectroArmor / 100;
        infoPsi.Width := infoPsi.Tag * FBagList[vIndex].Percs.PsiArmor / 100;
        infoPhisic.Width := infoPhisic.Tag * FBagList[vIndex].Percs.PhisicArmor / 100;
        infoFire.Width := infoFire.Tag * FBagList[vIndex].Percs.FireArmor / 100;

        infoRadiationFullArmor.Width := 0;
        infoChimisheFullArmor.Width := 0;
        infoElectroFullArmor.Width := 0;
        infoPsiFullArmor.Width := 0;
        infoPhisicFullArmor.Width := 0;
        infoFireFullArmor.Width := 0;

        infoLabradiation.Text := IfThen(FBagList[vIndex].Percs.RadiationArmor = 0, '', FBagList[vIndex].Percs.RadiationArmor.ToString + ' %');
        infoLabChimishe.Text := IfThen(FBagList[vIndex].Percs.ChimisheArmor = 0, '', FBagList[vIndex].Percs.ChimisheArmor.ToString + ' %');
        infoLabElectro.Text := IfThen(FBagList[vIndex].Percs.ElectroArmor = 0, '', FBagList[vIndex].Percs.ElectroArmor.ToString + ' %');
        infoLabPsi.Text := IfThen(FBagList[vIndex].Percs.PsiArmor = 0, '', FBagList[vIndex].Percs.PsiArmor.ToString + ' %');
        infoLabPhisic.Text := IfThen(FBagList[vIndex].Percs.PhisicArmor = 0, '', FBagList[vIndex].Percs.PhisicArmor.ToString + ' %');
        infoLabFire.Text := IfThen(FBagList[vIndex].Percs.FireArmor = 0, '', FBagList[vIndex].Percs.FireArmor.ToString + ' %');

        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := False;
        layPercs.Visible := true;
        recCountSlots.Visible := False;
        recHealth.Visible := False;
        layAddArmor.Visible := False;
        layAddArt.Visible := true;
        layUse.Visible := False;
        layPanel.Height := layPercs.Height + 25 + imgBottom.Height + imgTop.Height;
        layInfo.Visible := true;
      end;

    btArmor:
      begin
        labElementCost.Text := Format('%.0n', [FBagList[vIndex].Cost]);
        infoRadiation.Width := infoRadiation.Tag * FBagList[vIndex].Percs.RadiationArmor / 100;
        infoChimishe.Width := infoChimishe.Tag * FBagList[vIndex].Percs.ChimisheArmor / 100;
        infoElectro.Width := infoElectro.Tag * FBagList[vIndex].Percs.ElectroArmor / 100;
        infoPsi.Width := infoPsi.Tag * FBagList[vIndex].Percs.PsiArmor / 100;
        infoPhisic.Width := infoPhisic.Tag * FBagList[vIndex].Percs.PhisicArmor / 100;
        infoFire.Width := infoFire.Tag * FBagList[vIndex].Percs.FireArmor / 100;

        if Person.ArmorId > 0 then
        begin
          infoRadiationFullArmor.Width := infoRadiationFullArmor.Tag * FArmorPerc.RadiationArmor / 100;
          infoChimisheFullArmor.Width := infoChimisheFullArmor.Tag * FArmorPerc.ChimisheArmor / 100;
          infoElectroFullArmor.Width := infoElectroFullArmor.Tag * FArmorPerc.ElectroArmor / 100;
          infoPsiFullArmor.Width := infoPsiFullArmor.Tag * FArmorPerc.PsiArmor / 100;
          infoPhisicFullArmor.Width := infoPhisicFullArmor.Tag * FArmorPerc.PhisicArmor / 100;
          infoFireFullArmor.Width := infoFireFullArmor.Tag * FArmorPerc.FireArmor / 100;

          Compare(infoRadiation, infoRadiationFullArmor);
          Compare(infoChimishe, infoChimisheFullArmor);
          Compare(infoElectro, infoElectroFullArmor);
          Compare(infoPsi, infoPsiFullArmor);
          Compare(infoPhisic, infoPhisicFullArmor);
          Compare(infoFire, infoFireFullArmor);
        end
        else
        begin
          infoRadiationFullArmor.Width := infoRadiationFullArmor.Tag * FBagList[vIndex].Percs.RadiationArmor / FBagList[vIndex].Health;
          infoChimisheFullArmor.Width := infoChimisheFullArmor.Tag * FBagList[vIndex].Percs.ChimisheArmor / FBagList[vIndex].Health;
          infoElectroFullArmor.Width := infoElectroFullArmor.Tag * FBagList[vIndex].Percs.ElectroArmor / FBagList[vIndex].Health;
          infoPsiFullArmor.Width := infoPsiFullArmor.Tag * FBagList[vIndex].Percs.PsiArmor / FBagList[vIndex].Health;
          infoPhisicFullArmor.Width := infoPhisicFullArmor.Tag * FBagList[vIndex].Percs.PhisicArmor / FBagList[vIndex].Health;
          infoFireFullArmor.Width := infoFireFullArmor.Tag * FBagList[vIndex].Percs.FireArmor / FBagList[vIndex].Health;

          infoRadiationFullArmor.Fill.Color := $5AC98826;
          infoChimisheFullArmor.Fill.Color := $5AC98826;
          infoElectroFullArmor.Fill.Color := $5AC98826;
          infoPsiFullArmor.Fill.Color := $5AC98826;
          infoPhisicFullArmor.Fill.Color := $5AC98826;
          infoFireFullArmor.Fill.Color := $5AC98826;
        end;

        infoLabradiation.Text := IfThen(FBagList[vIndex].Percs.RadiationArmor = 0, '', FBagList[vIndex].Percs.RadiationArmor.ToString + ' %');
        infoLabChimishe.Text := IfThen(FBagList[vIndex].Percs.ChimisheArmor = 0, '', FBagList[vIndex].Percs.ChimisheArmor.ToString + ' %');
        infoLabElectro.Text := IfThen(FBagList[vIndex].Percs.ElectroArmor = 0, '', FBagList[vIndex].Percs.ElectroArmor.ToString + ' %');
        infoLabPsi.Text := IfThen(FBagList[vIndex].Percs.PsiArmor = 0, '', FBagList[vIndex].Percs.PsiArmor.ToString + ' %');
        infoLabPhisic.Text := IfThen(FBagList[vIndex].Percs.PhisicArmor = 0, '', FBagList[vIndex].Percs.PhisicArmor.ToString + ' %');
        infoLabFire.Text := IfThen(FBagList[vIndex].Percs.FireArmor = 0, '', FBagList[vIndex].Percs.FireArmor.ToString + ' %');

        SetHealthProgress(HealthProgress, FBagList[vIndex].Health);
        labCountSlots.Text := FBagList[vIndex].CountSlots.ToString;
        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := False;
        layPercs.Visible := true;
        recHealth.Visible := true;
        recCountSlots.Visible := true;
        layAddArmor.Visible := true;
        layAddArt.Visible := False;
        layUse.Visible := False;
        layPanel.Height := layPercs.Height + recCountSlots.Height + recHealth.Height + 25 + imgBottom.Height + imgTop.Height;
        layInfo.Visible := true;
      end;

    btWeapon:
      begin
        labElementCost.Text := Format('%.0n', [FBagList[vIndex].Cost]);
        recHealth.Visible := true;
        SetHealthProgress(HealthProgress, FBagList[vIndex].Health);
        recCountSlots.Visible := False;
        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := False;
        layPercs.Visible := False;
        layAddArmor.Visible := true;
        layAddArt.Visible := False;
        layUse.Visible := False;
        layPanel.Height := layAddArmor.Height + laySells.Height + imgBottom.Height + imgTop.Height;
        layInfo.Visible := true;
      end;
  end;

  imgPercsInfo.BringToFront;
  layInfo.Visible := true;
end;

end.
