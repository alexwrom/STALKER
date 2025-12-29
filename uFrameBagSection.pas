unit uFrameBagSection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uGlobal, System.ImageList, FMX.ImgList, Generics.Collections,
  FMX.Controls.Presentation, FMX.Effects, StrUtils, FireDAC.Comp.Client;

type
  TFrameBagSection = class(TFrame)
    VertScrollBox1: TVertScrollBox;
    imgList63x63: TImageList;
    imgList63x126: TImageList;
    imgList126x189: TImageList;
    imgList189x126: TImageList;
    Layout2: TLayout;
    Image4: TImage;
    Image11: TImage;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout7: TLayout;
    layWeaponLayouts: TLayout;
    flWeaponsBackgroud: TFlowLayout;
    flWeapons: TFlowLayout;
    HorzScrollBox1: THorzScrollBox;
    Layout9: TLayout;
    layArmorsLayuots: TLayout;
    Layout10: TLayout;
    flArmorsBackground: TFlowLayout;
    HorzScrollBox2: THorzScrollBox;
    flArmors: TFlowLayout;
    layArtefacsLayouts: TLayout;
    Layout11: TLayout;
    flArtsBackground: TFlowLayout;
    HorzScrollBox3: THorzScrollBox;
    flArts: TFlowLayout;
    HorzScrollBox4: THorzScrollBox;
    flMedical: TFlowLayout;
    Layout6: TLayout;
    flMedicalBackground: TFlowLayout;
    layMedicalLayouts: TLayout;
    layTopLeft: TLayout;
    layTop: TLayout;
    layTopClient: TLayout;
    Image8: TImage;
    Rectangle1: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    Image1: TImage;
    Image2: TImage;
    Rectangle2: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    labCash: TLabel;
    Label2: TLabel;
    Image5: TImage;
    Image6: TImage;
    Layout1: TLayout;
    Image7: TImage;
    Label4: TLabel;
    SwitchStyle: TSwitch;
    layInfo: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    Image10: TImage;
    Image3: TImage;
    Image13: TImage;
    Image9: TImage;
    Layout5: TLayout;
    Image12: TImage;
    btnCloseInfo: TCornerButton;
    imgPercs: TImage;
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
    Rectangle5: TRectangle;
    InnerGlowEffect2: TInnerGlowEffect;
    layAddArmor: TLayout;
    Image14: TImage;
    btnAddArmor: TCornerButton;
    layUse: TLayout;
    Image15: TImage;
    btnUse: TCornerButton;
    layAddArt: TLayout;
    Image16: TImage;
    btnAddArt: TCornerButton;
    recCountSlots: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    Image17: TImage;
    Label1: TLabel;
    labCountSlots: TLabel;
    recHealth: TRectangle;
    InnerGlowEffect5: TInnerGlowEffect;
    ImgHealth: TImage;
    HealthProgress: TRectangle;
    recHealthRestore: TRectangle;
    InnerGlowEffect6: TInnerGlowEffect;
    Image18: TImage;
    Label3: TLabel;
    labHealthRestore: TLabel;
    layChangeSlot: TLayout;
    Rectangle3: TRectangle;
    Layout8: TLayout;
    Image19: TImage;
    Image20: TImage;
    Image21: TImage;
    Image22: TImage;
    Layout12: TLayout;
    Image23: TImage;
    btnCloseChangeSlot: TCornerButton;
    Image24: TImage;
    recPsiChangeArt: TRectangle;
    recFireChangeArt: TRectangle;
    recElectroChangeArt: TRectangle;
    recChimisheChangeArt: TRectangle;
    recPhisicChangeArt: TRectangle;
    recRadiationArt: TRectangle;
    recChimisheArt: TRectangle;
    recElectroArt: TRectangle;
    recPsiArt: TRectangle;
    recPhisicArt: TRectangle;
    recFireArt: TRectangle;
    recRadiationChangeArt: TRectangle;
    Rectangle12: TRectangle;
    InnerGlowEffect7: TInnerGlowEffect;
    layArts: TLayout;
    Image25: TImage;
    GridPanelLayout1: TGridPanelLayout;
    laySlot1: TLayout;
    ImgGlass1: TImage;
    ImgSlot1: TImage;
    btnSlot1Info: TSpeedButton;
    laySlot2: TLayout;
    ImgGlass2: TImage;
    igfSelect: TInnerGlowEffect;
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
    Label12: TLabel;
    Rectangle16: TRectangle;
    InnerGlowEffect10: TInnerGlowEffect;
    Layout13: TLayout;
    Image26: TImage;
    btnChooseArt: TCornerButton;
    Rectangle13: TRectangle;
    InnerGlowEffect8: TInnerGlowEffect;
    imgArt: TImage;
    labArtName: TLabel;
    GlassList: TImageList;
    procedure SwitchStyleSwitch(Sender: TObject);
    procedure btnUseClick(Sender: TObject);
    procedure btnAddArmorClick(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnChooseArtClick(Sender: TObject);
    procedure btnAddArtClick(Sender: TObject);
  private
    FArtsList: TList<TPerc>;
    procedure CreateFreeCell(ALayout: TFlowLayout);
    procedure CreateElements;
    procedure CreateBagBackground;
    procedure OnClickElement(Sender: TObject);
    procedure ClearElements;
    procedure btnArtClick(Sender: TObject);
    procedure btnCloseChangeSlotClick(Sender: TObject);
    procedure ReloadArts;
    { Private declarations }
  public
    { Public declarations }

    procedure LoadBagElements;
    constructor Create(AObject: TFmxObject);
  end;

implementation

{$R *.fmx}

constructor TFrameBagSection.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  labCash.TextSettings.Font.Family := 'lcd';
  FArtsList:= TList<TPerc>.Create;
end;

procedure TFrameBagSection.LoadBagElements;
begin
  CreateBagBackground;
  CreateElements;
end;

procedure TFrameBagSection.CreateBagBackground;
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

procedure TFrameBagSection.CreateFreeCell(ALayout: TFlowLayout);
begin
  with TImage.Create(ALayout) do
  begin
    Parent := ALayout;
    Width := 63;
    Height := 63;
    Bitmap.Assign(imgList63x63.Source[1].MultiResBitmap[0].Bitmap);
    HitTest := false;
  end;
end;

procedure TFrameBagSection.ClearElements;
var
  i: integer;
begin
  for i := 0 to flMedical.ChildrenCount - 1 do
  begin
    (flMedical.Children[0] as TImage).Visible := false;
    FreeAndNil(flMedical.Children[0]);
  end;

  for i := 0 to flWeapons.ChildrenCount - 1 do
  begin
    (flWeapons.Children[0] as TImage).Visible := false;
    FreeAndNil(flWeapons.Children[0]);
  end;

  for i := 0 to flArmors.ChildrenCount - 1 do
  begin
    (flArmors.Children[0] as TImage).Visible := false;
    FreeAndNil(flArmors.Children[0]);
  end;

  for i := 0 to flArts.ChildrenCount - 1 do
  begin
    (flArts.Children[0] as TImage).Visible := false;
    FreeAndNil(flArts.Children[0]);
  end;
end;

procedure TFrameBagSection.CreateElements;
var
  i: integer;
  vImgBack, vImgElement: TImage;
  vLabCount: TLabel;
  vBtn: TSpeedButton;
begin
  ClearElements;

  if Assigned(FBagList) and (FBagList.Count > 0) then
    for i := 0 to FBagList.Count - 1 do
    begin
      vImgBack := TImage.Create(nil);
      with vImgBack do
      begin
        case FBagList[i].BagType of
          btMedical:
            begin
              Parent := flMedical;
              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btArt:
            begin
              Parent := flArts;
              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
              flArts.Width := flArts.ChildrenCount * 63;
            end;

          btArmor:
            begin
              Parent := flArmors;
              Width := 126;
              Height := 189;
              Bitmap.Assign(imgList126x189.Source[0].MultiResBitmap[0].Bitmap);
              flArmors.Width := flArmors.ChildrenCount * 126;
            end;

          btWeapon:
            begin
              Parent := flWeapons;
              Width := 189;
              Height := 126;
              Bitmap.Assign(imgList189x126.Source[0].MultiResBitmap[0].Bitmap);
              flWeapons.Width := flWeapons.ChildrenCount * 189;
            end;
        end;

        HitTest := false;
      end;

      vImgElement := TImage.Create(nil);
      with vImgElement do
      begin
        Parent := vImgBack;
        Align := TAlignLayout.Client;
        Bitmap.Assign(FBagList[i].Icon);
        HitTest := false;
      end;

      vLabCount := TLabel.Create(nil);
      with vLabCount do
      begin
        Align := TAlignLayout.Client;
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
        HitTest := false;
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
    end;
end;

procedure TFrameBagSection.ReloadArts;
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

  ExeExec('select a.chimishe, a.electro, a.fire, a.phisic, a.psi, a.radiation, a.icon, ub.slot, a.art_id from arts a join user_belt ub on ub.art_id = a.art_id where user_id = ' + Person.UserId.ToString + ' order by slot;', exActive, vQuery);

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
      ExeExec('delete from user_belt where slot = ' + vSlot.ToString + ' and user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery2);

    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFrameBagSection.btnArtClick(Sender: TObject);
procedure Compare(AArt, AChangeArt: TRectangle);
begin
  if AChangeArt.Width > AArt.Width then
      begin
        AArt.BringToFront;
        AChangeArt.Fill.Color := cBetterColor;
        AArt.Fill.Color := cEgualColor;
      end
      else
       begin
        AChangeArt.BringToFront;
        AChangeArt.Fill.Color := cEgualColor;
        AArt.Fill.Color := cWorseColor;
      end
end;

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
  end;
end;

procedure TFrameBagSection.btnCloseChangeSlotClick(Sender: TObject);
begin
   layChangeSlot.Visible := false;
end;

procedure TFrameBagSection.btnAddArmorClick(Sender: TObject);
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
      end;
    btWeapon:
      begin
        if Person.WeaponId <> 0 then
          ExeExec('insert into bag (table_name, row_id, health) values (''weapons'',' + Person.WeaponId.ToString + ',' + Person.WeaponHealth.ToString + ');', exExecute, vQuery);

        ExeExec('update users set weapon_id = ' + FBagList[vIndex].RowID.ToString + ', weapon_health = ' + FBagList[vIndex].Health.ToString + ' where user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
        ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''weapons'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' and health = ' + FBagList[vIndex].Health.ToString + ' limit 1);', exExecute, vQuery);
      end;
  end;
  layInfo.Visible := false;
  ReloadBag;
  ReloadPercs;
end;

procedure TFrameBagSection.btnAddArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
  vIndex: Integer;
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

      ReloadArts;
      layChangeSlot.Visible := true;
    end
    else
    begin
      ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''arts'' and row_id = ' + FBagList[vIndex].RowID.ToString + ' limit 1);', exExecute, vQuery);
      ExeExec('insert into user_belt (art_id, slot, user_id) values (' + FBagList[vIndex].RowID.ToString + ', (select count(1) + 1 from user_belt where user_id = ' + Person.UserId.ToString + '), ' + Person.UserId.ToString + ');', exExecute, vQuery);
    end;
  layInfo.Visible := false;
  ReloadBag;
  ReloadPercs;
end;

procedure TFrameBagSection.btnChooseArtClick(Sender: TObject);
var
  vQuery: TFDQuery;
begin
  ExeExec('update user_belt set art_id = ' + FBagList[layInfo.Tag].RowID.ToString + ' where slot = ' + igfSelect.Tag.ToString + ' and user_id = ' + Person.UserId.ToString + ';', exExecute, vQuery);
  ExeExec('insert into bag (table_name, row_id, health) values (''arts'',' + FArtsList[igfSelect.Tag - 1].ID.ToString + ', 100);', exExecute, vQuery);
  ExeExec('delete from bag where rowid = (select rowid from bag where table_name = ''arts'' and row_id = ' + FBagList[layInfo.Tag].RowID.ToString + ' limit 1);', exExecute, vQuery);
  ReloadBag;
  ReloadPercs;
  igfSelect.Parent := nil;
  layChangeSlot.Visible := false;
end;

procedure TFrameBagSection.btnCloseInfoClick(Sender: TObject);
begin
  layInfo.Visible := false;
end;

procedure TFrameBagSection.btnUseClick(Sender: TObject);
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
  layInfo.Visible := false;
  ReloadBag;
end;

procedure TFrameBagSection.OnClickElement(Sender: TObject);
var
  vIndex: integer;
begin
  vIndex := (Sender as TSpeedButton).Tag;
  case FBagList[vIndex].BagType of
    btMedical:
      begin
        recHealth.Visible := false;
        labHealthRestore.Text := FBagList[vIndex].HealthRestore.ToString;;
        recCountSlots.Visible := false;
        layInfo.Tag := (Sender as TSpeedButton).Tag;
        recHealthRestore.Visible := true;
        imgPercs.Visible := false;
        layUse.Visible := true;
        layAddArt.Visible := false;
        layAddArmor.Visible := false;
        layPanel.Height := 54 + layUse.Height + recHealthRestore.Height;
        layInfo.Visible := true;
      end;

    btArt:
      begin
        infoRadiation.Width := infoRadiation.Tag * FBagList[vIndex].Percs.RadiationArmor / 100;
        infoChimishe.Width := infoChimishe.Tag * FBagList[vIndex].Percs.ChimisheArmor / 100;
        infoElectro.Width := infoElectro.Tag * FBagList[vIndex].Percs.ElectroArmor / 100;
        infoPsi.Width := infoPsi.Tag * FBagList[vIndex].Percs.PsiArmor / 100;
        infoPhisic.Width := infoPhisic.Tag * FBagList[vIndex].Percs.PhisicArmor / 100;
        infoFire.Width := infoFire.Tag * FBagList[vIndex].Percs.FireArmor / 100;

        infoLabradiation.Text := IfThen(FBagList[vIndex].Percs.RadiationArmor = 0, '', FBagList[vIndex].Percs.RadiationArmor.ToString + ' %');
        infoLabChimishe.Text := IfThen(FBagList[vIndex].Percs.ChimisheArmor = 0, '', FBagList[vIndex].Percs.ChimisheArmor.ToString + ' %');
        infoLabElectro.Text := IfThen(FBagList[vIndex].Percs.ElectroArmor = 0, '', FBagList[vIndex].Percs.ElectroArmor.ToString + ' %');
        infoLabPsi.Text := IfThen(FBagList[vIndex].Percs.PsiArmor = 0, '', FBagList[vIndex].Percs.PsiArmor.ToString + ' %');
        infoLabPhisic.Text := IfThen(FBagList[vIndex].Percs.PhisicArmor = 0, '', FBagList[vIndex].Percs.PhisicArmor.ToString + ' %');
        infoLabFire.Text := IfThen(FBagList[vIndex].Percs.FireArmor = 0, '', FBagList[vIndex].Percs.FireArmor.ToString + ' %');

        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := false;
        imgPercs.Visible := true;
        recCountSlots.Visible := false;
        recHealth.Visible := false;
        layAddArmor.Visible := false;
        layAddArt.Visible := true;
        layUse.Visible := false;
        layPanel.Height := 54 + layAddArt.Height + imgPercs.Height;
        layInfo.Visible := true;
      end;

    btArmor:
      begin
        infoRadiation.Width := infoRadiation.Tag * FBagList[vIndex].Percs.RadiationArmor / 100;
        infoChimishe.Width := infoChimishe.Tag * FBagList[vIndex].Percs.ChimisheArmor / 100;
        infoElectro.Width := infoElectro.Tag * FBagList[vIndex].Percs.ElectroArmor / 100;
        infoPsi.Width := infoPsi.Tag * FBagList[vIndex].Percs.PsiArmor / 100;
        infoPhisic.Width := infoPhisic.Tag * FBagList[vIndex].Percs.PhisicArmor / 100;
        infoFire.Width := infoFire.Tag * FBagList[vIndex].Percs.FireArmor / 100;

        infoLabradiation.Text := IfThen(FBagList[vIndex].Percs.RadiationArmor = 0, '', FBagList[vIndex].Percs.RadiationArmor.ToString + ' %');
        infoLabChimishe.Text := IfThen(FBagList[vIndex].Percs.ChimisheArmor = 0, '', FBagList[vIndex].Percs.ChimisheArmor.ToString + ' %');
        infoLabElectro.Text := IfThen(FBagList[vIndex].Percs.ElectroArmor = 0, '', FBagList[vIndex].Percs.ElectroArmor.ToString + ' %');
        infoLabPsi.Text := IfThen(FBagList[vIndex].Percs.PsiArmor = 0, '', FBagList[vIndex].Percs.PsiArmor.ToString + ' %');
        infoLabPhisic.Text := IfThen(FBagList[vIndex].Percs.PhisicArmor = 0, '', FBagList[vIndex].Percs.PhisicArmor.ToString + ' %');
        infoLabFire.Text := IfThen(FBagList[vIndex].Percs.FireArmor = 0, '', FBagList[vIndex].Percs.FireArmor.ToString + ' %');

        HealthProgress.Width := HealthProgress.Tag * FBagList[vIndex].Health / 100;
        labCountSlots.Text := FBagList[vIndex].CountSlots.ToString;
        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := false;
        imgPercs.Visible := true;
        recHealth.Visible := true;
        recCountSlots.Visible := true;
        layAddArmor.Visible := true;
        layAddArt.Visible := false;
        layUse.Visible := false;
        layPanel.Height := 54 + layAddArmor.Height + imgPercs.Height + recCountSlots.Height + recHealth.Height;
        layInfo.Visible := true;
      end;

    btWeapon:
      begin
        recHealth.Visible := true;
        HealthProgress.Width := HealthProgress.Tag * FBagList[vIndex].Health / 100;
        recCountSlots.Visible := false;
        layInfo.Tag := (Sender as TSpeedButton).Tag;

        recHealthRestore.Visible := false;
        imgPercs.Visible := false;
        layAddArmor.Visible := true;
        layAddArt.Visible := false;
        layUse.Visible := false;
        layPanel.Height := 54 + layUse.Height + recHealth.Height;
        layInfo.Visible := true;
      end;
  end;

  layInfo.Visible := true;
end;

procedure TFrameBagSection.SwitchStyleSwitch(Sender: TObject);
begin
  if SwitchStyle.IsChecked then
    Person.IsClassicBag := true;
end;

end.
