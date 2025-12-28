unit uFrameBag;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uGlobal, System.ImageList, FMX.ImgList, Generics.Collections,
  FMX.Controls.Presentation, FMX.Effects, StrUtils, FireDAC.Comp.Client;

type
  TFrameBag = class(TFrame)
    VertScrollBox1: TVertScrollBox;
    imgList63x63: TImageList;
    imgList63x126: TImageList;
    imgList126x189: TImageList;
    imgList189x126: TImageList;
    Layout2: TLayout;
    flBackground: TFlowLayout;
    flElements: TFlowLayout;
    layBag: TLayout;
    layInfo: TLayout;
    Rectangle8: TRectangle;
    layPanel: TLayout;
    Image10: TImage;
    Image11: TImage;
    Image13: TImage;
    Image8: TImage;
    Layout4: TLayout;
    Image3: TImage;
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
    Image1: TImage;
    btnAddArmor: TCornerButton;
    layUse: TLayout;
    Image2: TImage;
    btnUse: TCornerButton;
    layAddArt: TLayout;
    Image6: TImage;
    btnAddArt: TCornerButton;
    Rectangle1: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    Image7: TImage;
    Label4: TLabel;
    SwitchStyle: TSwitch;
    Rectangle2: TRectangle;
    InnerGlowEffect3: TInnerGlowEffect;
    labCash: TLabel;
    Label1: TLabel;
    recCountSlots: TRectangle;
    InnerGlowEffect4: TInnerGlowEffect;
    Image4: TImage;
    Label2: TLabel;
    labCountSlots: TLabel;
    recHealth: TRectangle;
    InnerGlowEffect5: TInnerGlowEffect;
    ImgHealth: TImage;
    HealthProgress: TRectangle;
    recHealthRestore: TRectangle;
    InnerGlowEffect6: TInnerGlowEffect;
    Image5: TImage;
    Label3: TLabel;
    labHealthRestore: TLabel;
    procedure FramePainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SwitchStyleSwitch(Sender: TObject);
    procedure btnCloseInfoClick(Sender: TObject);
    procedure btnAddArmorClick(Sender: TObject);
    procedure btnUseClick(Sender: TObject);
  private

    procedure CreateFreeCell(ALayout: TFlowLayout);
    procedure OnClickElement(Sender: TObject);
    procedure SetLayBagHeight;
    procedure ClearElements;

    { Private declarations }
  public
    { Public declarations }
    procedure CreateElements;
    procedure CreateBagBackground;
    constructor Create(AObject: TFmxObject);
  end;

implementation

{$R *.fmx}

constructor TFrameBag.Create(AObject: TFmxObject);
begin
  inherited Create(AObject);
  labCash.TextSettings.Font.Family := 'lcd';
end;

procedure TFrameBag.CreateBagBackground;
var
  i: integer;
begin
  SetLayBagHeight;

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

  for i := 0 to flElements.ChildrenCount - 1 do
    if (flElements.Children[i] as TImage).Position.Y + (flElements.Children[i] as TImage).Height > vMaxHeight then
      vMaxHeight := (flElements.Children[i] as TImage).Position.Y + (flElements.Children[i] as TImage).Height;

  if vMaxHeight > Self.Height - 80 then
    layBag.Height := vMaxHeight
  else
    layBag.Height := Self.Height - 80;
end;

procedure TFrameBag.SwitchStyleSwitch(Sender: TObject);
begin
  if Not SwitchStyle.IsChecked then
    Person.IsClassicBag := false;
end;

procedure TFrameBag.CreateFreeCell(ALayout: TFlowLayout);
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

procedure TFrameBag.FramePainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  if flBackground.ChildrenCount = 0 then
    CreateBagBackground;
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

procedure TFrameBag.btnCloseInfoClick(Sender: TObject);
begin
  layInfo.Visible := false;
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
  layInfo.Visible := false;
  ReloadBag;
end;

procedure TFrameBag.ClearElements;
var
  i: integer;
begin
  for i := 0 to flElements.ChildrenCount - 1 do
  begin
    (flElements.Children[0] as TImage).Visible := false;
    FreeAndNil(flElements.Children[0]);
  end;
end;

procedure TFrameBag.CreateElements;
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
        Parent := flElements;

        case FBagList[i].BagType of
          btMedical:
            begin
              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btArt:
            begin
              Width := 63;
              Height := 63;
              Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btArmor:
            begin
              Width := 126;
              Height := 189;
              Bitmap.Assign(imgList126x189.Source[0].MultiResBitmap[0].Bitmap);
            end;

          btWeapon:
            begin
              Width := 189;
              Height := 126;
              Bitmap.Assign(imgList189x126.Source[0].MultiResBitmap[0].Bitmap);
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

      SetLayBagHeight;
    end;
end;

procedure TFrameBag.OnClickElement(Sender: TObject);
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

end.
