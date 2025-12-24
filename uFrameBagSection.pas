unit uFrameBagSection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uGlobal, System.ImageList, FMX.ImgList, Generics.Collections,
  FMX.Controls.Presentation;

type
  TFrameBagSection = class(TFrame)
    Image9: TImage;
    VertScrollBox1: TVertScrollBox;
    imgList63x63: TImageList;
    imgList63x126: TImageList;
    imgList126x189: TImageList;
    imgList189x126: TImageList;
    Layout2: TLayout;
    Label4: TLabel;
    Image4: TImage;
    Image11: TImage;
    Layout3: TLayout;
    Label3: TLabel;
    Layout4: TLayout;
    Label5: TLabel;
    Image3: TImage;
    Layout7: TLayout;
    Image5: TImage;
    labCash: TLabel;
    Label7: TLabel;
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
    SwitchStyle: TSwitch;
    procedure SwitchStyleSwitch(Sender: TObject);
  private

    procedure CreateFreeCell(ALayout: TFlowLayout);
    procedure CreateElements;
    procedure CreateBagBackground;
    procedure OnClickElement(Sender: TObject);
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

procedure TFrameBagSection.CreateElements;
var
  i: integer;
  vImgBack, vImgElement: TImage;
  vLabCount: TLabel;
  vBtn: TSpeedButton;
begin
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
            flArts.Width := flArts.Width + 63;
          end;

        btArmor:
          begin
            Parent := flArmors;
            Width := 126;
            Height := 189;
            Bitmap.Assign(imgList126x189.Source[0].MultiResBitmap[0].Bitmap);
            flArmors.Width := flArmors.Width + 126;
          end;

        btWeapon:
          begin
            Parent := flWeapons;
            Width := 189;
            Height := 126;
            Bitmap.Assign(imgList189x126.Source[0].MultiResBitmap[0].Bitmap);
            flWeapons.Width := flWeapons.Width + 189;
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
    end;
  end;
end;

procedure TFrameBagSection.OnClickElement(Sender: TObject);
begin

end;

procedure TFrameBagSection.SwitchStyleSwitch(Sender: TObject);
begin
  if SwitchStyle.IsChecked then
     Person.IsClassicBag := true;
end;

end.
