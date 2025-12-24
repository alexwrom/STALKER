unit uFrameBag;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uGlobal, System.ImageList, FMX.ImgList, Generics.Collections,
  FMX.Controls.Presentation, FMX.Effects;

type
  TFrameBag = class(TFrame)
    Image9: TImage;
    VertScrollBox1: TVertScrollBox;
    imgList63x63: TImageList;
    imgList63x126: TImageList;
    imgList126x189: TImageList;
    imgList189x126: TImageList;
    Layout2: TLayout;
    Label4: TLabel;
    Image5: TImage;
    labCash: TLabel;
    flBackground: TFlowLayout;
    flElements: TFlowLayout;
    layBag: TLayout;
    SwitchStyle: TSwitch;
    procedure FramePainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SwitchStyleSwitch(Sender: TObject);
  private

    procedure CreateFreeCell(ALayout: TFlowLayout);
    procedure OnClickElement(Sender: TObject);
    procedure SetLayBagHeight;

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
vMaxHeight : Single;
  I: Integer;
begin
  vMaxHeight := 0;

  for I := 0 to flElements.ChildrenCount - 1 do
     if (flElements.Children[i] as TImage).Position.Y + (flElements.Children[i] as TImage).Height > vMaxHeight then
       vMaxHeight := (flElements.Children[i] as TImage).Position.Y + (flElements.Children[i] as TImage).Height;

  if vMaxHeight > Self.Height - 50 then
    layBag.Height := vMaxHeight
  else
    layBag.Height := Self.Height - 50;
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
    Bitmap.Assign(imgList63x63.Source[0].MultiResBitmap[0].Bitmap);
    HitTest := false;
  end;
end;

procedure TFrameBag.FramePainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  if flBackground.ChildrenCount = 0 then
    CreateBagBackground;
end;

procedure TFrameBag.CreateElements;
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
    end;

    SetLayBagHeight;
  end;
end;

procedure TFrameBag.OnClickElement(Sender: TObject);
begin

end;

end.
