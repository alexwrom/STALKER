unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdGlobal,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, uGlobal, Rest.Json, Classes.action,
  FireDAC.Comp.Client, FMX.StdCtrls, Generics.Collections, StrUtils, Classes.send, Threading,
  FMX.Objects, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.TabControl, FMX.Layouts,
  Controls.listitem, Controls.item, FMX.Edit, FMX.EditBox, FMX.SpinBox, System.IOUtils, Math;

type
  TMainForm = class(TForm)
    IdTCPServer: TIdTCPServer;
    ProgressBar: TProgressBar;
    labStatusLoadData: TLabel;
    OpenDialog: TOpenDialog;
    TabControl: TTabControl;
    tabWeapon: TTabItem;
    layListWeapons: TLayout;
    recSelect: TRectangle;
    Layout1: TLayout;
    btnAddWeapon: TCornerButton;
    Image1: TImage;
    ImgIconWeapon: TImage;
    btnLoadIconWeapon: TCornerButton;
    Label1: TLabel;
    eNameWeapon: TEdit;
    eCostWeapon: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    btnSaveWeapon: TCornerButton;
    btnDeleteWeapon: TCornerButton;
    StyleBook: TStyleBook;
    layBoardWeapon: TLayout;
    tabArmor: TTabItem;
    layBoardArmor: TLayout;
    btnDeleteArmor: TCornerButton;
    btnSaveArmor: TCornerButton;
    eCostArmor: TEdit;
    Label4: TLabel;
    eNameArmor: TEdit;
    Image2: TImage;
    imgIconArmor: TImage;
    btnLoadIconArmor: TCornerButton;
    Label5: TLabel;
    Label6: TLabel;
    layListArmors: TLayout;
    Rectangle1: TRectangle;
    Layout4: TLayout;
    btnAddArmor: TCornerButton;
    Label7: TLabel;
    Label8: TLabel;
    sbSlotCount: TSpinBox;
    GridPanelLayout1: TGridPanelLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Label9: TLabel;
    sbRadiation: TSpinBox;
    sbFire: TSpinBox;
    Label10: TLabel;
    sbChimishe: TSpinBox;
    Label11: TLabel;
    sbElectro: TSpinBox;
    Label12: TLabel;
    sbPSI: TSpinBox;
    Label13: TLabel;
    sbPhisic: TSpinBox;
    Label14: TLabel;
    tabArts: TTabItem;
    layBoardArt: TLayout;
    btnDelArt: TCornerButton;
    btnSaveArt: TCornerButton;
    eCostArt: TEdit;
    Label15: TLabel;
    eNameArt: TEdit;
    Image3: TImage;
    imgIconArt: TImage;
    btnLoadIconArt: TCornerButton;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    GridPanelLayout2: TGridPanelLayout;
    Layout6: TLayout;
    Label20: TLabel;
    sbRadiationArt: TSpinBox;
    sbFireArt: TSpinBox;
    Label21: TLabel;
    sbChimisheArt: TSpinBox;
    Label22: TLabel;
    Layout7: TLayout;
    sbElectroArt: TSpinBox;
    Label23: TLabel;
    sbPSIArt: TSpinBox;
    Label24: TLabel;
    sbPhisicArt: TSpinBox;
    Label25: TLabel;
    layListArts: TLayout;
    Rectangle2: TRectangle;
    Layout9: TLayout;
    btnAddArt: TCornerButton;
    layLevel: TLayout;
    Label17: TLabel;
    rbLevel1: TRadioButton;
    rbLevel4: TRadioButton;
    rbLevel3: TRadioButton;
    rbLevel2: TRadioButton;
    tabMedical: TTabItem;
    layBoardMedical: TLayout;
    btnDeleteMedical: TCornerButton;
    btnSaveMedical: TCornerButton;
    eCostMedical: TEdit;
    Label26: TLabel;
    eNameMedical: TEdit;
    Image4: TImage;
    imgIconMedical: TImage;
    btnLoadIconMedical: TCornerButton;
    Label27: TLabel;
    Label28: TLabel;
    layListMedical: TLayout;
    Rectangle3: TRectangle;
    Layout10: TLayout;
    btnAddMedical: TCornerButton;
    Label29: TLabel;
    sbRestoreHitpoint: TSpinBox;
    tabMap: TTabItem;
    Layout5: TLayout;
    CornerButton1: TCornerButton;
    btnSaveMap: TCornerButton;
    Image5: TImage;
    imgMap: TImage;
    btnLoadMap: TCornerButton;
    Label31: TLabel;
    GridPanelLayout3: TGridPanelLayout;
    V: TLayout;
    Label33: TLabel;
    Layout11: TLayout;
    Label34: TLabel;
    Label35: TLabel;
    Label38: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    eTLLat: TEdit;
    eTLLon: TEdit;
    eBRLat: TEdit;
    eBRLon: TEdit;
    btnCreateQR: TCornerButton;
    CornerButton2: TCornerButton;
    CornerButton3: TCornerButton;
    CornerButton4: TCornerButton;
    Layout8: TLayout;
    btnCreateCardsArts: TCornerButton;
    Layout12: TLayout;
    btnCreateCardsArmors: TCornerButton;
    Layout13: TLayout;
    btnCreateCardsMedical: TCornerButton;
    Layout14: TLayout;
    btnCreateCardsWeapons: TCornerButton;
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure FormShow(Sender: TObject);
    procedure btnSaveWeaponClick(Sender: TObject);
    procedure btnLoadIconWeaponClick(Sender: TObject);
    procedure btnAddWeaponClick(Sender: TObject);
    procedure btnAddArmorClick(Sender: TObject);
    procedure sbRadiationChange(Sender: TObject);
    procedure btnSaveArmorClick(Sender: TObject);
    procedure btnAddArtClick(Sender: TObject);
    procedure btnLoadIconArtClick(Sender: TObject);
    procedure btnLoadIconArmorClick(Sender: TObject);
    procedure btnSaveArtClick(Sender: TObject);
    procedure rbLevel1Change(Sender: TObject);
    procedure btnAddMedicalClick(Sender: TObject);
    procedure OnDeleteClick(Sender: TObject);
    procedure btnSaveMedicalClick(Sender: TObject);
    procedure btnLoadIconMedicalClick(Sender: TObject);
    procedure btnSaveMapClick(Sender: TObject);
    procedure btnLoadMapClick(Sender: TObject);
    procedure btnCreateQRClick(Sender: TObject);
    procedure CornerButton2Click(Sender: TObject);
    procedure CornerButton3Click(Sender: TObject);
    procedure CornerButton4Click(Sender: TObject);
    procedure btnCreateCardsArtsClick(Sender: TObject);
    procedure btnCreateCardsArmorsClick(Sender: TObject);
    procedure btnCreateCardsMedicalClick(Sender: TObject);
    procedure btnCreateCardsWeaponsClick(Sender: TObject);
  private
    { Private declarations }
    FStrdata: UnicodeString;
    FStrDataForAdmin: UnicodeString;
    FPageCount: Integer;
    FPageCountAdmin: Integer;

    FSelID: Integer;
    FControlListWeapons, FControlListArmors, FControlListArts, FControlListMedical: TControlListItem;

    procedure LoadWeaponsToListview;
    procedure ItemWeaponClick(FTagObject: TObject; FItem: TFMXObject);
    procedure LoadArmorsToListview;
    procedure ItemArmorClick(FTagObject: TObject; FItem: TFMXObject);
    procedure LoadArtsToListview;
    procedure ItemArtClick(FTagObject: TObject; FItem: TFMXObject);
    procedure LoadMedicalToListview;
    procedure ItemMedicalClick(FTagObject: TObject; FItem: TFMXObject);
    procedure LoadMap;
    procedure CreateCards(AImg: TBitmap; AName: string; ACode: string; AID: integer);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.btnAddArmorClick(Sender: TObject);
begin
  FSelID := 0;
  layBoardArmor.Visible := true;
  eNameArmor.Text := '';
  eCostArmor.Text := '';
  sbRadiation.Value := 0;
  sbFire.Value := 0;
  sbSlotCount.Value := 0;
  sbChimishe.Value := 0;
  sbElectro.Value := 0;
  sbPSI.Value := 0;
  sbPhisic.Value := 0;
  imgIconArmor.Bitmap.Assign(nil);
end;

procedure TMainForm.btnAddArtClick(Sender: TObject);
begin
  FSelID := 0;
  layBoardArt.Visible := true;
  eNameArt.Text := '';
  eCostArt.Text := '';
  sbRadiationArt.Value := 0;
  sbFireArt.Value := 0;
  sbChimisheArt.Value := 0;
  sbElectroArt.Value := 0;
  sbPSIArt.Value := 0;
  sbPhisicArt.Value := 0;
  imgIconArt.Bitmap.Assign(nil);
end;

procedure TMainForm.btnAddMedicalClick(Sender: TObject);
begin
  FSelID := 0;
  layBoardMedical.Visible := true;
  eNameMedical.Text := '';
  eCostMedical.Text := '';
  sbRestoreHitpoint.Value := sbRestoreHitpoint.Min;
  imgIconMedical.Bitmap.Assign(nil);
end;

procedure TMainForm.btnAddWeaponClick(Sender: TObject);
begin
  FSelID := 0;
  layBoardWeapon.Visible := true;
  eNameWeapon.Text := '';
  eCostWeapon.Text := '';
  ImgIconWeapon.Bitmap.Assign(nil);
end;

procedure TMainForm.btnCreateCardsArmorsClick(Sender: TObject);
var
  FDQuery: TFDQuery;
  vBitmap: TBitmap;
begin
  ExeExec('select * from armors;', exActive, FDQuery);
  FDQuery.First;

  while not(FDQuery.Eof) do
  begin
    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(FDQuery.FieldByName('icon'));
      CreateCards(vBitmap, FDQuery.FieldByName('name').AsString, '01', FDQuery.FieldByName('armor_id').AsInteger);
    finally
      FDQuery.Next;
    end;
  end;
end;

procedure TMainForm.btnCreateCardsArtsClick(Sender: TObject);
var
  FDQuery: TFDQuery;
  vBitmap: TBitmap;
begin
  ExeExec('select * from arts order by art_name;', exActive, FDQuery);
  FDQuery.First;

  while not(FDQuery.Eof) do
  begin
    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(FDQuery.FieldByName('icon'));
      CreateCards(vBitmap, FDQuery.FieldByName('art_name').AsString, '02', FDQuery.FieldByName('art_id').AsInteger);
    finally
      FDQuery.Next;
    end;
  end;
end;

procedure TMainForm.btnCreateCardsMedicalClick(Sender: TObject);
var
  FDQuery: TFDQuery;
  vBitmap: TBitmap;
begin
  ExeExec('select * from medical;', exActive, FDQuery);
  FDQuery.First;

  while not(FDQuery.Eof) do
  begin
    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(FDQuery.FieldByName('icon'));
      CreateCards(vBitmap, FDQuery.FieldByName('name').AsString, '03', FDQuery.FieldByName('medical_id').AsInteger);
    finally
      FDQuery.Next;
    end;
  end;
end;

procedure TMainForm.btnCreateCardsWeaponsClick(Sender: TObject);
var
  FDQuery: TFDQuery;
  vBitmap: TBitmap;
begin
  ExeExec('select * from weapons;', exActive, FDQuery);
  FDQuery.First;

  while not(FDQuery.Eof) do
  begin
    vBitmap := TBitmap.Create;
    try
      vBitmap.Assign(FDQuery.FieldByName('icon'));
      CreateCards(vBitmap, FDQuery.FieldByName('name').AsString, '04', FDQuery.FieldByName('weapon_id').AsInteger);
    finally
      FDQuery.Next;
    end;
  end;
end;

procedure TMainForm.btnCreateQRClick(Sender: TObject);
begin
  CreateCards(imgIconArt.Bitmap, eNameArt.Text, '01', FSelID);
end;

procedure TMainForm.OnDeleteClick(Sender: TObject);
begin
  MessageDlg('Подтверждаете удаление?', Tmsgdlgtype.mtConfirmation, mbYesNo, 0,
    procedure(const AResult: TModalResult)
    var
      FDQuery: TFDQuery;
    begin

      if (AResult = mrYes) then
      begin

        case TabControl.TabIndex of
          0:
            begin
              ExeExec(Format('delete  from arts where art_id = %d;', [FSelID]), exExecute, FDQuery);
              LoadArtsToListview;
            end;
          1:
            begin
              ExeExec(Format('delete  from armors where armor_id = %d;', [FSelID]), exExecute, FDQuery);
              LoadArmorsToListview;
            end;
          2:
            begin
              ExeExec(Format('delete  from medical where medical_id = %d;', [FSelID]), exExecute, FDQuery);
              LoadMedicalToListview;
            end;
          3:
            begin
              ExeExec(Format('delete  from weapons where weapon_id = %d;', [FSelID]), exExecute, FDQuery);
              LoadWeaponsToListview;
            end;
        end;

      end;
    end);
end;

procedure TMainForm.btnLoadIconArmorClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    imgIconArmor.Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.btnLoadIconArtClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    imgIconArt.Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.btnLoadIconMedicalClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    imgIconMedical.Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.btnLoadIconWeaponClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    ImgIconWeapon.Bitmap.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.btnLoadMapClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    imgMap.Bitmap.LoadFromFile(OpenDialog.FileName);
    imgMap.Hint := OpenDialog.FileName;
  end;
end;

procedure TMainForm.btnSaveArmorClick(Sender: TObject);
var
  FDQuery: TFDQuery;
begin
  if FSelID <> 0 then
    ExeExec(Format('update armors set icon = X%s, name = %s, cost = %s, count_slots = %s, fire = %s, electro = %s, chimishe = %s, radiation = %s, psi = %s, phisic = %s where armor_id = %d;',
      [QuotedStr(BitmapToHexString(imgIconArmor.Bitmap)), QuotedStr(eNameArmor.Text), eCostArmor.Text, sbSlotCount.Text, sbFire.Text, sbElectro.Text, sbChimishe.Text, sbRadiation.Text, sbPSI.Text,
      sbPhisic.Text, FSelID]), exExecute, FDQuery)
  else
    ExeExec(Format('insert into armors (icon, name, cost,count_slots, fire, electro, chimishe, radiation, psi, phisic) values (X%s, %s, %s, %s, %s, %s, %s, %s, %s, %s);',
      [QuotedStr(BitmapToHexString(imgIconArmor.Bitmap)), QuotedStr(eNameArmor.Text), eCostArmor.Text, sbSlotCount.Text, sbFire.Text, sbElectro.Text, sbChimishe.Text, sbRadiation.Text, sbPSI.Text,
      sbPhisic.Text]), exExecute, FDQuery);

  LoadArmorsToListview;
end;

procedure TMainForm.btnSaveArtClick(Sender: TObject);
var
  FDQuery: TFDQuery;
begin
  if FSelID <> 0 then
    ExeExec(Format('update arts set icon = X%s, art_name = %s, cost = %s, fire = %s, electro = %s, chimishe = %s, radiation = %s, psi = %s, phisic = %s, level = %d where art_id = %d;',
      [QuotedStr(BitmapToHexString(imgIconArt.Bitmap)), QuotedStr(eNameArt.Text), eCostArt.Text, sbFireArt.Text, sbElectroArt.Text, sbChimisheArt.Text, sbRadiationArt.Text, sbPSIArt.Text,
      sbPhisicArt.Text, layLevel.Tag, FSelID]), exExecute, FDQuery)
  else
    ExeExec(Format('insert into arts (icon, art_name, cost, fire, electro, chimishe, radiation, psi, phisic, level) values (X%s, %s, %s, %s, %s, %s, %s, %s, %s, %d);',
      [QuotedStr(BitmapToHexString(imgIconArt.Bitmap)), QuotedStr(eNameArt.Text), eCostArt.Text, sbFireArt.Text, sbElectroArt.Text, sbChimisheArt.Text, sbRadiationArt.Text, sbPSIArt.Text,
      sbPhisicArt.Text, layLevel.Tag]), exExecute, FDQuery);

  LoadArtsToListview;
end;

procedure TMainForm.btnSaveMapClick(Sender: TObject);
var
  FDQuery: TFDQuery;
begin
  ExeExec(Format('update game_data set map_image_path = %s, map_left_top_lat = %s, map_left_top_lon = %s, map_right_bottom_lat = %s, map_right_bottom_lon = %s;',
    [QuotedStr((imgMap.Hint)), StringReplace(eTLLat.Text, ',', '.', []), StringReplace(eTLLon.Text, ',', '.', []), StringReplace(eBRLat.Text, ',', '.', []), StringReplace(eBRLon.Text, ',', '.', [])]),
    exExecute, FDQuery);
end;

procedure TMainForm.btnSaveMedicalClick(Sender: TObject);
var
  FDQuery: TFDQuery;
begin
  if FSelID <> 0 then
    ExeExec(Format('update medical set icon = X%s, name = %s, cost = %s, health_restore = %s where medical_id = %d;', [QuotedStr(BitmapToHexString(imgIconMedical.Bitmap)),
      QuotedStr(eNameMedical.Text), eCostMedical.Text, sbRestoreHitpoint.Text, FSelID]), exExecute, FDQuery)
  else
    ExeExec(Format('insert into medical (icon, name, cost, health_restore) values (X%s, %s, %s, %s);', [QuotedStr(BitmapToHexString(imgIconMedical.Bitmap)), QuotedStr(eNameMedical.Text),
      eCostMedical.Text, sbRestoreHitpoint.Text]), exExecute, FDQuery);

  LoadMedicalToListview;
end;

procedure TMainForm.btnSaveWeaponClick(Sender: TObject);
var
  FDQuery: TFDQuery;
begin
  if FSelID <> 0 then
    ExeExec(Format('update weapons set icon = X%s, name = %s, cost = %s where weapon_id = %d;', [QuotedStr(BitmapToHexString(ImgIconWeapon.Bitmap)), QuotedStr(eNameWeapon.Text), eCostWeapon.Text,
      FSelID]), exExecute, FDQuery)
  else
    ExeExec(Format('insert into weapons (icon, name, cost) values (X%s, %s, %s);', [QuotedStr(BitmapToHexString(ImgIconWeapon.Bitmap)), QuotedStr(eNameWeapon.Text), eCostWeapon.Text]),
      exExecute, FDQuery);

  LoadWeaponsToListview;
end;

procedure TMainForm.CornerButton2Click(Sender: TObject);
begin
  CreateCards(imgIconArmor.Bitmap, eNameArmor.Text, '02', FSelID);
end;

procedure TMainForm.CornerButton3Click(Sender: TObject);
begin
  CreateCards(imgIconMedical.Bitmap, eNameMedical.Text, '03', FSelID);
end;

procedure TMainForm.CornerButton4Click(Sender: TObject);
begin
  CreateCards(ImgIconWeapon.Bitmap, eNameWeapon.Text, '04', FSelID);
end;

procedure TMainForm.CreateCards(AImg: TBitmap; AName: string; ACode: string; AID: integer);
begin
  TTask.Run(
    procedure
    var
      vBmp: TBitmap;
      vImgQR: TImage;
      Scale: Single;
      DstX: Extended;
      DstY: Extended;
      AFolderName: string;
    begin
      case ACode.ToInteger() of
        1: AFolderName := 'Броня';
        2: AFolderName := 'Артефакты';
        3: AFolderName := 'Медицина';
        4: AFolderName := 'Оружие';
      end;

      TDirectory.CreateDirectory(GetUserAppPath + '\' + AFolderName);

      vBmp := TBitmap.Create;
      try
        vBmp.LoadFromFile(System.IOUtils.TPath.Combine(GetUserAppPath, 'Card-1.png'));
        vBmp.Canvas.BeginScene;
        Scale := Min(vBmp.Width / 2 / AImg.Width, vBmp.Height / AImg.Height);
        DstX := (vBmp.Width / 2 - AImg.Width * Scale) / 2;
        DstY := (vBmp.Height - AImg.Height * Scale) / 2;

        vBmp.Canvas.DrawBitmap(AImg, RectF(0, 0, AImg.Width, AImg.Height), RectF(DstX, DstY, DstX + AImg.Width * Scale, DstY + AImg.Height * Scale), 1);
        vBmp.Canvas.Font.Size := 100;
        vBmp.Canvas.Font.Style := [TFontStyle.fsBold];
        vBmp.Canvas.Fill.Color := TAlphaColors.Black;
        vBmp.Canvas.FillText(RectF(vBmp.Width / 2, 0, vBmp.Width, vBmp.Height), AName, true, 0.8, [], TTextAlign.Center, TTextAlign.Center);
        vBmp.Canvas.EndScene;
        vBmp.SaveToFile(System.IOUtils.TPath.Combine(GetUserAppPath + '\' + AFolderName, AName + ' - 1.jpg'));

        vImgQR := TImage.Create(nil);
        vImgQR.Width := vBmp.Height;
        vImgQR.Height := vBmp.Height;
        try
          GenerateQRCode('{"code":"' + ACode + AID.ToString.PadLeft(3, '0') + '"}', vImgQR);

          vBmp.LoadFromFile(System.IOUtils.TPath.Combine(GetUserAppPath, 'Card-1.png'));
          vBmp.Canvas.BeginScene;
          vBmp.Canvas.DrawBitmap(vImgQR.Bitmap, RectF(0, 0, vBmp.Width, vBmp.Height), RectF(vBmp.Width / 2 - vBmp.Height / 2, 0, vBmp.Width / 2 + vBmp.Height / 2, vBmp.Height), 1);
          vBmp.Canvas.EndScene;
          vBmp.SaveToFile(System.IOUtils.TPath.Combine(GetUserAppPath+ '\' + AFolderName, AName + ' - 2.jpg'));
        finally
          vImgQR.Free;
        end;
      finally
        vBmp.Free;
      end;
    end);

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadWeaponsToListview;
  LoadArmorsToListview;
  LoadArtsToListview;
  LoadMedicalToListview;
  LoadMap;
  labStatusLoadData.Text := 'Загружено';
end;

procedure TMainForm.LoadMap;
var
  FDQuery: TFDQuery;
begin

  ExeExec('select * from game_data;', exActive, FDQuery);
  FDQuery.First;

  if FDQuery.RecordCount > 0 then
  begin
    imgMap.Hint := FDQuery.FieldByName('map_image_path').AsString;

    TTask.Run(
      procedure
      begin
        imgMap.Bitmap.LoadFromFile(FDQuery.FieldByName('map_image_path').AsString);
      end);

    eTLLat.Text := FDQuery.FieldByName('map_left_top_lat').AsString;
    eTLLon.Text := FDQuery.FieldByName('map_left_top_lon').AsString;
    eBRLat.Text := FDQuery.FieldByName('map_right_bottom_lat').AsString;
    eBRLon.Text := FDQuery.FieldByName('map_right_bottom_lon').AsString;
  end;

  FreeQueryAndConn(FDQuery);
end;

procedure TMainForm.LoadArtsToListview;
var
  FDQuery: TFDQuery;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  layBoardArt.Visible := False;
  recSelect.Parent := nil;

  ExeExec('select * from arts order by art_name;', exActive, FDQuery);
  FDQuery.First;

  if FControlListArts <> nil then
  begin
    try
      FControlListArts.Clear;
      FControlListArts.Visible := False;
      FControlListArts.Parent := nil;
      FreeAndNil(FControlListArts);
      FControlListArts := nil;
    finally
    end;
  end;

  FControlListArts := TControlListItem.Create(layListArts);

  while not(FDQuery.Eof) do
  begin
    listitem := FControlListArts.AddItem();

    with listitem do
    begin
      CenterText := FDQuery.FieldByName('art_name').AsString;
      OnItemClick := ItemArtClick;
      Tag := FDQuery.FieldByName('art_id').AsInteger;
      TagObject := listitem;
      ShowHint := False;
      GetImage.Assign(FDQuery.FieldByName('icon'));

      FDQuery.Next;
    end;
  end;

  FreeQueryAndConn(FDQuery);
  FControlListArts.PrepareForPaint;
  FControlListArts.Repaint;
end;

procedure TMainForm.LoadMedicalToListview;
var
  FDQuery: TFDQuery;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  layBoardMedical.Visible := False;
  recSelect.Parent := nil;

  ExeExec('select * from medical order by name;', exActive, FDQuery);
  FDQuery.First;

  if FControlListMedical <> nil then
  begin
    try
      FControlListMedical.Clear;
      FControlListMedical.Visible := False;
      FControlListMedical.Parent := nil;
      FreeAndNil(FControlListMedical);
      FControlListMedical := nil;
    finally
    end;
  end;

  FControlListMedical := TControlListItem.Create(layListMedical);

  while not(FDQuery.Eof) do
  begin
    listitem := FControlListMedical.AddItem(False);

    with listitem do
    begin
      CenterText := FDQuery.FieldByName('name').AsString;
      OnItemClick := ItemMedicalClick;
      Tag := FDQuery.FieldByName('medical_id').AsInteger;
      TagObject := listitem;
      ShowHint := False;
      GetImage.Assign(FDQuery.FieldByName('icon'));

      FDQuery.Next;
    end;
  end;

  FreeQueryAndConn(FDQuery);
  FControlListMedical.PrepareForPaint;
  FControlListMedical.Repaint;
end;

procedure TMainForm.LoadWeaponsToListview;
var
  FDQuery: TFDQuery;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  layBoardWeapon.Visible := False;
  recSelect.Parent := nil;

  ExeExec('select * from weapons order by name;', exActive, FDQuery);
  FDQuery.First;

  if FControlListWeapons <> nil then
  begin
    try
      FControlListWeapons.Clear;
      FControlListWeapons.Visible := False;
      FControlListWeapons.Parent := nil;
      FreeAndNil(FControlListWeapons);
      FControlListWeapons := nil;
    finally
    end;
  end;

  FControlListWeapons := TControlListItem.Create(layListWeapons);

  while not(FDQuery.Eof) do
  begin
    listitem := FControlListWeapons.AddItem(False);

    with listitem do
    begin
      CenterText := FDQuery.FieldByName('name').AsString;
      OnItemClick := ItemWeaponClick;
      Tag := FDQuery.FieldByName('weapon_id').AsInteger;
      TagObject := listitem;
      ShowHint := False;
      GetImage.Assign(FDQuery.FieldByName('icon'));

      FDQuery.Next;
    end;
  end;

  FreeQueryAndConn(FDQuery);
  FControlListWeapons.PrepareForPaint;
  FControlListWeapons.Repaint;
end;

procedure TMainForm.LoadArmorsToListview;
var
  FDQuery: TFDQuery;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  layBoardArmor.Visible := False;
  recSelect.Parent := nil;

  ExeExec('select * from armors order by name;', exActive, FDQuery);
  FDQuery.First;

  if FControlListArmors <> nil then
  begin
    try
      FControlListArmors.Clear;
      FControlListArmors.Visible := False;
      FControlListArmors.Parent := nil;
      FreeAndNil(FControlListArmors);
      FControlListArmors := nil;
    finally
    end;
  end;

  FControlListArmors := TControlListItem.Create(layListArmors);

  while not(FDQuery.Eof) do
  begin
    listitem := FControlListArmors.AddItem(False);

    with listitem do
    begin
      CenterText := FDQuery.FieldByName('name').AsString;
      recIconBackgroundTransp.Width := recIconBackgroundTransp.Height;
      rcBackground.Margins.Left := 40;
      OnItemClick := ItemArmorClick;
      Tag := FDQuery.FieldByName('armor_id').AsInteger;
      TagObject := listitem;
      ShowHint := False;
      GetImage.Assign(FDQuery.FieldByName('icon'));

      FDQuery.Next;
    end;
  end;

  FreeQueryAndConn(FDQuery);
  FControlListArmors.PrepareForPaint;
  FControlListArmors.Repaint;
end;

procedure TMainForm.ItemArmorClick(FTagObject: TObject; FItem: TFMXObject);
var
  FDQuery: TFDQuery;
begin
  recSelect.Parent := (FItem as TControlItem).rcBackground;
  recSelect.Align := TAlignLayout.Contents;
  recSelect.BringToFront;
  recSelect.Visible := true;
  FSelID := (FItem as TControlItem).Tag;

  ExeExec('select * from armors where armor_id = ' + FSelID.ToString + ';', exActive, FDQuery);
  eNameArmor.Text := FDQuery.FieldByName('name').AsString;
  eCostArmor.Text := FDQuery.FieldByName('cost').AsString;
  imgIconArmor.Bitmap.Assign(FDQuery.FieldByName('icon'));
  sbSlotCount.Value := FDQuery.FieldByName('count_slots').AsInteger;
  sbFire.Value := FDQuery.FieldByName('fire').AsInteger;
  sbElectro.Value := FDQuery.FieldByName('electro').AsInteger;
  sbChimishe.Value := FDQuery.FieldByName('chimishe').AsInteger;
  sbRadiation.Value := FDQuery.FieldByName('radiation').AsInteger;
  sbPSI.Value := FDQuery.FieldByName('psi').AsInteger;
  sbPhisic.Value := FDQuery.FieldByName('phisic').AsInteger;

  layBoardArmor.Visible := true;
  FreeQueryAndConn(FDQuery);
end;

procedure TMainForm.ItemArtClick(FTagObject: TObject; FItem: TFMXObject);
var
  FDQuery: TFDQuery;
begin
  recSelect.Parent := (FItem as TControlItem).rcBackground;
  recSelect.Align := TAlignLayout.Contents;
  recSelect.BringToFront;
  recSelect.Visible := true;
  FSelID := (FItem as TControlItem).Tag;

  ExeExec('select * from arts where art_id = ' + FSelID.ToString + ';', exActive, FDQuery);
  eNameArt.Text := FDQuery.FieldByName('art_name').AsString;
  eCostArt.Text := FDQuery.FieldByName('cost').AsString;
  imgIconArt.Bitmap.Assign(FDQuery.FieldByName('icon'));
  sbFireArt.Value := FDQuery.FieldByName('fire').AsInteger;
  sbElectroArt.Value := FDQuery.FieldByName('electro').AsInteger;
  sbChimisheArt.Value := FDQuery.FieldByName('chimishe').AsInteger;
  sbRadiationArt.Value := FDQuery.FieldByName('radiation').AsInteger;
  sbPSIArt.Value := FDQuery.FieldByName('psi').AsInteger;
  sbPhisicArt.Value := FDQuery.FieldByName('phisic').AsInteger;

  case FDQuery.FieldByName('level').AsInteger of
    1:
      rbLevel1.IsChecked := true;
    2:
      rbLevel2.IsChecked := true;
    3:
      rbLevel3.IsChecked := true;
    4:
      rbLevel4.IsChecked := true;
  end;

  layBoardArt.Visible := true;
  FreeQueryAndConn(FDQuery);
end;

procedure TMainForm.ItemWeaponClick(FTagObject: TObject; FItem: TFMXObject);
var
  FDQuery: TFDQuery;
begin
  recSelect.Parent := (FItem as TControlItem).rcBackground;
  recSelect.Align := TAlignLayout.Contents;
  recSelect.BringToFront;
  recSelect.Visible := true;
  FSelID := (FItem as TControlItem).Tag;

  ExeExec('select * from weapons where weapon_id = ' + FSelID.ToString + ';', exActive, FDQuery);
  eNameWeapon.Text := FDQuery.FieldByName('name').AsString;
  eCostWeapon.Text := FDQuery.FieldByName('cost').AsString;
  ImgIconWeapon.Bitmap.Assign(FDQuery.FieldByName('icon'));

  layBoardWeapon.Visible := true;
  FreeQueryAndConn(FDQuery);
end;

procedure TMainForm.ItemMedicalClick(FTagObject: TObject; FItem: TFMXObject);
var
  FDQuery: TFDQuery;
begin
  recSelect.Parent := (FItem as TControlItem).rcBackground;
  recSelect.Align := TAlignLayout.Contents;
  recSelect.BringToFront;
  recSelect.Visible := true;
  FSelID := (FItem as TControlItem).Tag;

  ExeExec('select * from medical where medical_id = ' + FSelID.ToString + ';', exActive, FDQuery);
  eNameMedical.Text := FDQuery.FieldByName('name').AsString;
  eCostMedical.Text := FDQuery.FieldByName('cost').AsString;
  sbRestoreHitpoint.Text := FDQuery.FieldByName('health_restore').AsString;
  imgIconMedical.Bitmap.Assign(FDQuery.FieldByName('icon'));

  layBoardMedical.Visible := true;
  FreeQueryAndConn(FDQuery);
end;

procedure TMainForm.rbLevel1Change(Sender: TObject);
begin
  if (Sender as TRadioButton).IsChecked then
    layLevel.Tag := (Sender as TRadioButton).Tag;
end;

procedure TMainForm.sbRadiationChange(Sender: TObject);
var
  vCost: double;
begin
  vCost := sbSlotCount.Value / 5 * 10000;
  vCost := vCost + sbFire.Value / 100 * 30000;
  vCost := vCost + sbChimishe.Value / 100 * 30000;
  vCost := vCost + sbElectro.Value / 100 * 30000;
  vCost := vCost + sbPSI.Value / 100 * 30000;
  vCost := vCost + sbPhisic.Value / 100 * 30000;
  vCost := vCost + sbRadiation.Value / 100 * 30000;
  eCostArmor.Text := Round(vCost).ToString;
end;

procedure TMainForm.IdTCPServerExecute(AContext: TIdContext);
begin
  TTask.Run(
    procedure
    var
      vContext: string;
      vPerson: TPerson;
      vAnswer: TAction;
      FDQuery: TFDQuery;
      vStrData: UnicodeString;
      vAction: TAction;
      vStringData: TList<UnicodeString>;
      vStr, vString: UnicodeString;
      I: Integer;
    begin
      vContext := AContext.Connection.Socket.ReadLn(IndyUTF8Encoding(true));
      try
        if vContext = 'PDA ADMIN' then
        begin
          vAnswer := TAction.Create;
          vAnswer.SendType := stUpdateData;
          vAnswer.PageCount := FPageCountAdmin;
          vStrData := FStrDataForAdmin;
        end
        else if Copy(vContext, 1, 7) = 'INSERT:' then
        begin
          try
            ExeExec('delete from arts_to_map; delete from anomalies; delete from places;', exExecute, FDQuery);

            vAction := TJson.JsonToObject<TAction>(Copy(vContext, 8));

            vStringData := TList<UnicodeString>.Create;

            if vAction.PageCount > 0 then
            begin

              for I := 1 to vAction.PageCount do
              begin
                vStr := AContext.Connection.Socket.ReadLn(#13#10, IndyUTF8Encoding(true));
                vStringData.Add(vStr);
              end;
            end;

            case vAction.SendType of
              stUpdateData:
                begin
                  For I := 0 to vStringData.Count - 1 do
                    vString := vString + vStringData[I];

                  ExeExec(vString, exExecute, FDQuery);
                end;
            end;

            vAnswer := TAction.Create;
            vAnswer.SendType := stUpdateData;
            vAnswer.PageCount := 0;
            vStrData := '';
          except
            vAnswer := TAction.Create;
            vAnswer.SendType := stCancel;
            vAnswer.PageCount := 0;
            vStrData := '';
          end;

        end
        else
        begin
          vPerson := TPerson.Create;
          vPerson := TJson.JsonToObject<TPerson>(vContext);
          try

          finally
            vPerson.Free;
          end;
        end;

        AContext.Connection.Socket.WriteLn(TJson.ObjectToJsonString(vAnswer) + #13#10 + vStrData, IndyUTF8Encoding(true));

      finally
        AContext.Connection.Disconnect;
      end;
    end)

end;

end.
