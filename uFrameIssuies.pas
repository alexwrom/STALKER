unit uFrameIssuies;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Ani, FMX.Effects, FMX.Objects,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FireDAC.Comp.Client, uGlobal, Generics.Collections,
  System.ImageList, FMX.ImgList, Controls.listitem, Controls.item, FMX.Layouts,
  FMX.TabControl;

type
  TFrameIssuies = class(TFrame)
    ImageList: TImageList;
    layTopMenu: TLayout;
    Layout3: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    imgBtnActive: TImage;
    btnToActive: TSpeedButton;
    Image14: TImage;
    imgBtnComplete: TImage;
    btnToComplete: TSpeedButton;
    Image15: TImage;
    imgBtnCancel: TImage;
    btnToCancel: TSpeedButton;
    Image1: TImage;
    recSelectMenu: TRectangle;
    ImgBtnInfo: TImage;
    btnToInfo: TSpeedButton;
    Image12: TImage;
    Image2: TImage;
    TabControl: TTabControl;
    TabIssuies: TTabItem;
    TabInfo: TTabItem;
    Image8: TImage;
    layIssuiesActive: TLayout;
    recSelect: TRectangle;
    Layout1: TLayout;
    lab: TLabel;
    Image11: TImage;
    labDetailIssueActive: TLabel;
    Label2: TLabel;
    layAdditionalActive: TLayout;
    Rectangle1: TRectangle;
    Image9: TImage;
    Image3: TImage;
    Image16: TImage;
    Image17: TImage;
    layControlNotifications: TLayout;
    recSelectNotification: TRectangle;
    Layout10: TLayout;
    Label8: TLabel;
    labMessageText: TLabel;
    layDownload: TLayout;
    Image4: TImage;
    btnDownloadInfo: TCornerButton;
    procedure btnToActiveClick(Sender: TObject);
    procedure btnToCompleteClick(Sender: TObject);
    procedure btnToCancelClick(Sender: TObject);
    procedure btnToInfoClick(Sender: TObject);
  private
    FAllIssueList: TList<TIssueData>;
    FNotificationsList: TList<TNotificationData>;
    FControlListIssuies: TControlListItem;
    FControlListAdditional: TControlListItem;
    FControlListNotifications: TControlListItem;
    procedure LoadIssuiesData;
    procedure ItemClick(FTagObject: TObject; FItem: TFMXObject);
    procedure LoadInfo;
    procedure LoadInfoData;
    procedure ItemNotificationClick(FTagObject: TObject; FItem: TFMXObject);

    { Private declarations }
  public
    { Public declarations }
    procedure LoadIssuies(AStatus: integer);
    procedure ClearSelection;
    constructor Create(AObject: TFMXObject);
  end;

implementation

{$R *.fmx}

constructor TFrameIssuies.Create(AObject: TFMXObject);
begin
  inherited Create(AObject);
end;

procedure TFrameIssuies.LoadIssuiesData;
var
  vQuery: TFDQuery;
  vIssueList: TIssueData;
begin
  if Assigned(FAllIssueList) then
    FAllIssueList.Clear
  else
    FAllIssueList := TList<TIssueData>.Create;

  ExeExec('select * from all_issuies where user_id = ' + Person.UserId.ToString + ';', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vIssueList.Coords.Latitude := vQuery.FieldByName('lat').AsFloat;
    vIssueList.Coords.Longitude := vQuery.FieldByName('lon').AsFloat;
    vIssueList.ID := vQuery.FieldByName('issue_id').AsInteger;
    vIssueList.PrevID := vQuery.FieldByName('prev_issue_id').AsInteger;
    vIssueList.Cost := vQuery.FieldByName('cost').AsInteger;
    vIssueList.Name := vQuery.FieldByName('name').AsString;
    vIssueList.Detail := vQuery.FieldByName('detail').AsString;
    vIssueList.BlockDetail := vQuery.FieldByName('block_detail').AsString;
    vIssueList.RadiusIN := vQuery.FieldByName('radius_in').AsInteger;
    vIssueList.RadiusOUT := vQuery.FieldByName('radius_out').AsInteger;
    vIssueList.CompleteAfterOUT := vQuery.FieldByName('complete_after_out').AsBoolean;
    vIssueList.CompleteAfterIN := vQuery.FieldByName('complete_after_in').AsBoolean;
    vIssueList.StatusID := vQuery.FieldByName('status_id').AsInteger;
    vIssueList.BlockStatusID := vQuery.FieldByName('block_status_id').AsInteger;
    FAllIssueList.Add(vIssueList);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFrameIssuies.LoadInfo;
var
  I: integer;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  LoadInfoData;

  recSelectNotification.Parent := nil;

  if FControlListNotifications <> nil then
  begin
    try
      FControlListNotifications.Clear;
      FControlListNotifications.Visible := false;
      FControlListNotifications.Parent := nil;
      FreeAndNil(FControlListNotifications);
      FControlListNotifications := nil;
    finally
    end;
  end;

  FControlListNotifications := TControlListItem.Create(layControlNotifications);

  for I := 0 to FNotificationsList.Count - 1 do
  begin
    listitem := FControlListNotifications.AddItem(false);

    with listitem do
    begin
      CenterText := FNotificationsList[I].Name;
      listitem.Height := 70;
      OnItemClick := ItemNotificationClick;
      Tag := FNotificationsList[I].ID;
      TagObject := listitem;
      ShowHint := false;
      Hint := FNotificationsList[I].MessageText;

      if FNotificationsList[I].IsOpen then
      begin
        GetImage.Assign(ImageList.Source[4].MultiResBitmap[0].Bitmap);
        labCenterText.TextSettings.FontColor := TAlphaColors.Darkgray;
      end
      else
      begin
        GetImage.Assign(ImageList.Source[3].MultiResBitmap[0].Bitmap);
        labCenterText.TextSettings.FontColor := $FFFF8707;
      end;
    end;
  end;

  FControlListNotifications.PrepareForPaint;
  FControlListNotifications.Repaint;
end;

procedure TFrameIssuies.LoadInfoData;
var
  vQuery: TFDQuery;
  vNotifItem: TNotificationData;
begin
  if Assigned(FNotificationsList) then
    FNotificationsList.Clear
  else
    FNotificationsList := TList<TNotificationData>.Create;

  ExeExec('select * from notifications_for_me where group_id = ' + Person.GroupId.ToString + ';', exActive, vQuery);
  vQuery.First;

  while Not vQuery.Eof do
  begin
    vNotifItem.ID := vQuery.FieldByName('notification_id').AsInteger;
    vNotifItem.Name := vQuery.FieldByName('name').AsString;
    vNotifItem.MessageText := vQuery.FieldByName('message').AsString;
    vNotifItem.IsOpen := vQuery.FieldByName('is_open').AsBoolean;
    vNotifItem.LoadData := vQuery.FieldByName('load_data').AsString;
    FNotificationsList.Add(vNotifItem);
    vQuery.Next;
  end;

  FreeQueryAndConn(vQuery);
end;

procedure TFrameIssuies.btnToActiveClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabIssuies;
  recSelectMenu.Parent := imgBtnActive;
  LoadIssuies(0);
end;

procedure TFrameIssuies.btnToCancelClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabIssuies;
  recSelectMenu.Parent := imgBtnCancel;
  LoadIssuies(2);
end;

procedure TFrameIssuies.btnToCompleteClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabIssuies;
  recSelectMenu.Parent := imgBtnComplete;
  LoadIssuies(1);
end;

procedure TFrameIssuies.btnToInfoClick(Sender: TObject);
begin
  recSelectNotification.Parent := nil;
  labMessageText.Text := '';
  TabControl.ActiveTab := TabInfo;
  recSelectMenu.Parent := ImgBtnInfo;
  LoadInfo;
end;

procedure TFrameIssuies.ClearSelection;
begin
  recSelect.Parent := nil;
  labDetailIssueActive.Text := '';

  if FControlListAdditional <> nil then
  begin
    try
      FControlListAdditional.Clear;
      FControlListAdditional.Visible := false;
      FControlListAdditional.Parent := nil;
      FreeAndNil(FControlListAdditional);
      FControlListAdditional := nil;
    finally
    end;
  end;
end;

procedure TFrameIssuies.LoadIssuies(AStatus: integer);
var
  I: integer;
  vItem: TListViewItem;
  listitem: TControlItem;
begin
  ClearSelection;
  LoadIssuiesData;

  recSelect.Parent := nil;

  if FControlListIssuies <> nil then
  begin
    try
      FControlListIssuies.Clear;
      FControlListIssuies.Visible := false;
      FControlListIssuies.Parent := nil;
      FreeAndNil(FControlListIssuies);
      FControlListIssuies := nil;
    finally
    end;
  end;

  FControlListIssuies := TControlListItem.Create(layIssuiesActive);

  for I := 0 to FAllIssueList.Count - 1 do
    if (FAllIssueList[I].PrevID = 0) and (FAllIssueList[I].StatusID <> -1) and (FAllIssueList[I].BlockStatusID = AStatus) then
    begin
      listitem := FControlListIssuies.AddItem(false);

      with listitem do
      begin
        CenterText := FAllIssueList[I].Name;
        listitem.Height := 70;
        OnItemClick := ItemClick;
        Tag := FAllIssueList[I].ID;
        TagObject := listitem;
        ShowHint := false;
        Hint := FAllIssueList[I].BlockDetail;
        GetImage.Assign(ImageList.Source[FAllIssueList[I].BlockStatusID].MultiResBitmap[0].Bitmap)
      end;
    end;

  FControlListIssuies.PrepareForPaint;
  FControlListIssuies.Repaint;
end;

procedure TFrameIssuies.ItemNotificationClick(FTagObject: TObject; FItem: TFMXObject);
var
  vQuery: TFDQuery;
begin
  recSelectNotification.Parent := (FItem as TControlItem).rcBackground;
  recSelectNotification.Align := TAlignLayout.Bottom;
  recSelectNotification.BringToFront;
  recSelectNotification.Visible := true;
  labMessageText.Text := (FItem as TControlItem).Hint;
  (FItem as TControlItem).GetImage.Assign(ImageList.Source[4].MultiResBitmap[0].Bitmap);
  (FItem as TControlItem).labCenterText.TextSettings.FontColor := TAlphaColors.Darkgray;
  layDownload.Visible := Pos('#download', labMessageText.Text) > 0;
  ExeExec('update notifications set is_open = true where notification_id = ' + (FItem as TControlItem).Tag.ToString + ';', exExecute, vQuery);
end;

procedure TFrameIssuies.ItemClick(FTagObject: TObject; FItem: TFMXObject);
var
  I: integer;
  listitem: TControlItem;
  vSearchID: integer;

  procedure AddToList;
  begin
    vSearchID := FAllIssueList[I].ID;

    listitem := FControlListAdditional.AddItem(false);

    with listitem do
    begin
      layTop.Height := 60;
      layTop.Margins.Top := 0;
      labCenterText.TextSettings.Font.Size := 13;
      CenterText := FAllIssueList[I].Detail;

      Enabled := false;
      GetImage.Assign(ImageList.Source[FAllIssueList[I].StatusID].MultiResBitmap[0].Bitmap);

    end;
  end;

begin
  recSelect.Parent := (FItem as TControlItem).rcBackground;
  recSelect.Align := TAlignLayout.Bottom;
  recSelect.BringToFront;
  recSelect.Visible := true;
  labDetailIssueActive.Text := (FItem as TControlItem).Hint;

  vSearchID := (FItem as TControlItem).Tag;

  if FControlListAdditional <> nil then
  begin
    try
      FControlListAdditional.Clear;
      FControlListAdditional.Visible := false;
      FControlListAdditional.Parent := nil;
      FreeAndNil(FControlListAdditional);
      FControlListAdditional := nil;
    finally
    end;
  end;

  FControlListAdditional := TControlListItem.Create(layAdditionalActive);

  I := 0;

  while I <= FAllIssueList.Count - 1 do
  begin
    if (FAllIssueList[I].ID = vSearchID) and (FAllIssueList[I].StatusID <> -1) then
    begin
      AddToList;
      Break;
    end;

    Inc(I);
  end;
  I := 0;

  while I <= FAllIssueList.Count - 1 do
  begin
    if (FAllIssueList[I].PrevID = vSearchID) and (FAllIssueList[I].StatusID <> -1) then
    begin
      AddToList;
      I := 0;
    end;

    Inc(I);
  end;

  FControlListAdditional.PrepareForPaint;
  FControlListAdditional.Repaint;

end;

end.
