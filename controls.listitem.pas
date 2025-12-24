unit controls.listitem;

interface

uses System.SysUtils, System.Types, System.UITypes, System.classes,
  System.Variants, FMX.Objects, FMX.StdCtrls, FMX.Layouts,
  FMX.Types, FMX.controls, FMX.Graphics, controls.item;

type
  TControlListItem = class(TVertScrollBox)
  public
    function AddItem(AddRightButtom: Boolean = false): TControlItem;
    procedure DeleteItem(AObject: TObject);
    procedure Clear;
    function Count: integer;
    constructor Create(AOwner: TFMXObject); overload;
  private
    procedure vsbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure vsbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure vsbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  end;

implementation

constructor TControlListItem.Create(AOwner: TFMXObject);
begin
  inherited Create(AOwner);
  Self.Parent := AOwner;
  Self.Align := TAlignLayout.Client;
  Self.OnMouseDown := vsbMouseDown;
  Self.OnMouseUp := vsbMouseUp;
  Self.OnMouseMove := vsbMouseMove;
  Self.Height := 0;
end;

procedure TControlListItem.vsbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Self.HelpKeyword := Y.ToString;
end;

procedure TControlListItem.vsbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    Self.ScrollBy(0, -Self.HelpKeyword.ToSingle + Y);
    Self.HelpKeyword := Y.ToString;
  end;
end;

procedure TControlListItem.vsbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  lObject: TFMXObject;
begin
  if Self.Content.ChildrenCount > 0 then
    for lObject in Self.Content.Children do
      if lObject is TControlItem then
      begin
        (lObject as TControlItem).rcBackground.HitTest := true;
        (lObject as TControlItem).rcBackground.Fill.Color := $01000000
      end;

end;

function TControlListItem.AddItem(AddRightButtom: Boolean = false): TControlItem;
begin
  result := TControlItem.Create(Self, AddRightButtom);
end;

procedure TControlListItem.DeleteItem(AObject: TObject);
var
  lObject, lChild: TFMXObject;
  lChildChild: TFMXObject;
begin
  if Self.Content.ChildrenCount > 0 then
    for lObject in Self.Content.Children do
      if lObject is TControlItem then
        if (lObject as TControlItem).TagObject = AObject then
        begin
          for lChild in lObject.Children do
          begin
            for lChildChild in lChild.Children do
            begin
              lChildChild.Parent := nil;
              lChildChild.Free;
            end;
            lChild.Parent := nil;
            lChild.Free;
          end;
          lObject.Parent := nil;
          lObject.Free;
        end;
end;

procedure TControlListItem.Clear();
var
  lObject, lChild: TFMXObject;
  lChildChild: TFMXObject;
begin
  if Self.Content.ChildrenCount > 0 then
    for lObject in Self.Content.Children do
      if lObject is TControlItem then
      begin
        for lChild in lObject.Children do
        begin
          for lChildChild in lChild.Children do
          begin
            lChildChild.Parent := nil;
            lChildChild.Free;
          end;
          lChild.Parent := nil;
          lChild.Free;
        end;
        lObject.Parent := nil;
        lObject.Free;
      end;
end;

function TControlListItem.Count(): integer;
begin
  result := Self.Content.ChildrenCount;
end;

end.
