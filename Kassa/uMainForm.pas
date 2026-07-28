unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.Objects, DelphiZXingQRCode;

type
  TMainForm = class(TForm)
    Image1: TImage;
    Layout1: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    CornerButton1: TCornerButton;
    CornerButton2: TCornerButton;
    CornerButton3: TCornerButton;
    CornerButton4: TCornerButton;
    CornerButton5: TCornerButton;
    CornerButton6: TCornerButton;
    CornerButton7: TCornerButton;
    CornerButton8: TCornerButton;
    CornerButton9: TCornerButton;
    btnCheck: TCornerButton;
    labSumma: TLabel;
    CornerButton11: TCornerButton;
    btnClear: TCornerButton;
    btnComeback: TCornerButton;
    labOk: TLabel;
    procedure CornerButton11Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnComebackClick(Sender: TObject);
  private
    procedure GenerateQRCode(const AText: string; AImage: TImage; AIsCheck: boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.btnCheckClick(Sender: TObject);
begin
  GenerateQRCode('{"code":"'+ ('-' + labSumma.Text).PadLeft(7,'0')+ '"}', Image1, true);
  labOk.Visible := true;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  labSumma.Text := '0';
  Image1.Bitmap.Clear(TAlphaColors.Slategray);
  MainForm.Fill.Color := TAlphaColors.Slategray;
end;

procedure TMainForm.btnComebackClick(Sender: TObject);
begin
  GenerateQRCode('{"code":"'+ (labSumma.Text).PadLeft(7,'0')+ '"}', Image1, false);
  labOk.Visible := true;
end;

procedure TMainForm.CornerButton11Click(Sender: TObject);
begin
 if labOk.Visible then
   begin
     labSumma.Text := (Sender as TCornerButton).Text;
     labOk.Visible := false;
     Image1.Bitmap.Clear(TAlphaColors.Slategray);
     MainForm.Fill.Color := TAlphaColors.Slategray;
   end
 else
   if labSumma.Text = '0' then
     labSumma.Text := (Sender as TCornerButton).Text
   else
     if Length(labSumma.Text) < 6 then
       labSumma.Text := labSumma.Text + (Sender as TCornerButton).Text;


end;

procedure TMainForm.GenerateQRCode(const AText: string; AImage: TImage; AIsCheck: boolean);
var
  QRCode: TDelphiZXingQRCode;
  Bitmap: TBitmap;
  Row: integer;
  Col: integer;
  vScale: Single;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := AText;
    QRCode.Encoding := TQRCodeEncoding.qrAuto;
    QRCode.QuietZone := 4;

    vScale := AImage.Height / QRCode.Rows;

    Bitmap := TBitmap.Create();
    try
      Bitmap.SetSize(Round(AImage.Height), Round(AImage.Height));

      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Col := 0 to QRCode.Columns - 1 do
        begin
          if not Bitmap.Canvas.BeginScene then
            Exit;
          try
            Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;

            if (QRCode.IsBlack[Row, Col]) then
              Bitmap.Canvas.Fill.Color := TAlphaColors.Black
            else
              Bitmap.Canvas.Fill.Color := TAlphaColors.White;

            if AIsCheck then
               MainForm.Fill.Color := TAlphaColors.Darksalmon
            else
               MainForm.Fill.Color := TAlphaColors.Darkseagreen;

            // Рисуем пиксель
            Bitmap.Canvas.FillRect(RectF(Col * vScale, Row * vScale, Col * vScale + vScale, Row * vScale + vScale), // Прямоугольник 1x1
              0, 0, [], 1.0);
          finally
            Bitmap.Canvas.EndScene;
          end;
        end;
      end;
      AImage.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  finally
    QRCode.Free;
  end;
end;
end.
