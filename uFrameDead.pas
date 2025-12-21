unit uFrameDead;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Ani, FMX.Effects, FMX.Objects;

type
  TFrameDead = class(TFrame)
    Image9: TImage;
    Image1: TImage;
    GlowEffect1: TGlowEffect;
    animGlow: TFloatAnimation;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
