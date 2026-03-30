unit uFrameSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.Effects, FMX.Controls.Presentation, FireDAC.Comp.Client, uGlobal, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFrameSettings = class(TFrame)
    recSkin: TRectangle;
    Rectangle5: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    Label8: TLabel;
    Label1: TLabel;
    tbVolume: TTrackBar;
    procedure tbVolumeChange(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AObject: TFmxObject);
    { Public declarations }
  end;

implementation

{$R *.fmx}

constructor TFrameSettings.Create(AObject: TFmxObject);
var
  vQuery : TFDQuery;
begin
  inherited Create(AObject);
  try
    ExeExec('select value from settings where setting_id = ''volume'';', exActive, vQuery);
    FVolume := Round(vQuery.FieldByName('value').AsFloat);
    tbVolume.Value := FVolume;
  finally
    FreeQueryAndConn(vQuery);
  end;
end;

procedure TFrameSettings.tbVolumeChange(Sender: TObject);
var
  vQuery : TFDQuery;
begin
  try
    ExeExec('update settings set value = ' + QuotedStr(Round(tbVolume.Value).ToString)+ ' where setting_id = ''volume'';', exExecute, vQuery);
  finally
  end;

  FVolume := Round(tbVolume.Value);
end;

end.
