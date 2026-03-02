unit classes.send;

interface

uses uGlobal, classes.marker;

type
  TSend = class
  private
    FCode: string;
    FIp: string;
    FMarker: TMarker;

  published
    property Ip: string read FIp write FIp;
    property Code: string read FCode write FCode;
    property Marker: TMarker read FMarker write FMarker;
  end;

implementation

end.
