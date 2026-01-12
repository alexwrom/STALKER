unit classes.send;

interface

uses uGlobal;

type
  TSend = class
  private
    FCode: string;
    FIp: string;

  published
    property Ip: string read FIp write FIp;
    property Code: string read FCode write FCode;
  end;

implementation

end.
