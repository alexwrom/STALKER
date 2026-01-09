unit classes.send;

interface

uses uGlobal;

type
  TSend = class
  private
    //FSendType: TSendType;
    FIp: string;
    //FJSONObject: string;
  published
    //property SendType: TSendType read FSendType write FSendType;
    property Ip: string read FIp write FIp;
    //property JSONObject: string read FJSONObject write FJSONObject;
  end;

implementation

end.
