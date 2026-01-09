unit classes.action;

interface

uses uGlobal;

type
  TAction = class
  private
    FSendType: TSendType;
    FJSONObject: string;
  published
    property SendType: TSendType read FSendType write FSendType;
    property JSONObject: string read FJSONObject write FJSONObject;
  end;

implementation

end.
