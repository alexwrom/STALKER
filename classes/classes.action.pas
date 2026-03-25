unit classes.action;

interface

uses uGlobal;

type
  TAction = class
  private
    FSendType: TSendType;
    FJSONObject: UnicodeString;
  published
    property SendType: TSendType read FSendType write FSendType;
    property JSONObject: UnicodeString read FJSONObject write FJSONObject;
  end;

implementation

end.

