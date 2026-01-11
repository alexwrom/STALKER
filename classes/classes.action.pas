unit classes.action;

interface

uses uGlobal;

type
  TAction = class
  private
    FSendType: TSendType;
    FJSONObject: UnicodeString;
    FPageCount: integer;
  published
    property SendType: TSendType read FSendType write FSendType;
    property JSONObject: UnicodeString read FJSONObject write FJSONObject;
    property PageCount: integer read FPageCount write FPageCount;
  end;

implementation

end.

