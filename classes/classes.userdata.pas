unit classes.userdata;

interface
uses classes.data;

type
  TUserData = class
  private
    FUserID: integer;
    FData: TData;

  published
    property UserID: integer read FUserID write FUserID;
    property Data: TData read FData write FData;
  end;
implementation

end.
