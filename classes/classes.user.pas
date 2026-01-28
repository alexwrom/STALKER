unit classes.user;

interface

type
  TUser = class
  private
    FUserID: integer;
    FPassword: string;
    FUsername: string;
  published
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property UserID: integer read FUserID write FUserID;
  end;

implementation

end.
