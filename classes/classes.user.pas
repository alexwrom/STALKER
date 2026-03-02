unit classes.user;

interface

type
  TUser = class
  private
    FUsername: string;
    FUserID: integer;
    FPassword: string;
  published
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property UserID: integer read FUserID write FUserID;
  end;

implementation

end.
