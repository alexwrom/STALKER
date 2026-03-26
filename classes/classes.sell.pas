unit classes.sell;

interface

type
  TSell = class

  private
    FTableName: string;
    FRowID: integer;
    FHealth: double;
    FCost: integer;
  published
    property TableName: string read FTableName write FTableName;
    property RowID: integer read FRowID write FRowID;
    property Health: double read FHealth write FHealth;
    property Cost: integer read FCost write FCost;
  end;

implementation

end.
