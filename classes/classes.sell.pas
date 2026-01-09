unit classes.sell;

interface

type
  TSell = class

  private
    FTableName: string;
    FRowID: integer;
    FHealth: double;
    FCost: Extended;
  published
    property TableName: string read FTableName write FTableName;
    property RowID: integer read FRowID write FRowID;
    property Health: double read FHealth write FHealth;
    property Cost: Extended read FCost write FCost;
  end;

implementation

end.
