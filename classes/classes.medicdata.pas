unit classes.medicdata;

interface

type
  TMedicData = class
  private
    FHealth: integer;
    FIsRestore: boolean;

  published
    property Health: integer read FHealth write FHealth;
    property IsRestore: boolean read FIsRestore write FIsRestore;
  end;

implementation

end.

