unit classes.tehnicdata;

interface

type
  TTehnicData = class
  private
    FLevel: integer;
    FIsFree: boolean;

  published
    property Level: integer read FLevel write FLevel;
    property IsFree: boolean read FIsFree write FIsFree;
  end;

implementation

end.

