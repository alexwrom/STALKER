unit classes.answer;

interface
  type
  TAnswer = class
  private
    FMessage: string;
    FStatus: string;
    FJSON: UnicodeString;
    published
    property Status: string read  FStatus write FStatus;
    property Message: string read  FMessage write FMessage;
    property JSON: UnicodeString read  FJSON write FJSON;
  end;
implementation

end.
