unit classes.data;

interface
uses Generics.Collections;

type
  TData = class
  private
    FSQL: TList<UnicodeString>;
  published
    property SQL: TList<UnicodeString> read FSQL write FSQL;
  end;
implementation

end.
