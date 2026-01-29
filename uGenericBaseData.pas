unit uGenericBaseData;

interface

uses
  uGlobal, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FireDAC.Comp.Client, StrUtils, FMX.Graphics, Classes.action;

function GoGenericBaseData(): TList<UnicodeString>;

implementation

procedure GenerateTableInsert(ATable: string; var AStrData: TList<UnicodeString>);
var
  FDQuery: TFDQuery;
  FDQueryCol: TFDQuery;
  vColumns: TList<TColumn>;
  vColumn: TColumn;
  i: integer;
  vColName: string;
  vColValue: UnicodeString;
  vStr: string;
begin
  vColumns := TList<TColumn>.Create;
  try
    ExeExec('PRAGMA Table_Info(' + QuotedStr(ATable) + ')', exActive, FDQueryCol);
    try

      FDQueryCol.First;
      while not FDQueryCol.Eof do
      begin
        if FDQueryCol.FieldByName('name').AsString <> 'user_id' then  //Нельзя передавать штатное поле user_id, оно добавляется ниже
        begin
          vColumn.Name := FDQueryCol.FieldByName('name').AsString;
          vColumn.TypeCol := FDQueryCol.FieldByName('type').AsString;
          vColumns.Add(vColumn);
        end;
        FDQueryCol.Next;
      end;
    finally
      FreeQueryAndConn(FDQueryCol);
    end;

    AStrData.Add('delete from ' + ATable + ' where user_id = ' + Person.UserId.ToString + ';');

    ExeExec('select * from ' + ATable + ';', exActive, FDQuery);

    try
      FDQuery.First;

      while not FDQuery.Eof do
      begin
        vColName := '';
        vColValue := '';

        for i := 0 to vColumns.Count - 1 do
        begin
          vColName := vColName + IfThen(i = 0, '', ',') + vColumns[i].Name;

          if (vColumns[i].TypeCol = 'BOOLEAN') or (vColumns[i].TypeCol = 'INTEGER') or (vColumns[i].TypeCol = 'DOUBLE') then
          begin
            if FDQuery.FieldByName(vColumns[i].Name).AsString = '' then
              vColValue := vColValue + IfThen(i = 0, '', ',') + 'NULL'
            else
              vColValue := vColValue + IfThen(i = 0, '', ',') + StringReplace(FDQuery.FieldByName(vColumns[i].Name).AsString, ',', '.', [rfReplaceAll]);
          end
          else if (vColumns[i].TypeCol = 'VARCHAR') or (vColumns[i].TypeCol = 'DATETIME') or (vColumns[i].TypeCol = 'TIME') then
            vColValue := vColValue + IfThen(i = 0, '', ',') + QuotedStr(FDQuery.FieldByName(vColumns[i].Name).AsString);
        end;

        vColName := vColName + ',' + 'user_id';  // Вот здесь user_id
        vColValue := vColValue + ',' + Person.UserId.ToString;

        AStrData.Add('insert into ' + ATable + ' (' + vColName + ') values (' + vColValue + ');');
        FDQuery.Next;
      end;

    finally
      FreeQueryAndConn(FDQuery);
    end;

  finally
    FreeAndNil(vColumns);
  end;

end;

function GoGenericBaseData(): TList<UnicodeString>;
begin
  // Порядок важен
  Result := TList<UnicodeString>.Create;
  GenerateTableInsert('bag', Result);
  GenerateTableInsert('belt', Result);
  GenerateTableInsert('users', Result);
  GenerateTableInsert('life_log', Result);
end;

end.
