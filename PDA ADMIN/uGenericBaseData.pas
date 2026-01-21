unit uGenericBaseData;

interface

uses
  uGlobal, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FireDAC.Comp.Client, StrUtils, FMX.Graphics, Classes.action;

function GoGenericBaseData(var APageCount: integer): UnicodeString;

implementation

procedure GenerateTableInsert(ATable: string; var AStrData: string; var APageCount: integer);
var
  FDQuery: TFDQuery;
  FDQueryCol: TFDQuery;
  vColumns: TList<TColumn>;
  vColumn: TColumn;
  i: integer;
  vColName: string;
  vColValue: UnicodeString;
  vBitmap: TBitmap;
  vStr: string;
begin
  vColumns := TList<TColumn>.Create;
  try
    ExeExec('PRAGMA Table_Info(' + QuotedStr(ATable) + ')', exActive, FDQueryCol);
    try

      FDQueryCol.First;
      while not FDQueryCol.Eof do
      begin
        vColumn.Name := FDQueryCol.FieldByName('name').AsString;
        vColumn.TypeCol := FDQueryCol.FieldByName('type').AsString;
        vColumns.Add(vColumn);
        FDQueryCol.Next;
      end;

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
                vColValue := vColValue + IfThen(i = 0, '', ',') + QuotedStr(FDQuery.FieldByName(vColumns[i].Name).AsString)

            end;

            vStr := 'insert into ' + ATable + ' (' + vColName + ') values (' + vColValue + ');';
            AStrData := AStrData + IfThen(AStrData = '', '', #13#10) + vStr;
            APageCount := APageCount + 1;
            FDQuery.Next;
          end;

        finally
          FreeQueryAndConn(FDQuery);
        end;
    finally
      FreeQueryAndConn(FDQueryCol);
    end;
  finally
    FreeAndNil(vColumns);
  end;

end;

function GoGenericBaseData(var APageCount: integer): UnicodeString;
begin
  // Порядок важен
  GenerateTableInsert('anomalies', Result, APageCount);
  GenerateTableInsert('arts_to_map', Result, APageCount);
  GenerateTableInsert('places', Result, APageCount);
end;

end.
