unit uGenericBaseData;

interface

uses
  uGlobal, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FireDAC.Comp.Client, StrUtils, FMX.Graphics;

function GoGenericBaseData(AUserID: integer): TList<UnicodeString>;

implementation

function BitmapToHexString(Bitmap: TBitmap): string;
var
  Stream: TMemoryStream;
  i: integer;
  pdByte: PByte;
begin
  Result := '';
  if not Assigned(Bitmap) then
    Exit;

  Stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Stream);
    Stream.Position := 0;

    pdByte := Stream.Memory;
    for i := 0 to Stream.Size - 1 do
    begin
      Result := Result + IntToHex(pdByte^, 2);
      Inc(pdByte);
    end;
  finally
    Stream.Free;
  end;
end;

function GenerateTableInsert(ATable: string): TList<UnicodeString>;
var
  FDQuery: TFDQuery;
  FDQueryCol: TFDQuery;
  vColumns: TList<TColumn>;
  vColumn: TColumn;
  i: integer;
  vColName: string;
  vColValue: UnicodeString;
  vBitmap: TBitmap;
begin
  Result := TList<UnicodeString>.Create;
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
            else if (vColumns[i].TypeCol = 'VARCHAR') or (vColumns[i].TypeCol = 'DATETIME') then
              vColValue := vColValue + IfThen(i = 0, '', ',') + QuotedStr(FDQuery.FieldByName(vColumns[i].Name).AsString)

            else if vColumns[i].TypeCol = 'BLOB' then
            begin
              vBitmap := TBitmap.Create;
              vBitmap.Assign(FDQuery.FieldByName(vColumns[i].Name));
              vColValue := vColValue + IfThen(i = 0, '', ',') + 'X' + QuotedStr(BitmapToHexString(vBitmap));
            end;
          end;

          Result.Add('insert into ' + ATable + ' (' + vColName + ') values (' + vColValue + ');');
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

function GoGenericBaseData(AUserID: integer): TList<UnicodeString>;
begin
  Result := TList<UnicodeString>.Create;
  // Порядок важен
  Result.AddRange(GenerateTableInsert('statuses'));
  Result.AddRange(GenerateTableInsert('armors'));
  Result.AddRange(GenerateTableInsert('anomaly_types'));
  Result.AddRange(GenerateTableInsert('anomalies'));
  Result.AddRange(GenerateTableInsert('arts'));
  Result.AddRange(GenerateTableInsert('critical_issuies'));
  Result.AddRange(GenerateTableInsert('detectors'));
  Result.AddRange(GenerateTableInsert('groups'));
  Result.AddRange(GenerateTableInsert('issuies_block'));
  Result.AddRange(GenerateTableInsert('issuies'));
  Result.AddRange(GenerateTableInsert('medical'));
  Result.AddRange(GenerateTableInsert('notifications'));
  Result.AddRange(GenerateTableInsert('places'));
  Result.AddRange(GenerateTableInsert('weapons'));
end;

end.
