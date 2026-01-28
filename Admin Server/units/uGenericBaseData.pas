unit uGenericBaseData;

interface

uses
  uGlobal, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FireDAC.Comp.Client, StrUtils, FMX.Graphics;

const
  cDefLength = 9000;

function GoGenericBaseData(AIsAdmin: boolean = false): TList<UnicodeString>;
function AddMapToHex: UnicodeString;

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

procedure GenerateTableInsert(ATable: string; var AStrData: TList<UnicodeString>);
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
        if FDQueryCol.FieldByName('name').AsString <> 'map_image_path' then
        begin
          vColumn.Name := FDQueryCol.FieldByName('name').AsString;
          vColumn.TypeCol := FDQueryCol.FieldByName('type').AsString;
          vColumns.Add(vColumn);
        end;
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

            else if vColumns[i].TypeCol = 'BLOB' then
            begin
              vBitmap := TBitmap.Create;
              try
                vBitmap.Assign(FDQuery.FieldByName(vColumns[i].Name));
                vColValue := vColValue + IfThen(i = 0, '', ',') + 'X' + QuotedStr(BitmapToHexString(vBitmap));
              except
                vColValue := vColValue + IfThen(i = 0, '', ',') + 'NULL'
              end;

            end;
          end;

          AStrData.Add('insert into ' + ATable + ' (' + vColName + ') values (' + vColValue + ');');

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

function AddMapToHex: UnicodeString;
var
  FDQuery: TFDQuery;
  vBitmap: TBitmap;
begin
  ExeExec('select map_image_path from game_data;', exActive, FDQuery);
  try
    vBitmap := TBitmap.Create;
    try
      vBitmap.LoadFromFile(FDQuery.FieldByName('map_image_path').AsString);
      Result := BitmapToHexString(vBitmap);
    except
    end;

  finally
    FreeQueryAndConn(FDQuery);
  end;
end;

function GoGenericBaseData(AIsAdmin: boolean = false): TList<UnicodeString>;
begin
  // Порядок важен
  Result := TList<UnicodeString>.Create;

  GenerateTableInsert('arts', Result);
  GenerateTableInsert('places', Result);
  GenerateTableInsert('anomaly_types', Result);
  GenerateTableInsert('anomalies', Result);
  GenerateTableInsert('arts_to_map', Result);

  if not AIsAdmin then
  begin
    GenerateTableInsert('statuses', Result);
    GenerateTableInsert('armors', Result);

    GenerateTableInsert('critical_issuies', Result);
    GenerateTableInsert('detectors', Result);
    GenerateTableInsert('groups', Result);
    GenerateTableInsert('issuies_block', Result);
    GenerateTableInsert('issuies', Result);
    GenerateTableInsert('medical', Result);
    GenerateTableInsert('notifications', Result);

    GenerateTableInsert('weapons', Result);
    GenerateTableInsert('bag', Result);
  end;

  GenerateTableInsert('game_data', Result);
end;

end.
