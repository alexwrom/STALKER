unit uApiServer;

interface

uses
  IdHTTPServer, IdContext, IdCustomHTTPServer, IdSSLOpenSSL, System.JSON,
  System.SysUtils, System.Classes, Rest.JSON, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, StrUtils,
  System.Threading,
  Generics.Defaults, Generics.Collections, uGlobal,
  Classes.Data, uGenericBaseData;

type
  TApiServer = class
  private
    FHttpServer: TIdHTTPServer;

    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure SendJsonResponse(AResponseInfo: TIdHTTPResponseInfo; AStatusCode: Integer; AJson: TJSONObject);
    function GetData(aUserName, APassword: string; AUserId: Integer): TJSONObject;
    function GetDataAdmin: TJSONObject;
    function GetMap: TJSONObject;
    function UploadData(ASQLList: TList<UnicodeString>): TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(APort: Word);
    procedure Stop;
  end;

implementation

constructor TApiServer.Create;
begin
  FHttpServer := TIdHTTPServer.Create(nil);
  FHttpServer.OnCommandGet := HandleRequest;
  FHttpServer.OnCommandOther := HandleRequest;
end;

destructor TApiServer.Destroy;
begin
  Stop;
  FHttpServer.Free;
  inherited;
end;

procedure TApiServer.SendJsonResponse(AResponseInfo: TIdHTTPResponseInfo; AStatusCode: Integer; AJson: TJSONObject);
begin
  AResponseInfo.ResponseNo := AStatusCode;
  AResponseInfo.ContentType := 'application/x-www-form-urlencoded; charset=utf-8';
  AResponseInfo.ContentText := AJson.ToString;
end;

procedure TApiServer.Start(APort: Word);
begin
  FHttpServer.DefaultPort := APort;
  FHttpServer.Active := True;
  WriteLn(Format('API server started to port %d', [APort]));
end;

procedure TApiServer.Stop;
begin
  FHttpServer.Active := False;
  WriteLn('API server stopped');
end;

procedure TApiServer.HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  JsonResponse: TJSONObject;
  Login, Password: string;
  StreamReader: TStreamReader;
  JsonValue: TJSONValue;
  vDocument: string;
  AData: string;
  UserId: Integer;
  vData: TData;
begin

  // Устанавливаем заголовки CORS (для веб-приложений)
  AResponseInfo.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  AResponseInfo.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, OPTIONS');
  AResponseInfo.CustomHeaders.Add('Access-Control-Allow-Headers: Content-Type');

  // Обработка OPTIONS запроса для CORS
  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 204;
    AResponseInfo.ContentText := '';
    Exit;
  end;

  vDocument := ARequestInfo.Document;
  // Проверяем, что запрос идет к эндпоинту  GET
  if (ARequestInfo.Command = 'GET') then
  begin
    try
      // Парсим JSON из тела запроса
      if Assigned(ARequestInfo.PostStream) then
      begin
        ARequestInfo.PostStream.Position := 0;
        StreamReader := TStreamReader.Create(ARequestInfo.PostStream, TEncoding.UTF8);
        JsonValue := TJSONObject.ParseJSONValue(StreamReader.ReadToEnd);
      end;

      try
        // Получение списка настроек для ADMIN PDA
        if (vDocument = '/api/get_data_admin') then
        begin
          JsonResponse := GetDataAdmin;
        end
        else if (vDocument = '/api/get_map') then
        begin
          JsonResponse := GetMap;
        end;

        SendJsonResponse(AResponseInfo, 200, JsonResponse);

      finally
        JsonResponse.Free;
        // JsonValue.Free;
      end;
    except
      on E: Exception do
      begin

        // Ошибка обработки запроса
        JsonResponse := TJSONObject.Create;
        try
          JsonResponse.AddPair('status', 'error');
          JsonResponse.AddPair('message', 'Ошибка обработки запроса: ' + E.Message);
          SendJsonResponse(AResponseInfo, 400, JsonResponse);
        finally
          JsonResponse.Free;
        end;
      end;
    end;
  end
  else if (ARequestInfo.Command = 'POST') then
  begin
    try
      // Парсим JSON из тела запроса
      ARequestInfo.PostStream.Position := 0;
      StreamReader := TStreamReader.Create(ARequestInfo.PostStream, TEncoding.UTF8);
      JsonValue := TJSONObject.ParseJSONValue(StreamReader.ReadToEnd);
      AData := (JsonValue as TJSONObject).ToString;

      try
        // Авторизация
        if (vDocument = '/api/get_data') then
        begin
          Login := (JsonValue as TJSONObject).GetValue('username').Value;
          Password := (JsonValue as TJSONObject).GetValue('password').Value;
          UserId := StrToInt((JsonValue as TJSONObject).GetValue('user_id').Value);

          JsonResponse := GetData(Login, Password, UserId);
        end
        else if (vDocument = '/api/upload_data_from_admin') then
        begin
          vData := TJSON.JsonToObject<TData>(AData);

          JsonResponse := UploadData(vData.SQL);
        end;

        SendJsonResponse(AResponseInfo, 200, JsonResponse);

      finally
        JsonResponse.Free;
        JsonValue.Free;
      end;
    except
      on E: Exception do
      begin
        // Ошибка обработки запроса
        JsonResponse := TJSONObject.Create;
        try
          JsonResponse.AddPair('status', 'error');
          JsonResponse.AddPair('message', 'Ошибка обработки запроса: ' + E.Message);
          SendJsonResponse(AResponseInfo, 400, JsonResponse);
        finally
          JsonResponse.Free;
        end;
      end;
    end;
  end
  else
  begin
    // Неизвестный эндпоинт
    JsonResponse := TJSONObject.Create;
    try
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Неизвестный эндпоинт');
      SendJsonResponse(AResponseInfo, 404, JsonResponse);
    finally
      JsonResponse.Free;
    end;
  end;
end;

// Авторизация по логину и паролю
function TApiServer.GetData(aUserName: string; APassword: string; AUserId: Integer): TJSONObject;
var
  vQuery: TFDQuery;
  JsonResponse: TJSONObject;
  AData: TData;
  I: Integer;
begin
  Result := nil;

  try
    ExeExec('select count(*) as cnt from users where nickname = ' + QuotedStr(aUserName) + ';', exActive, vQuery);

    if vQuery.FieldByName('cnt').AsInteger = 0 then // Создаем пользователя и высылаем все данные
    begin
      FreeQueryAndConn(vQuery);

      ExeExec('insert into users (nickname, group_id) values (' + QuotedStr(aUserName) + ', 1);', exExecute, vQuery);

      ExeExec('select user_id from users where nickname = ' + QuotedStr(aUserName) + ';', exActive, vQuery);

      FullData.Insert(0, 'insert into users (user_id, nickname, group_id) values (' + vQuery.FieldByName('user_id').AsString + ',' + QuotedStr(aUserName) + ', 1);');

      AData := TData.Create;
      AData.SQL := TList<UnicodeString>.Create;

      for I := 0 to FullData.Count - 1 do
        AData.SQL.Add(FullData[I]);

      JsonResponse := TJSONObject.Create;
      try
        JsonResponse.AddPair('status', 'success');
        JsonResponse.AddPair('message', 'Авторизация успешна');
        JsonResponse.AddPair('JSON', TJSON.ObjectToJsonString(AData));
        Result := JsonResponse;
      finally
      end;
    end
    else
    begin

      if AUserId = -1 then // Если в базе есть логин, а на телефоне не зарегистрирован, то возвращаем ошибку
      begin
        JsonResponse := TJSONObject.Create;
        try
          JsonResponse.AddPair('status', 'error');
          JsonResponse.AddPair('message', 'Такой сталкер уже существует');
          Result := JsonResponse;
        finally
        end;
      end
      else // Ищем для него информацию по уведомлениях
      begin

      end;
    end;

  finally
    FreeQueryAndConn(vQuery);
  end;
end;

function TApiServer.GetDataAdmin(): TJSONObject;
var
  vQuery: TFDQuery;
  JsonResponse: TJSONObject;
  AData: TData;
  I: Integer;
begin
  Result := nil;

  AData := TData.Create;
  AData.SQL := TList<UnicodeString>.Create;

  for I := 0 to AdminData.Count - 1 do
    AData.SQL.Add(AdminData[I]);

  JsonResponse := TJSONObject.Create;
  try
    JsonResponse.AddPair('status', 'success');
    JsonResponse.AddPair('message', 'Данные отправлены');
    JsonResponse.AddPair('JSON', TJSON.ObjectToJsonString(AData));
    Result := JsonResponse;
  finally
  end;

end;

function TApiServer.GetMap(): TJSONObject;
var
  JsonResponse: TJSONObject;
begin
  Result := nil;

  JsonResponse := TJSONObject.Create;
  try
    JsonResponse.AddPair('status', 'success');
    JsonResponse.AddPair('message', 'Данные отправлены');
    JsonResponse.AddPair('JSON', AddMapToHex);
    Result := JsonResponse;
  finally
  end;

end;

function TApiServer.UploadData(ASQLList: TList<UnicodeString>): TJSONObject;
var
  vQuery: TFDQuery;
  JsonResponse: TJSONObject;
  vSQLText: UnicodeString;
  I: Integer;
begin
  Result := nil;

  try
    ExeExec('delete from anomalies; delete from arts_to_map; delete from places;', exExecute, vQuery);

    for I := 0 to ASQLList.Count - 1 do
      vSQLText := vSQLText + ASQLList[I];

    ExeExec(vSQLText, exExecute, vQuery);

    JsonResponse := TJSONObject.Create;
    try
      JsonResponse.AddPair('status', 'success');
      JsonResponse.AddPair('message', 'Данные загружены успешно');
      JsonResponse.AddPair('JSON', '');
      Result := JsonResponse;
    finally
    end;

  except
    JsonResponse := TJSONObject.Create;
    try
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Данные не загружены');
      JsonResponse.AddPair('JSON', '');
      Result := JsonResponse;
    finally
    end;
  end;
end;

end.
