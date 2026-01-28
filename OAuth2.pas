unit OAuth2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.StrUtils,
  FMX.Controls, System.IOUtils, FMX.Forms, FMX.Graphics, FMX.Dialogs, JSON, Rest.Client, Generics.Collections, Rest.Types, Rest.Exception,
  System.Net.HttpClient;

const
  API_url = 'http://192.168.1.22:2026/';

  function GetDataServer(aCommand: string; JSON: string = ''): string;
  function PostDataServer(aCommand: string; JSON: string = ''): string;

implementation

function SendRequest(URL: string; Headers: TDictionary<string, string>; JSON: string; Method: TRESTRequestMethod): string;
var
  FRest: TRestClient;
  FRequest: TRestrequest;
  FResponse: TRestResponse;
  Key: String;
begin
  Result := '';
  FRest := TRestClient.Create(URL);

  try
    FResponse := TRestResponse.Create(nil);
    try
      FRequest := TRestrequest.Create(nil);
      try
        FRequest.Client := FRest;
        FRequest.Method := Method;
        FRequest.Response := FResponse;

        if Headers <> nil then
          for Key in Headers.Keys do
            if NOT FRequest.Params.ContainsParameter(Key) then
            begin
              with FRequest.Params.AddHeader(Key, Headers.Items[Key]) do
                Options := Options + [poDoNotEncode];
            end;

        if JSON <> '' then
          FRequest.AddBody(JSON, 'application/json');

        try
          FRequest.Execute;

          if FResponse.StatusCode = 200 then
            Result := FResponse.JSONText
          else
            Result := 'error';
        except
        end;
      finally
        FreeAndNil(FRequest);
      end;
    finally
      FreeAndNil(FResponse);
    end;
  finally
    FreeAndNil(FRest);
  end;

end;

function GetDataServer(aCommand: string; JSON: string = ''): string;
var
  Headers: TDictionary<String, String>;
begin
  Headers := TDictionary<String, String>.Create;
  Headers.Add('Accept', 'application/json');

  Result := SendRequest(API_url + aCommand, Headers, JSON, rmGet);
  Headers.Free
end;

function PostDataServer(aCommand: string; JSON: string = ''): string;
var
  Headers: TDictionary<String, String>;
begin
  Headers := TDictionary<String, String>.Create;
  Headers.Add('Accept', 'application/json');

  Result := SendRequest(API_url + aCommand, Headers, JSON, rmPOST);
  Headers.Free
end;

end.
