unit NotificacaoDomain;

interface

uses
  MVCFramework, System.Classes, cHTTP;

type
   [MVCPath('/')]
   TNotificacaoDomain = class(TMVCController)
   private
     function EnviaNotificacao(const ATokens, AMensagem: String): THTTPResponse;
   public
     [MVCHTTPMethod([httpPOST])]
     [MVCPath('/notificacao')]
     procedure NotificaMobile(CTX: TWebContext);

     [MVCHTTPMethod([httpGET])]
     [MVCPath('/token')]
     procedure GetToken(CTX: TWebContext);

     [MVCHTTPMethod([httpPOST])]
     [MVCPath('/token')]
     procedure SetToken(CTX: TWebContext);

     [MVCHTTPMethod([httpDELETE])]
     [MVCPath('/token')]
     procedure DeleteToken(CTX: TWebContext);

     [MVCHTTPMethod([httpOPTIONS])]
     [MVCPath('/notificacao')]
     [MVCPath('/token')]
     procedure Options(CTX: TWebContext);
   end;

   TDado = class
     token: string;
     platform: string;
     device: string;
   end;

   TNotificacao = class
     mensagem: string;
   end;

var
  vgListaToken: TstringList;

implementation

uses
  System.SysUtils, superobject;

{ TNotificacaoDomain }

procedure TNotificacaoDomain.DeleteToken(CTX: TWebContext);
var
  index: Integer;
  vDado: TDado;
begin
  vDado := TDado.Create;

  try
    try

      vDado.token := '';

      vDado.FromJson(CTX.Request.RawWebRequest.Content);

      if not vDado.token.IsEmpty then
      begin
        index := vgListaToken.IndexOf(vDado.token);
        if index >= 0 then
        begin
          vgListaToken.Delete(index);
        end;
      end
      else
      begin
        CTX.Response.StatusCode := 400;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Token não definido"}';
      end;

    except
      on e: Exception do
      begin
        CTX.Response.StatusCode := 500;
      end;
    end;
  finally
    vDado.DisposeOf;
  end;
end;

function TNotificacaoDomain.EnviaNotificacao(const ATokens, AMensagem: String): THTTPResponse;
var
  vRequest: THTTPRequest;
  vResponse: THTTPResponse;
begin
  Result := nil;

  vRequest := THTTPRequest.Create;
  vRequest.SSL := True;
  vRequest.Header.Add('Content-Type:application/json');
  vRequest.Header.Add('Authorization:Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqdGkiOiJmM2RjNDU5MS05MmZlLTRhMzctOTYxNy01ZTJlNzE0NjdhMDMifQ.ybVPFydoukhUc8LuRt3MreJRgelTp6iTUBiYMmqjSn0');
  vRequest.Content.Text := '{ '
                          +' "tokens": ['+ATokens+'],  '
                          +' "profile": "notificationapp",        '
                          +' "notification": {                 '
                          +'     "message": "'+AMensagem+'" }  '
                          +'}';

  try
    Result := THTTPClient.POST('https://api.ionic.io/push/notifications', vRequest);
  finally
    vRequest.DisposeOf;
  end;

end;

procedure TNotificacaoDomain.GetToken(CTX: TWebContext);
var
  i: Integer;
  vRetorno: string;
begin
  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.StatusCode := 200;

  try
    vRetorno := '';

    for i := 0 to vgListaToken.Count -1 do
    begin
      if vRetorno <> '' then
      begin
        vRetorno := vRetorno + ',';
      end;

      vRetorno := vRetorno +'"'+ vgListaToken.Strings[i]+'"';
    end;

    CTX.Response.RawWebResponse.Content := '{ "tokens":['+vRetorno+']}';
  except
    on e: Exception do
    begin
      CTX.Response.StatusCode := 500;
    end;
  end;
end;

procedure TNotificacaoDomain.NotificaMobile(CTX: TWebContext);
var
  vNotificacao: TNotificacao;
  i: Integer;
  vTokens: string;
  vRetorno: THTTPResponse;
begin
  vRetorno := nil;
  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.StatusCode := 200;

  try
    try
      vNotificacao := TNotificacao.Create;
      vNotificacao.mensagem := '';

      vNotificacao.FromJson(CTX.Request.RawWebRequest.Content);

      if vNotificacao.mensagem.IsEmpty then
      begin
        CTX.Response.StatusCode := 400;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Mensagem não definida" }';
        Exit;
      end;

      vTokens := '';

      for i := 0 to vgListaToken.Count -1 do
      begin
        if vTokens <> '' then
        begin
          vTokens := vTokens + ',';
        end;

        vTokens := vTokens +'"'+ vgListaToken.Strings[i] +'"';
      end;

      if vTokens.IsEmpty then
      begin
        CTX.Response.StatusCode := 500;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Não há tokens registrados" }';
        Exit;
      end;

      CTX.Response.RawWebResponse.Content := '{ "tokens":['+vTokens+']}';

      vRetorno := EnviaNotificacao(vTokens, vNotificacao.mensagem);

      if Assigned(vRetorno) then
      begin
        CTX.Response.RawWebResponse.Content := vRetorno.Content.Text;
      end;

    except
      on e: Exception do
      begin
        CTX.Response.StatusCode := 500;
      end;
    end;
  finally
    vNotificacao.DisposeOf;

    if Assigned(vRetorno) then
    begin
      vRetorno.DisposeOf;
    end;
  end;
end;

procedure TNotificacaoDomain.Options(CTX: TWebContext);
begin
  CTX.Response.ContentType := 'application/json; charset=UTF-8';
  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.SetCustomHeader('Access-Control-Allow-Methods','POST, PUT, GET');
  CTX.Response.SetCustomHeader('Access-Control-Max-Age','3600');
  CTX.Response.SetCustomHeader('Access-Control-Allow-Credentials','false');
  CTX.Response.SetCustomHeader('Access-Control-Allow-Headers','Content-Type, X-Session-Id');
end;

procedure TNotificacaoDomain.SetToken(CTX: TWebContext);
var
  vDado: TDado;
begin
  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.StatusCode := 200;
  vDado := TDado.Create;
  try
    try

      vDado.token := '';

      vDado.FromJson(CTX.Request.RawWebRequest.Content);

      if not vDado.token.IsEmpty and (vgListaToken.IndexOf(vDado.token) = -1) then
      begin
        vgListaToken.Add(vDado.token);
      end
      else
      begin
        CTX.Response.StatusCode := 400;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Token já existente"}';

        if vDado.token.IsEmpty then
        begin
          CTX.Response.RawWebResponse.Content := '{ "erro": "Token não definido"}';
        end;
      end;

    except
      on e: Exception do
      begin
        CTX.Response.StatusCode := 500;
      end;
    end;
  finally
    vDado.DisposeOf;
  end;
end;

end.
