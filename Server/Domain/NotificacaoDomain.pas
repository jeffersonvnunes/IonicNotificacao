unit NotificacaoDomain;

interface

uses
  MVCFramework, System.Classes, cHTTP, System.Generics.Collections ;

type
   [MVCPath('/')]
   TNotificacaoDomain = class(TMVCController)
   private
     function EnviaNotificacao(const AToken, AMensagem: String): THTTPResponse;
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
  vgListaToken: TDictionary<string, TDado>;

implementation

uses
  System.SysUtils, superobject;

{ TNotificacaoDomain }

procedure TNotificacaoDomain.DeleteToken(CTX: TWebContext);
var
  key: String;
  vDado, vDadoLista: TDado;
begin
  vDado := TDado.Create;

  try
    try

      vDado.device := '';

      vDado.FromJson(CTX.Request.RawWebRequest.Content);

      if not vDado.device.IsEmpty then
      begin

        if vgListaToken.ContainsKey(vDado.device) then
        begin
          vDadoLista := vgListaToken.Items[vDado.device];
          vgListaToken.Remove(vDado.device);
          vDadoLista.DisposeOf;
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

function TNotificacaoDomain.EnviaNotificacao(const AToken, AMensagem: String): THTTPResponse;
var
  vRequest: THTTPRequest;
begin
  Result := nil;

  vRequest := THTTPRequest.Create;
  vRequest.SSL := True;
  vRequest.Header.Add('Content-Type:application/json');
  vRequest.Header.Add('Authorization:key=AIzaSyAa1mUPLQmCHtlEQjY5DJr1jT5X8NXxfg8');
  vRequest.Content.Text := '{ '
                          +' "data": { '
                          +'  "message": "'+AMensagem+'"'
                          +' },'
                          +' "to" : "'+AToken+'"'
                          +'}';

  try
    Result := THTTPClient.POST('https://gcm-http.googleapis.com/gcm/send', vRequest);
  finally
    vRequest.DisposeOf;
  end;

end;

procedure TNotificacaoDomain.GetToken(CTX: TWebContext);
var
  key: string;
  vRetorno: string;
  vDado: TDado;
begin
  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.StatusCode := 200;

  try
    vRetorno := '';

    for key in vgListaToken.Keys do
    begin
      vDado := vgListaToken.Items[key];

      if vRetorno <> '' then
      begin
        vRetorno := vRetorno + ',';
      end;

      vRetorno := vRetorno + vDado.toJson.AsJSon;
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
  key: string;
  vTokens: string;
  vRetorno: THTTPResponse;
  vRetornos: TStringList;
  vDado: TDado;
begin
  vRetorno := nil;
  vRetornos := nil;
  vNotificacao := nil;

  CTX.Response.SetCustomHeader('Access-Control-Allow-Origin','*');
  CTX.Response.StatusCode := 200;

  vRetornos := TStringList.Create;  
  vNotificacao := TNotificacao.Create;

  try
    try

      vNotificacao.mensagem := '';

      vNotificacao.FromJson(CTX.Request.RawWebRequest.Content);

      if vNotificacao.mensagem.IsEmpty then
      begin
        CTX.Response.StatusCode := 400;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Mensagem não definida" }';
        Exit;
      end;

      vTokens := '';

      for key in vgListaToken.Keys do
      begin
        vDado := vgListaToken.Items[key];

        if (vDado.platform = 'Android') then
        begin
          vRetorno := EnviaNotificacao(vDado.token, vNotificacao.mensagem);           

          if vTokens <> '' then
          begin
            vTokens := vTokens + ',';
            vRetornos.Add(',');
          end;

          vRetornos.Add(vRetorno.Content.Text);

          vRetorno.DisposeOf;

          vTokens := vTokens +'"'+ vDado.token +'"';
        end;
      end;

      if vTokens.IsEmpty then
      begin
        CTX.Response.StatusCode := 500;
        CTX.Response.RawWebResponse.Content := '{ "erro": "Não há tokens registrados" }';
        Exit;
      end;

      CTX.Response.RawWebResponse.Content :='"retorno":['+ vRetornos.Text+']';

    except
      on e: Exception do
      begin
        CTX.Response.StatusCode := 500;
      end;
    end;
  finally
    vNotificacao.DisposeOf;
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

    vDado.device := '';

    vDado.FromJson(CTX.Request.RawWebRequest.Content);

    if not (vDado.device.IsEmpty or vgListaToken.ContainsKey(vDado.device)) then
    begin
      vgListaToken.Add(vDado.device, vDado);
    end
    else
    begin
      CTX.Response.StatusCode := 400;
      CTX.Response.RawWebResponse.Content := '{ "erro": "Device já existente"}';

      if vDado.device.IsEmpty then
      begin
        CTX.Response.RawWebResponse.Content := '{ "erro": "Device não definido"}';
      end;
    end;

  except
    on e: Exception do
    begin
      CTX.Response.StatusCode := 500;
    end;
  end;
end;

end.
