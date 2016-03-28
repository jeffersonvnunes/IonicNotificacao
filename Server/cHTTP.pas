unit cHTTP;

interface

uses  Classes, IdHTTP, IdSSLOpenSSL, IdHeaderList;

type

  TProxy = class
  private
    FUsuario: String;
    FSenha: String;
    FPorta: Integer;
    FEndereco: String;
    FBasicAuthentication: Boolean;
  protected
    procedure setBasicAuthentication(const Value: Boolean);
    procedure setEndereco(const Value: String);
    procedure setPorta(const Value: Integer);
    procedure setSenha(const Value: String);
    procedure setUsuario(const Value: String);
  public
    property Usuario: String read FUsuario write setUsuario;
    property Senha: String read FSenha write setSenha;
    property Porta: Integer read FPorta write setPorta;
    property Endereco: String read FEndereco write setEndereco;
    property BasicAuthentication: Boolean read FBasicAuthentication write setBasicAuthentication;
  end;

  THTTPBase = class
  private
    FHeader: TIdHeaderList;
    FContent: TStringList;
    FContentType: String;
    FContentEncoding: String;
    FAccept: String;
  protected
    procedure setHeader(const Value: TIdHeaderList); virtual;
    procedure setContent(const Value: TStringList); virtual;
    procedure setContentType(const Value: String); virtual;
    procedure setAccept(const Value: String); virtual;
    procedure setContentEncoding(const Value: String); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    property Header: TIdHeaderList read FHeader write setHeader;
    property Content: TStringList read FContent write setContent;
    property ContentType: String read FContentType write setContentType;
    property ContentEncoding: String read FContentEncoding write setContentEncoding;
    property Accept: String read FAccept write setAccept;
  end;

  THTTPRequest = class(THTTPBase)
  private
    FSSL: Boolean;
    FSSLVersion: TIdSSLVersion;
    FProxy: TProxy;
  protected
    procedure setSSL(const Value: Boolean);
    procedure setSSLVersion(const Value: TIdSSLVersion);
  public
    constructor Create; override;
    destructor Destroy; override;
    property SSL: Boolean read FSSL write setSSL;
    property SSLVersion: TIdSSLVersion read FSSLVersion write setSSLVersion;
    property Proxy: TProxy read FProxy;
  end;

  THTTPResponse = class(THTTPBase)
  private
    FResponseText: String;
    FResponseCode: Integer;
  protected
    procedure setResponseCode(const Value: Integer);
    procedure setResponseText(const Value: String);
  public
    property ResponseText: String read FResponseText write setResponseText;
    property ResponseCode: Integer read FResponseCode write setResponseCode;
  end;

  THTTPClient = class
  private
    class procedure LimparHeadersDefault(pRequisicao: TIdHTTPRequest);
    class procedure InicializarDadosRequisicao(pIdHTTP: TIdHTTP; pRequisicao: THTTPRequest; pIOHandler: TIdSSLIOHandlerSocketOpenSSL = nil);
    class procedure InicializarDadosResposta(pIdHTTP: TIdHTTP; pResposta: THTTPResponse);
  public
    class function GET(pURL:String; pRequisicao: THTTPRequest = nil): THTTPResponse;
    class function PUT(pURL:String; pRequisicao: THTTPRequest = nil): THTTPResponse;
    class function DELETE(pURL:String; pRequisicao: THTTPRequest = nil): THTTPResponse;
    class function POST(pURL:String; pRequisicao: THTTPRequest = nil): THTTPResponse;
    class function isRetornoFaixaSucesso(const ACodigoRetorno: Integer): Boolean;
  end;

implementation

uses SysUtils, IdBaseComponent, IdGlobalProtocols;

{ THTTPBase }

constructor THTTPBase.Create;
begin
  FHeader := TIdHeaderList.Create(QuoteHTTP);
  FContent := TStringList.Create;
end;

destructor THTTPBase.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FContent);
end;

procedure THTTPBase.setHeader(const Value: TIdHeaderList);
begin
  FHeader.Assign(Value);
end;

procedure THTTPBase.setAccept(const Value: String);
begin
  FAccept := Value;
end;

procedure THTTPBase.setContent(const Value: TStringList);
begin
  FContent.Assign(Value);
end;

procedure THTTPBase.setContentEncoding(const Value: String);
begin
  FContentEncoding := Value;
end;

procedure THTTPBase.setContentType(const Value: String);
begin
  FContentType := Value;
end;

{ THTTPRequest }

constructor THTTPRequest.Create;
begin
  inherited;
  FProxy := TProxy.Create;
  FSSLVersion := sslvTLSv1;
end;

destructor THTTPRequest.Destroy;
begin
  FreeAndNil(FProxy);
  inherited;
end;

procedure THTTPRequest.setSSL(const Value: Boolean);
begin
  FSSL := Value;
end;

procedure THTTPRequest.setSSLVersion(const Value: TIdSSLVersion);
begin
  FSSLVersion := Value;
end;

{ THTTPClient }

class function THTTPClient.DELETE(pURL: String; pRequisicao: THTTPRequest): THTTPResponse;
var
  vIdHttp: TIdHTTP;
  vSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin           
  Result        := THTTPResponse.Create;
  vSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  vIdHttp       := TIdHTTP.Create(nil);
  try
    THTTPClient.InicializarDadosRequisicao(vIdHttp, pRequisicao, vSSLIOHandler);
    try
      {$IF CompilerVErsion > 26}
        Result.Content.Text := vIdHttp.Delete(pURL);
      {$ELSE}
        vIdHttp.Delete(pURL);
      {$IFEND}
      THTTPClient.InicializarDadosResposta(vIdHttp, Result);

    except
      on e: EIdHTTPProtocolException do
      begin
        THTTPClient.InicializarDadosResposta(vIdHttp, Result);
        Result.Content.Text := e.ErrorMessage;
      end;

      on e: Exception do
      begin
        Result.ResponseCode := 500;
        Result.ResponseText := 'Não foi possível atender a requisição: '+ e.Message;
      end;
    end;
  finally
    vIdHttp.Free;
    vSSLIOHandler.Free;
  end;
end;

class function THTTPClient.GET(pURL: String; pRequisicao: THTTPRequest): THTTPResponse;
var
  vIdHttp: TIdHTTP;
  vSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result        := THTTPResponse.Create;
  vSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  vIdHttp       := TIdHTTP.Create(nil);
  try
    THTTPClient.InicializarDadosRequisicao(vIdHttp, pRequisicao, vSSLIOHandler);

    try
      Result.Content.Text := vIdHttp.Get(pURL);
      THTTPClient.InicializarDadosResposta(vIdHttp, Result);
    except
      on e: EIdHTTPProtocolException do
      begin
        THTTPClient.InicializarDadosResposta(vIdHttp, Result);
        Result.Content.Text := e.ErrorMessage;
      end;

      on e: Exception do
      begin
        Result.ResponseCode := 500;
        Result.ResponseText := 'Não foi possível atender a requisição: '+ e.Message;
      end;
    end;
  finally
    vIdHttp.Free;
    vSSLIOHandler.Free;
  end;
end;

class procedure THTTPClient.InicializarDadosResposta(pIdHTTP: TIdHTTP; pResposta: THTTPResponse);
begin
  pResposta.Header.Text := pIdHttp.Response.RawHeaders.Text;
  pResposta.ResponseText := pIdHttp.Response.ResponseText;
  pResposta.ResponseCode := pIdHttp.Response.ResponseCode;
end;

class function THTTPClient.isRetornoFaixaSucesso(const ACodigoRetorno: Integer): Boolean;
begin
  Result := (ACodigoRetorno >= 200) and (ACodigoRetorno < 300);
end;

class procedure THTTPClient.InicializarDadosRequisicao(pIdHTTP: TIdHTTP; pRequisicao: THTTPRequest;
  pIOHandler: TIdSSLIOHandlerSocketOpenSSL);
begin
  if pRequisicao <> nil then
  begin
    if pRequisicao.Header.Count > 0 then
    begin
      pIdHttp.Request.CustomHeaders.Text := pRequisicao.Header.Text;
      THTTPClient.LimparHeadersDefault(pIdHttp.Request);
    end;

    if pRequisicao.SSL and (pIOHandler <> nil) then
    begin
      pIOHandler.SSLOptions.Method := pRequisicao.SSLVersion;
      pIdHttp.IOHandler := pIOHandler;
    end;

    pIdHTTP.Request.ContentType := pRequisicao.ContentType;
    pIdHTTP.Request.Accept := pRequisicao.Accept;
    pIdHTTP.Request.ContentEncoding  := pRequisicao.ContentEncoding;

    pIdHttp.ProxyParams.ProxyServer := pRequisicao.Proxy.Endereco;
    pIdHttp.ProxyParams.ProxyPort := pRequisicao.Proxy.Porta;
    pIdHttp.ProxyParams.ProxyUsername := pRequisicao.Proxy.Usuario;
    pIdHttp.ProxyParams.ProxyPassword := pRequisicao.Proxy.Senha;
  end;
end;

class procedure THTTPClient.LimparHeadersDefault(pRequisicao: TIdHTTPRequest);
begin
  pRequisicao.URL := '';
  pRequisicao.Accept := '';
  pRequisicao.AcceptCharSet := '';
  pRequisicao.AcceptEncoding := '';
  pRequisicao.AcceptLanguage := '';
  pRequisicao.From := '';
  pRequisicao.Password := '';
  pRequisicao.Referer := '';
  pRequisicao.UserAgent := '';
  pRequisicao.UserName := '';
  pRequisicao.Host := '';
  pRequisicao.ProxyConnection := '';
  pRequisicao.CacheControl := '';
  pRequisicao.CharSet := '';
  pRequisicao.Connection := '';
  pRequisicao.ContentDisposition := '';
  pRequisicao.ContentEncoding := '';
  pRequisicao.ContentLanguage := '';
  pRequisicao.ContentType := '';
  pRequisicao.ContentVersion := '';
  pRequisicao.Date := 0;
  pRequisicao.Expires := 0;
  pRequisicao.LastModified := 0;
end;

class function THTTPClient.POST(pURL: String; pRequisicao: THTTPRequest): THTTPResponse;
var
  vIdHttp: TIdHTTP;
  vMemoStream: TMemoryStream;
  vSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  vMemoStream   := TMemoryStream.Create;
  vSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  vIdHttp       := TIdHTTP.Create(nil);
  Result        := THTTPResponse.Create;
  try
    THTTPClient.InicializarDadosRequisicao(vIdHttp, pRequisicao, vSSLIOHandler);

    if (pRequisicao <> nil) and (pRequisicao.Content.Count > 0) then
      pRequisicao.FContent.SaveToStream(vMemoStream);

    try
      Result.Content.Text := vIdHttp.Post(pURL, vMemoStream);
      THTTPClient.InicializarDadosResposta(vIdHttp, Result);

    except
      on e: EIdHTTPProtocolException do
      begin
        THTTPClient.InicializarDadosResposta(vIdHttp, Result);
        Result.Content.Text := e.ErrorMessage;
      end;

      on e: Exception do
      begin
        Result.ResponseCode := 500;
        Result.ResponseText := 'Não foi possível atender a requisição: '+ e.Message;
      end;
    end;
  finally
    vIdHttp.Free;
    vSSLIOHandler.Free;
    vMemoStream.Free;
  end;
end;

class function THTTPClient.PUT(pURL: String; pRequisicao: THTTPRequest): THTTPResponse;
var
  vIdHttp: TIdHTTP;
  vMemoStream: TMemoryStream;
  vSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result        := THTTPResponse.Create;
  vMemoStream   := TMemoryStream.Create;
  vSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  vIdHttp       := TIdHTTP.Create(nil);
  try

    THTTPClient.InicializarDadosRequisicao(vIdHttp, pRequisicao, vSSLIOHandler);

    if (pRequisicao <> nil) and (pRequisicao.Content.Count > 0) then
      pRequisicao.FContent.SaveToStream(vMemoStream);

    try
      Result.Content.Text := vIdHttp.Put(pURL, vMemoStream);
      THTTPClient.InicializarDadosResposta(vIdHttp, Result);

    except
      on e: EIdHTTPProtocolException do
      begin
        THTTPClient.InicializarDadosResposta(vIdHttp, Result);
        Result.Content.Text := e.ErrorMessage;
      end;

      on e: Exception do
      begin
        Result.ResponseCode := 500;
        Result.ResponseText := 'Não foi possível atender a requisição: '+ e.Message;
      end;
    end;
  finally
    vIdHttp.Free;
    vSSLIOHandler.Free;
    vMemoStream.Free;
  end;
end;

{ THTTPResponse }

procedure THTTPResponse.setResponseCode(const Value: Integer);
begin
  FResponseCode := Value;
end;

procedure THTTPResponse.setResponseText(const Value: String);
begin
  FResponseText := Value;
end;

{ TProxy }

procedure TProxy.setBasicAuthentication(const Value: Boolean);
begin
  FBasicAuthentication := Value;
end;

procedure TProxy.setEndereco(const Value: String);
begin
  FEndereco := Value;
end;

procedure TProxy.setPorta(const Value: Integer);
begin
  FPorta := Value;
end;

procedure TProxy.setSenha(const Value: String);
begin
  FSenha := Value;
end;

procedure TProxy.setUsuario(const Value: String);
begin
  FUsuario := Value;
end;

end.
