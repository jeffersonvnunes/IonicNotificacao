unit NotificacaoDomain;

interface

uses
  MVCFramework;

type
   [MVCPath('/')]
   TNotificacaoDomain = class(TMVCController)
   public

     [MVCHTTPMethod([httpPOST])]
     [MVCPath('/notificacao')]
     procedure NotificaMobile(CTX: TWebContext);
   end;

implementation

uses
  System.SysUtils;

{ TNotificacaoDomain }

procedure TNotificacaoDomain.NotificaMobile(CTX: TWebContext);
begin
  CTX.Response.StatusCode := 200;

  try


  except
    on e: Exception do
    begin
      CTX.Response.StatusCode := 500;
    end;
  end;

end;

end.
