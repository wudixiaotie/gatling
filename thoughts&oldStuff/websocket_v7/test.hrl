

-record(config, {ssl,               %% SSL parameters
         inet_user,         %% User set inet options
         emulated,          %% #socket_option{} emulated
         inet_ssl,          %% inet options for internal ssl socket
         transport_info,                 %% Callback info
         connection_cb
        }).
-record(socket_options,
    {
      mode   = list, 
      packet = 0,
      packet_size = 0,
      header = 0,
      active = true
     }).
-record(sslsocket, {fd = nil, pid = nil}).
-record(ssl_options, {
      protocol    :: tls | dtls,
      versions    :: ['tlsv1.2' | 'tlsv1.1' | tlsv1 | sslv3] | ['dtlsv1.2' | dtlsv1],
      verify      :: verify_none | verify_peer,
      verify_fun,  %%:: fun(CertVerifyErrors::term()) -> boolean(),
      fail_if_no_peer_cert ::  boolean(),
      verify_client_once   ::  boolean(),
      %% fun(Extensions, State, Verify, AccError) ->  {Extensions, State, AccError}
      validate_extensions_fun, 
      depth                :: integer(),
      certfile             :: binary(),
      cert                 :: term(),
      keyfile              :: binary(),
      key                  :: {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' | 'PrivateKeyInfo', term()},
      password         :: string(),
      cacerts              :: [term()],
      cacertfile           :: binary(),
      dh                   :: term(),
      dhfile               :: binary(),
      user_lookup_fun,  % server option, fun to lookup the user
      psk_identity         :: binary(),
      srp_identity,  % client option {User, Password}
      ciphers,    % 
      %% Local policy for the server if it want's to reuse the session
      %% or not. Defaluts to allways returning true.
      %% fun(SessionId, PeerCert, Compression, CipherSuite) -> boolean()
      reuse_session,  
      %% If false sessions will never be reused, if true they
      %% will be reused if possible.
      reuse_sessions       :: boolean(),
      renegotiate_at,
      secure_renegotiate,
      debug,
      %% undefined if not hibernating, or number of ms of
      %% inactivity after which ssl_connection will go into
      %% hibernation
      hibernate_after      :: boolean(),
      %% This option should only be set to true by inet_tls_dist
      erl_dist = false     :: boolean(),
      next_protocols_advertised = undefined, %% [binary()],
      next_protocol_selector = undefined,  %% fun([binary()]) -> binary())
      log_alert             :: boolean(),
      server_name_indication = undefined
      }).