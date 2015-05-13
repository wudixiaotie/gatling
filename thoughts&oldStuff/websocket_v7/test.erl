-module (test).

-compile (export_all).

-define(DEFAULT_RENEGOTIATE_AT, 268435456). %% math:pow(2, 28) 
-define(NO_PROTOCOL, <<>>).
-include ("test.hrl").


listen(Port, Options0) ->
    {ok, Config} = handle_options(Options0, server),
    ConnectionCb = connection_cb(Options0),
    #config{transport_info = {Transport, _, _, _}, inet_user = Options, connection_cb = ConnectionCb} = Config,
    case Transport:listen(Port, Options) of
        {ok, ListenSocket} ->
        {ok, #sslsocket{pid = {ListenSocket, Config}}};
        Err = {error, _} ->
        Err
    end.


connection_cb(tls) ->
    tls_connection;
connection_cb(dtls) ->
    dtls_connection;
connection_cb(Opts) ->
   connection_cb(proplists:get_value(protocol, Opts, tls)).



%% The option cacerts overrides cacertsfile
ca_cert_default(_,_, [_|_]) ->
    undefined;
ca_cert_default(verify_none, _, _) ->
    undefined;
ca_cert_default(verify_peer, {Fun,_}, _) when is_function(Fun) ->
    undefined;
%% Server that wants to verify_peer and has no verify_fun must have
%% some trusted certs.
ca_cert_default(verify_peer, undefined, _) ->
    "".


emulated_options(Opts) ->
    emulated_options(Opts, internal_inet_values(), #socket_options{}).

emulated_options([{mode,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(mode,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{mode=Opt});
emulated_options([{header,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(header,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{header=Opt});
emulated_options([{active,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(active,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{active=Opt});
emulated_options([{packet,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(packet,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{packet=Opt});
emulated_options([{packet_size,Opt}|Opts], Inet, Emulated) ->
    validate_inet_option(packet_size,Opt),
    emulated_options(Opts, Inet, Emulated#socket_options{packet_size=Opt});
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.







cipher_suites(Version, []) ->
    ssl_cipher:suites(Version);
cipher_suites(Version, [{_,_,_,_}| _] = Ciphers0) -> %% Backwards compatibility
    Ciphers = [{KeyExchange, Cipher, Hash} || {KeyExchange, Cipher, Hash, _} <- Ciphers0],
    cipher_suites(Version, Ciphers);
cipher_suites(Version, [{_,_,_}| _] = Ciphers0) ->
    Ciphers = [ssl_cipher:suite(C) || C <- Ciphers0],
    cipher_suites(Version, Ciphers);

cipher_suites(Version, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    Supported0 = ssl_cipher:suites(Version)
    ++ ssl_cipher:anonymous_suites()
    ++ ssl_cipher:psk_suites(Version)
    ++ ssl_cipher:srp_suites(),
    Supported = ssl_cipher:filter_suites(Supported0),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, Supported)] of
    [] ->
        Supported;
    Ciphers ->
        Ciphers
    end;
cipher_suites(Version, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- Ciphers0],
    cipher_suites(Version, Ciphers);
cipher_suites(Version, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = [ssl_cipher:openssl_suite(C) || C <- string:tokens(Ciphers0, ":")],
    cipher_suites(Version, Ciphers).









internal_inet_values() ->
    [{packet_size,0},{packet, 0},{header, 0},{active, false},{mode,binary}].













make_next_protocol_selector(undefined) ->
    undefined;
make_next_protocol_selector({client, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
        case detect(fun(PreferredProtocol) ->
                lists:member(PreferredProtocol, AdvertisedProtocols)
            end, AllProtocols) of
            undefined ->
        DefaultProtocol;
            PreferredProtocol ->
        PreferredProtocol
        end
    end;

make_next_protocol_selector({server, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
        case detect(fun(PreferredProtocol) ->
                lists:member(PreferredProtocol, AllProtocols)
            end,
            AdvertisedProtocols) of
        undefined ->
            DefaultProtocol;
            PreferredProtocol ->
            PreferredProtocol
        end
    end.






detect(_Pred, []) ->
    undefined;
detect(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            H;
        _ ->
            detect(Pred, T)
    end.




handle_options(Opts0, _Role) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
                 {list, [{mode, list}]}], Opts0),
    ReuseSessionFun = fun(_, _, _, _) -> true end,

    DefaultVerifyNoneFun =
    {fun(_,{bad_cert, _}, UserState) ->
         {valid, UserState};
        (_,{extension, _}, UserState) ->
         {unknown, UserState};
        (_, valid, UserState) ->
         {valid, UserState};
        (_, valid_peer, UserState) ->
         {valid, UserState}
     end, []},

    VerifyNoneFun = handle_option(verify_fun, Opts, DefaultVerifyNoneFun),

    UserFailIfNoPeerCert = handle_option(fail_if_no_peer_cert, Opts, false),
    UserVerifyFun = handle_option(verify_fun, Opts, undefined),
    CaCerts = handle_option(cacerts, Opts, undefined),

    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun} =
    %% Handle 0, 1, 2 for backwards compatibility
    case proplists:get_value(verify, Opts, verify_none) of
        0 ->
        {verify_none, false,
         ca_cert_default(verify_none, VerifyNoneFun, CaCerts), VerifyNoneFun};
        1  ->
        {verify_peer, false,
         ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
        2 ->
        {verify_peer, true,
         ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
        verify_none ->
        {verify_none, false,
         ca_cert_default(verify_none, VerifyNoneFun, CaCerts), VerifyNoneFun};
        verify_peer ->
        {verify_peer, UserFailIfNoPeerCert,
         ca_cert_default(verify_peer, UserVerifyFun, CaCerts), UserVerifyFun};
        Value ->
        throw({error, {options, {verify, Value}}})
    end,

    CertFile = handle_option(certfile, Opts, <<>>),

    Versions = case handle_option(versions, Opts, []) of
           [] ->
               tls_record:supported_protocol_versions();
           Vsns  ->
               [tls_record:protocol_version(Vsn) || Vsn <- Vsns]
           end,

    SSLOptions = #ssl_options{
      versions   = Versions,
      verify     = validate_option(verify, Verify),
      verify_fun = VerifyFun,
      fail_if_no_peer_cert = FailIfNoPeerCert,
      verify_client_once =  handle_option(verify_client_once, Opts, false),
      depth      = handle_option(depth,  Opts, 1),
      cert       = handle_option(cert, Opts, undefined),
      certfile   = CertFile,
      key        = handle_option(key, Opts, undefined),
      keyfile    = handle_option(keyfile,  Opts, CertFile),
      password   = handle_option(password, Opts, ""),
      cacerts    = CaCerts,
      cacertfile = handle_option(cacertfile, Opts, CaCertDefault),
      dh         = handle_option(dh, Opts, undefined),
      dhfile     = handle_option(dhfile, Opts, undefined),
      user_lookup_fun = handle_option(user_lookup_fun, Opts, undefined),
      psk_identity = handle_option(psk_identity, Opts, undefined),
      srp_identity = handle_option(srp_identity, Opts, undefined),
      ciphers    = handle_option(ciphers, Opts, []),
      %% Server side option
      reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
      reuse_sessions = handle_option(reuse_sessions, Opts, true),
      secure_renegotiate = handle_option(secure_renegotiate, Opts, false),
      renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
      hibernate_after = handle_option(hibernate_after, Opts, undefined),
      erl_dist = handle_option(erl_dist, Opts, false),
      next_protocols_advertised =
            handle_option(next_protocols_advertised, Opts, undefined),
      next_protocol_selector =
            make_next_protocol_selector(
              handle_option(client_preferred_next_protocols, Opts, undefined)),
      log_alert = handle_option(log_alert, Opts, true)
     },

    CbInfo  = proplists:get_value(cb_info, Opts, {gen_tcp, tcp, tcp_closed, tcp_error}),
    SslOptions = [protocol, versions, verify, verify_fun,
          fail_if_no_peer_cert, verify_client_once,
          depth, cert, certfile, key, keyfile,
          password, cacerts, cacertfile, dh, dhfile,
          user_lookup_fun, psk_identity, srp_identity, ciphers,
          reuse_session, reuse_sessions, ssl_imp,
          cb_info, renegotiate_at, secure_renegotiate, hibernate_after,
          erl_dist, next_protocols_advertised,
          client_preferred_next_protocols, log_alert],

    SockOpts = lists:foldl(fun(Key, PropList) ->
                   proplists:delete(Key, PropList)
               end, Opts, SslOptions),

    {SSLsock, Emulated} = emulated_options(SockOpts),
    ConnetionCb = connection_cb(Opts),

    {ok, #config{ssl = SSLOptions, emulated = Emulated, inet_ssl = SSLsock,
         inet_user = SockOpts, transport_info = CbInfo, connection_cb = ConnetionCb
        }}.

handle_option(OptionName, Opts, Default) ->
    validate_option(OptionName,
            proplists:get_value(OptionName, Opts, Default)).


validate_option(versions, Versions)  ->
    validate_versions(Versions, Versions);
validate_option(verify, Value)
  when Value == verify_none; Value == verify_peer ->
    Value;
validate_option(verify_fun, undefined)  ->
    undefined;
%% Backwards compatibility
validate_option(verify_fun, Fun) when is_function(Fun) ->
    {fun(_,{bad_cert, _} = Reason, OldFun) ->
         case OldFun([Reason]) of
         true ->
             {valid, OldFun};
         false ->
             {fail, Reason}
         end;
    (_,{extension, _}, UserState) ->
         {unknown, UserState};
    (_, valid, UserState) ->
         {valid, UserState};
    (_, valid_peer, UserState) ->
         {valid, UserState}
     end, Fun};
validate_option(verify_fun, {Fun, _} = Value) when is_function(Fun) ->
   Value;
validate_option(fail_if_no_peer_cert, Value)
  when Value == true; Value == false ->
    Value;
validate_option(verify_client_once, Value)
  when Value == true; Value == false ->
    Value;
validate_option(depth, Value) when is_integer(Value),
                                   Value >= 0, Value =< 255->
    Value;
validate_option(cert, Value) when Value == undefined;
                                 is_binary(Value) ->
    Value;
validate_option(certfile, undefined = Value) ->
    Value;
validate_option(certfile, Value) when is_binary(Value) ->
    Value;
validate_option(certfile, Value) when is_list(Value) ->
    list_to_binary(Value);

validate_option(key, undefined) ->
    undefined;
validate_option(key, {KeyType, Value}) when is_binary(Value),
                        KeyType == rsa; %% Backwards compatibility
                        KeyType == dsa; %% Backwards compatibility
                        KeyType == 'RSAPrivateKey';
                        KeyType == 'DSAPrivateKey';
                        KeyType == 'PrivateKeyInfo' ->
    {KeyType, Value};

validate_option(keyfile, undefined) ->
   <<>>;
validate_option(keyfile, Value) when is_binary(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value), Value =/= "" ->
    list_to_binary(Value);
validate_option(password, Value) when is_list(Value) ->
    Value;

validate_option(cacerts, Value) when Value == undefined;
                     is_list(Value) ->
    Value;
%% certfile must be present in some cases otherwhise it can be set
%% to the empty string.
validate_option(cacertfile, undefined) ->
   <<>>;
validate_option(cacertfile, Value) when is_binary(Value) ->
    Value;
validate_option(cacertfile, Value) when is_list(Value), Value =/= ""->
    list_to_binary(Value);
validate_option(dh, Value) when Value == undefined;
                is_binary(Value) ->
    Value;
validate_option(dhfile, undefined = Value)  ->
    Value;
validate_option(dhfile, Value) when is_binary(Value) ->
    Value;
validate_option(dhfile, Value) when is_list(Value), Value =/= "" ->
    list_to_binary(Value);
validate_option(psk_identity, undefined) ->
    undefined;
validate_option(psk_identity, Identity)
  when is_list(Identity), Identity =/= "", length(Identity) =< 65535 ->
    list_to_binary(Identity);
validate_option(user_lookup_fun, undefined) ->
    undefined;
validate_option(user_lookup_fun, {Fun, _} = Value) when is_function(Fun, 3) ->
   Value;
validate_option(srp_identity, undefined) ->
    undefined;
validate_option(srp_identity, {Username, Password})
  when is_list(Username), is_list(Password), Username =/= "", length(Username) =< 255 ->
    {list_to_binary(Username), list_to_binary(Password)};

validate_option(ciphers, Value)  when is_list(Value) ->
    Version = tls_record:highest_protocol_version([]),
    try cipher_suites(Version, Value)
    catch
    exit:_ ->
        throw({error, {options, {ciphers, Value}}});
    error:_->
        throw({error, {options, {ciphers, Value}}})
    end;
validate_option(reuse_session, Value) when is_function(Value) ->
    Value;
validate_option(reuse_sessions, Value) when Value == true;
                        Value == false ->
    Value;

validate_option(secure_renegotiate, Value) when Value == true;
                        Value == false ->
    Value;
validate_option(renegotiate_at, Value) when is_integer(Value) ->
    erlang:min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(hibernate_after, undefined) ->
    undefined;
validate_option(hibernate_after, Value) when is_integer(Value), Value >= 0 ->
    Value;
validate_option(erl_dist,Value) when Value == true;
                     Value == false ->
    Value;
validate_option(client_preferred_next_protocols = Opt, {Precedence, PreferredProtocols} = Value)
  when is_list(PreferredProtocols) ->
    case tls_record:highest_protocol_version([]) of
    {3,0} ->
        throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
    _ ->
        validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
        validate_npn_ordering(Precedence),
        {Precedence, PreferredProtocols, ?NO_PROTOCOL}
    end;
validate_option(client_preferred_next_protocols = Opt, {Precedence, PreferredProtocols, Default} = Value)
      when is_list(PreferredProtocols), is_binary(Default),
           byte_size(Default) > 0, byte_size(Default) < 256 ->
    case tls_record:highest_protocol_version([]) of
    {3,0} ->
        throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
    _ ->
        validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
        validate_npn_ordering(Precedence),
        Value
    end;

validate_option(client_preferred_next_protocols, undefined) ->
    undefined;
validate_option(log_alert, Value) when Value == true;
                       Value == false ->
    Value;
validate_option(next_protocols_advertised = Opt, Value) when is_list(Value) ->
    case tls_record:highest_protocol_version([]) of
    {3,0} ->
        throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
    _ ->
        validate_binary_list(next_protocols_advertised, Value),
        Value
    end;

validate_option(next_protocols_advertised, undefined) ->
    undefined;
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_npn_ordering(client) ->
    ok;
validate_npn_ordering(server) ->
    ok;
validate_npn_ordering(Value) ->
    throw({error, {options, {client_preferred_next_protocols, {invalid_precedence, Value}}}}).

validate_binary_list(Opt, List) ->
    lists:foreach(
        fun(Bin) when is_binary(Bin),
                      byte_size(Bin) > 0,
                      byte_size(Bin) < 256 ->
            ok;
           (Bin) ->
            throw({error, {options, {Opt, {invalid_protocol, Bin}}}})
        end, List).

validate_versions([], Versions) ->
    Versions;
validate_versions([Version | Rest], Versions) when Version == 'tlsv1.2';
                                                   Version == 'tlsv1.1';
                                                   Version == tlsv1;
                                                   Version == sslv3 ->
    validate_versions(Rest, Versions);
validate_versions([Ver| _], Versions) ->
    throw({error, {options, {Ver, {versions, Versions}}}}).

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {options, {mode,Value}}});
validate_inet_option(packet, Value)
  when not (is_atom(Value) orelse is_integer(Value)) ->
    throw({error, {options, {packet,Value}}});
validate_inet_option(packet_size, Value)
  when not is_integer(Value) ->
    throw({error, {options, {packet_size,Value}}});
validate_inet_option(header, Value)
  when not is_integer(Value) ->
    throw({error, {options, {header,Value}}});
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {options, {active,Value}}});
validate_inet_option(_, _) ->
    ok.