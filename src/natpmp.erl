%%% -*- erlang -*-
%%% This file is part of nat-pmp released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016 Benoît Chesneau <benoitc@refuge.io>


-module(natpmp).


-export([get_external_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).

-define(NAT_PMP_PORT, 5351).
-define(NAT_TRIES, 9).
-define(NAT_INITIAL_MS, 250).

-define(RECOMMENDED_MAPPING_LIFETIME_SECONDS, 3600).

-type natpmp_error() :: unsupported_version
                        | not_authorized
                        | network_failure
                        | out_of_resource
                        | unsupported_opcode
                        | bad_response.

-export_types([natpmp_error/0]).


%% @doc get external ip
-spec get_external_address(Gateway) -> {ok, ExternalIp} | {error, Reason} when
	Gateway :: inet:ip_address() | inet:hostname(),
    ExternalIp :: inet:ip_address() | inet:hostname(),
    Reason :: natpmp_error().
get_external_address(Gateway) ->
	Msg = << 0, 0 >>,
	nat_rpc(Gateway, Msg, 0).


%% @doc add a port mapping with default lifetime
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

%% @doc add a port mapping
-spec add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest, Lifetime) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Lifetime :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer(),
      Reason :: natpmp_error().
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort, Lifetime) ->
    OpCode = case Protocol of
                 udp -> 1;
                 tcp -> 2;
                 _ -> error(unknown_protocol)
             end,

    Msg = << 0,
             OpCode,
             0:16,
             InternalPort:16/unsigned-integer,
             ExternalPort:16/unsigned-integer,
             Lifetime:32/unsigned-integer >>,

    nat_rpc(Gateway, Msg, OpCode).


%% @doc delete a port mapping
-spec delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) ->
    ok | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Reason :: natpmp_error().
delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPort) ->
	case add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort, 0) of
        {ok, _, InternalPort, 0, 0} -> ok;
        {ok, _, _, _, _} -> {error, bad_response};
        Error -> Error
    end.



%% ---------------------
%% - private functions -
%% ---------------------

nat_rpc(Gateway0, Msg, OpCode) ->
	_ = application:start(inets),
    {ok, Gateway} = inet:parse_address(Gateway0),
    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),
    try
        nat_rpc1(Sock, Gateway, Msg, OpCode, 0)
    after
        gen_udp:close(Sock)
    end.


nat_rpc1(_Sock, _Gateway, _Msg, _OpCode, ?NAT_TRIES) ->
    timeout;
nat_rpc1(Sock, Gateway, Msg, OpCode, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = 250 bsl Tries,
    ok = gen_udp:send(Sock, Gateway, ?NAT_PMP_PORT, Msg),
    receive
        {udp, _Sock, Gateway, _Port, Packet} ->
            parse_response(Packet, OpCode);
        {udp, _, _, _, _} ->
            nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
    after Timeout ->
              nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
    end.


parse_response(<< _Version, ResponseCode, Status:16/unsigned-integer,
                  _Since:32/unsigned-integer, A, B, C, D >>, OpCode) ->

     ExpectedCode = OpCode + 128,
     if
         ExpectedCode =:= ResponseCode ->
            case parse_status(Status) of
                ok -> {ok, inet:ntoa({A, B, C, D})};
                Error -> Error
            end;
         true ->
             {error, bad_response}
     end;
parse_response(<< _Version, ResponseCode, Status:16/unsigned-integer,
                 Since:32/unsigned-integer,
                 InternalPort:16/unsigned-integer,
                 ExternalPort:16/unsigned-integer,
                 Lifetime:32/unsigned-integer >>, OpCode) ->

    ExpectedCode = OpCode + 128,
    if
        ExpectedCode =:= ResponseCode ->
            case parse_status(Status) of
                ok -> {ok, Since, InternalPort, ExternalPort, Lifetime};
                Error -> Error
            end;
        true ->
            {error, bad_response}
    end;
parse_response(_, _) ->
    {error, bad_response}.



parse_status(0) -> ok;
parse_status(1) -> {error, unsupported_version};
parse_status(2) -> {error, not_authorized};
parse_status(3) -> {error, network_failure};
parse_status(4) -> {error, out_of_resource};
parse_status(5) -> {error, unsupported_opcode}.
