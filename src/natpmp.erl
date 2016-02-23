%%% -*- erlang -*-
%%% This file is part of nat-pmp released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016 Benoît Chesneau <benoitc@refuge.io>


-module(natpmp).


-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([discover/0]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).

-define(NAT_PMP_PORT, 5351).
-define(NAT_TRIES, 5).
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

%% @doc get internal address used for this gateway
get_internal_address(Gateway) ->
    [{_, {MyIp, _}}|_] = route(Gateway),
    inet_parse:ntoa(MyIp).

discover_with_addr(Parent, Ref, Addr) ->
    case natpmp:get_external_address(Addr) of
        {ok, _Ip} ->
            Parent ! {nat, Ref, self(), Addr};
        _Else ->
            ok
    end.

%% @doc discover a Nat gateway
discover() ->
     Net_10 = inet_cidr:parse("10.0.0.0/8"),
     Net_172_16 = inet_cidr:parse("172.16.0.0/12"),
     Net_192_168 = inet_cidr:parse("192.168.0.0/16"),
     Networks = [Net_10, Net_172_16, Net_192_168],

     IPs = lists:foldl(fun({_, {Addr, Mask}}, Acc) ->
                               case is_network(Networks, Addr) of
                                   true ->
                                       case inet_cidr:is_ipv4(Addr) of
                                           true ->
                                               Ip0 = mask(Addr, Mask),
                                               Ip = setelement(4, Ip0,
                                                               element(4, Ip0) bor 1),
                                               [Ip | Acc];
                                           false ->
                                               Acc
                                       end;
                                   false ->
                                       Acc
                               end
                 end, [], routes()),

     Ref = make_ref(),
     Self = self(),

     Workers = lists:foldl(fun(Ip, Acc) ->
                                   Pid = spawn_link(fun() ->
                                                            discover_with_addr(Self, Ref, Ip)
                                                    end),
                                   erlang:monitor(process, Pid),
                                   [Pid | Acc]
                           end, [], IPs),

     discover_wait(Workers, Ref).

discover_wait([], _Ref) ->
    no_nat;
discover_wait(Workers, Ref) ->
    receive
        {nat, Ref, WorkerPid, GatewayIp} ->
            lists:foreach(fun(Pid) ->
                                  catch unlink(Pid),
                                  catch exit(Pid, shutdown),
                                  receive
                                      {'DOWN', _, _, Pid, _} -> ok
                                  end
                          end, Workers -- [WorkerPid]),
            {ok, GatewayIp};
        {'DOWN', _MRef, _Type, WorkerPid, _Info} ->
            discover_wait(Workers -- [WorkerPid], Ref)

    end.


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
%%
parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) ->
    {ok, Addr} = inet:parse_address(S),
    Addr.


nat_rpc(Gateway0, Msg, OpCode) ->
	_ = application:start(inets),
    Gateway = parse_address(Gateway0),
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
    case gen_udp:send(Sock, Gateway, ?NAT_PMP_PORT, Msg) of
        ok ->
            receive
                {udp, _Sock, Gateway, _Port, Packet} ->
                    parse_response(Packet, OpCode);
                {udp, _, _, _, _} ->
                    nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
            after Timeout ->
                      nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
            end;
        _Error ->
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


%% check if an ip is a member of the test networks
is_network([Net | Rest], Addr) ->
    case inet_cidr:contains(Net, Addr) of
        true -> true;
        false -> is_network(Rest, Addr)
    end;
is_network([], _Addr) ->
    false.


%% convenient function to recover the list of routes
%% https://gist.github.com/archaelus/1247174
%% from @archaleus (Geoff Cant)
route(Targ) ->
    route(Targ, routes()).

route(Targ, Routes) ->
    sort_routes(routes_for(Targ, Routes)).

routes_for(Targ, Routes) ->
    [ RT || RT = {_IF, {Addr, Mask}} <- Routes,
            tuple_size(Targ) =:= tuple_size(Addr),
            match_route(Targ, Addr, Mask)
    ].

sort_routes(Routes) ->
    lists:sort(fun ({_, {_AddrA, MaskA}}, {_, {_AddrB, MaskB}}) ->
                       MaskA > MaskB
               end,
               Routes).

match_route(Targ, Addr, Mask)
  when tuple_size(Targ) =:= tuple_size(Addr),
       tuple_size(Targ) =:= tuple_size(Mask) ->
    lists:all(fun (A) -> A end,
              [element(I, Targ) band element(I, Mask)
               =:= element(I, Addr) band element(I, Mask)
               || I <- lists:seq(1, tuple_size(Targ)) ]).


routes() ->
    {ok, IFData} = inet:getifaddrs(),
    lists:append([ routes(IF, IFOpts) || {IF, IFOpts} <- IFData ]).

routes(IF, Opts) ->
    {_,Routes} = lists:foldl(fun parse_opts/2, {undefined, []}, Opts),
    [{IF, Route}  || Route <- Routes].

parse_opts({addr, Addr}, {undefined, Routes}) ->
    {{addr, Addr}, Routes};
parse_opts({netmask, Mask}, {{addr, Addr}, Routes})
  when tuple_size(Mask) =:= tuple_size(Addr) ->
    {undefined, [{Addr, Mask} | Routes]};
parse_opts(_, Acc) -> Acc.

%% apply mask to the ip
mask({A, B, C, D}, {E, F, G, H}) ->
    {A band E, B band F, C band G, D band H}.
