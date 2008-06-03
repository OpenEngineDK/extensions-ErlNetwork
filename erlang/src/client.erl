-module(client).

-export([start/0,
	 stop/1,
	 send/2,
	 init/0,
	 loop/1]).

-define(SERVER, "camel02").
-define(PORT, 2345).

send(Node,Msg) ->
    Node ! {msg, Msg}.

stop(Node) ->
    Node ! close.

start() ->
    spawn(?MODULE, init, []).

init() ->
    {ok, Socket} = gen_tcp:connect(?SERVER, ?PORT, [binary, {packet, 4}]),
    io:format("connected to server~n"),
    loop(Socket).

loop(Socket) ->
    receive
	{msg, Msg} ->
	    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
	    io:format("sent package~n"),
	    ?MODULE:loop(Socket);
	close ->
	    io:format("closed client"),
	    gen_tcp:close(Socket);
	{tcp, Socket, Bin} ->
	    io:format("received package: ~p~n", [binary_to_term(Bin)]),
	    ?MODULE:loop(Socket);
	{tcp_closed, _} ->
	    io:format("server closed the socket~n");
	Other ->
	    io:format("ignoring message: ~p~n",[Other]),
	    ?MODULE:loop(Socket)    
    end.
