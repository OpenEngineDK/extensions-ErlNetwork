-module(server).

%% public exports
-export([start/1,
	 connect/2,
	 disconnect/2,
	 broadcast/2]).

%% private exports
-export([handle_connections/2,
	 handle_requests/2,
	 init/1,
	 loop/1]).

-define(PORT, 2345).

%% command line start (-s or -run)
start([Port]) when is_atom(Port) ->
    start(list_to_integer(atom_to_list(Port)));

start([Port]) when is_list(Port) ->
    start(list_to_integer(Port));

start(Port) when is_integer(Port) ->
    spawn(?MODULE, init, [Port]).

connect(Server, NodePid) ->
    Server ! {add, NodePid}.

disconnect(Server, NodePid) ->
    Server ! {'EXIT', NodePid, disconnect}.

broadcast(Server, Msg) ->
    Server ! {bcast, Msg}.
    
init(Port) ->
    io:format("Starting the server on port ~p~n", [Port]),
    process_flag(trap_exit, true),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4},
					 {reuseaddr, true},
					 {active, true}]),
    spawn(?MODULE, handle_connections, [self(), Listen]),
    loop([]).

loop(Nodes) ->
    receive
	{add, Node} ->
	    io:format("client connected. #clients = ~B~n", [length(Nodes)+1]),
	    link(Node),
	    loop([Node|Nodes]);

	{bcast, Msg} ->
	    lists:foreach(fun(Node) -> Node ! {send, Msg} end, Nodes),
	    loop(Nodes);

	{'EXIT', Pid, _} ->
	    io:format("client exited.    #clients = ~B~n", [length(Nodes)-1]),
	    loop(lists:delete(Pid, Nodes));

	Other ->
	    io:format("unknown message: ~p~n", [Other]),
	    loop(Nodes)
    end.


%% Listens for new connections.
%% On a new connections the process spawns off a new listener and
%% continues to service the connected client.
handle_connections(Server, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(?MODULE, handle_connections, [Server, Listen]),
    %% register client with the server
    connect(Server, self()),
    handle_requests(Server, Socket).

%% Handles all incomming and outgoing communication with a client.
handle_requests(Server, Socket) ->
    receive
	{send, Msg} ->
	    gen_tcp:send(Socket,Msg),
	    ?MODULE:handle_requests(Server, Socket);

	{tcp, Socket, <<Id:16,_/binary>>=Bin} ->
	    io:format("broadcasting id(~B) size(~B)~n", [Id, size(Bin)]),
	    broadcast(Server, Bin),
	    ?MODULE:handle_requests(Server, Socket);

	{tcp_closed, Socket} ->
	    %%io:format("socket closed~n")
	    %%disconnect(Server, self())
	    exit(socket_closed)
    end.


