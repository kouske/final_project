%% @author a0220048
%% @doc print logs for easier debugging


-module(logger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_debug_mode/1, print_logs/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

init_debug_mode(NodeList) ->
	net_kernel:stop(),
	net_kernel:start([logger, shortnames]),
	io:format("Starting net_kernel for logger~n"),
	erlang:set_cookie(node(), aaa),
	io:format("Setting cookie...~n"),
	io:format("Connecting to nodes...~n"),
	[net_kernel:connect_node(Node) || Node <- NodeList],
	io:format("Connected to nodes ~p~n", [nodes()]),

	[rpc:call(list_to_atom(Node), ghsinit, init_debug, []) || Node <- NodeList],
	
	timer:sleep(5000),
	global:register_name(logger, erlang:self()),
	global:sync(),
	print_logs().

print_logs() ->
	io:format("Logging process started~n"),
	
	receive
		{Node, Msg} -> global:send(Node, Msg);
		
		Msg -> io:format("~p~n", [Msg])
	end.

