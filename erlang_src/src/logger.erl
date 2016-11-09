%% @author a0220048
%% @doc print logs for easier debugging


-module(logger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_debug_mode/1, kill_node/1, print_stats/1]).



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
	[net_kernel:connect_node(list_to_atom(Node)) || Node <- NodeList],
	io:format("Connected to nodes ~p~n", [nodes()]),

	[rpc:cast(list_to_atom(Node), ghsinit, init_debug, []) || Node <- NodeList],
	
	timer:sleep(5000),
	global:register_name(logger, erlang:self()),
	global:sync().

kill_node(Node) ->
		global:send(Node, {exit}).
		
print_stats(Node) ->
		global:send(Node, {print_stats}).

