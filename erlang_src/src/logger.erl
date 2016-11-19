%% @author a0220048
%% @doc print logs for easier debugging


-module(logger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_debug_mode/1, kill_all/0, print_stats/0]).



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
	kill_all(),

	[rpc:cast(list_to_atom(Node), ghsinit, init_debug, []) || Node <- NodeList].

kill_all() ->
		[global:send(Node, {print_stats}) || Node <- global:registered_names(), Node /= logger],
		[global:send(Node, {exit}) || Node <- global:registered_names(), Node /= logger].
		
print_stats() ->
		[global:send(Node, {print_stats}) || Node <- global:registered_names(), Node /= logger].
	