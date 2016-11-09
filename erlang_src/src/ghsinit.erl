%% @author Maxim Altshul & Daniel Harari
%% @doc @todo Add description to init.

-module(ghsinit).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, init_debug/0, init_kernel_debug/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================

init_kernel_debug(Name) ->
	net_kernel:stop(),
	net_kernel:start([Name, shortnames]),
	io:format("Starting net_kernel for ~p~n",[Name]),
	erlang:set_cookie(node(), aaa),
	io:format("Setting cookie...~n").
	
init_debug() -> 
	compile_all(),
	io:format("Connecting to nodes...~n"),
	[net_kernel:connect_node(list_to_atom("node" ++ I ++ "@MTG-DANIELHA")) || I <- ["1","2","3","4","5","6"]],
	io:format("Connected to nodes ~p~n", [nodes()]),
	timer:sleep(1000),
	Me = node(),
	case Me of
		'node1@MTG-DANIELHA' -> 
				Neighbors = [{node2, -10, basic},
							 {node4, -30, basic}],
				 MyMac = node1;
		'node2@MTG-DANIELHA' -> 
				Neighbors = [{node1, -10, basic}, 
							 {node6, -15, basic}, 
							 {node3, -10, basic}],
				 MyMac = node2;
		'node3@MTG-DANIELHA' -> 
				Neighbors = [{node2, -10, basic}, 
							 {node6, -20, basic}, 
							 {node5, -15, basic}, 
							 {node4, -10, basic}],
				 MyMac = node3;
		'node4@MTG-DANIELHA' -> 
				Neighbors = [{node3, -10, basic}, 
							 {node1, -30, basic}, 
							 {node5, -10, basic}],
				 MyMac = node4;
		'node5@MTG-DANIELHA' -> 
				Neighbors = [{node4, -10, basic}, 
							 {node3, -15, basic}, 
							 {node6, -10, basic}],
				 MyMac = node5;
		'node6@MTG-DANIELHA' -> 
				Neighbors = [{node5, -10, basic}, 
							 {node3, -20, basic}, 
							 {node2, -15, basic}],
				 MyMac = node6
				end,

	timer:sleep(5000),
	
	global:register_name(MyMac, erlang:self()),
	global:sync(),
	io:format("Sleeping some more ~n"),
	timer:sleep(5000),
	io:format("Registered names are: ~p~n",[global:registered_names()]),
	timer:sleep(5000),
	ghs:start(Neighbors, MyMac).

init() ->
	compile_all(),
	MyMac = os_dispatcher:get_self_mac(),
	MyIP = os_dispatcher:get_self_ip(),
	Neighbors = os_dispatcher:get_neighbors(),
	Neighbors_IP = os_dispatcher:get_all_nodes(),
	
	% start the node
	net_kernel:start([list_to_atom(MyMac ++ "@" ++ MyIP), longnames]),
	erlang:set_cookie(node(), final_project),
	
	%connect to all one hop neighbors
	[net_kernel:connect_node(list_to_atom(MAC ++ "@" ++ IP)) || {MAC, IP} <- Neighbors_IP, lists:keyfind(MAC, 1, Neighbors) /= false], 
	
	global:register_name(list_to_atom(MyMac), erlang:self()),
	
	% TODO: connect function here
	
	ghs:start(Neighbors, MyMac).
	
	
compile_all() ->
	compile:file("c:/users/danielha/github/final_project/erlang_src/src/ghs.erl"),
	compile:file("c:/users/danielha/github/final_project/erlang_src/src/os_dispatcher.erl").
	

