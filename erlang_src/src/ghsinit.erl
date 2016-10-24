%% @author Maxim Altshul & Daniel Harari
%% @doc @todo Add description to init.

-module(ghsinit).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, init_debug/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================



init_debug(Name) -> 
	% TODO: Add init for debug mode
	net_kernel:start([list_to_atom(Name), longnames]),
	erlang:set_cookie(node(), aaa),
	global:register_name(list_to_atom(Name), erlang:self()),
	timer:sleep(10000),
	[net_kernel:connect_node('node' ++ I ++ "@MTG-DANIELHA") || I <- lists:seq(1,6)],
	Me = node(),
	case Me of
		'node1@MTG-DANIELHA' -> 
				Neighbors = [{node2, 10, basic},
							 {node4, 30, basic}],
				 MyMac = node1;
		'node2@MTG-DANIELHA' -> 
				Neighbors = [{node1, 10, basic}, 
							 {node6, 15, basic}, 
							 {node3, 10, basic}],
				 MyMac = node2;
		'node3@MTG-DANIELHA' -> 
				Neighbors = [{node2, 10, basic}, 
							 {node6, 20, basic}, 
							 {node5, 15, basic}, 
							 {node4, 10, basic}],
				 MyMac = node3;
		'node4@MTG-DANIELHA' -> 
				Neighbors = [{node3, 10, basic}, 
							 {node1, 30, basic}, 
							 {node5, 10, basic}],
				 MyMac = node4;
		'node5@MTG-DANIELHA' -> 
				Neighbors = [{node4, 10, basic}, 
							 {node3, 15, basic}, 
							 {node6, 10, basic}],
				 MyMac = node5;
		'node6@MTG-DANIELHA' -> 
				Neighbors = [{node5, 10, basic}, 
							 {node3, 20, basic}, 
							 {node2, 10, basic}],
				 MyMac = node6
				end,
	ghs:start(Neighbors, MyMac).

init() ->
	compile_all(),
	MyMac = os_dispatcher:get_self_mac(),
	MyIP = os_dispatcher:get_self_ip(),
	Neighbors = os_dispatcher:get_neighbors(),
	Neighbors_IP = os_dispatcher:get_neighbors_with_ip(),
	
	% start the node
	net_kernel:start([list_to_atom(MyMac ++ "@" ++ MyIP), longnames]),
	erlang:set_cookie(node(), final_project),
	global:register_name(list_to_atom(MyMac), erlang:self()),
	
	% TODO: connect function here
	
	ghs:start(Neighbors, MyMac).
	
	
compile_all() ->
	compile:file("ghs.erl"),
	compile:file("os_dispatcher.erl").
	

