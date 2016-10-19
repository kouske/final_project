%% @author Maxim Altshul & Daniel Harari
%% @doc @todo Add description to init.

-module(init).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, init_debug/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================



init_debug() -> 
	% TODO: Add init for debug mode
	[].

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
	

