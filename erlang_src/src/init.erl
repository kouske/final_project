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
	Neighbors = os_dispatcher:get_neighbors(),
	Neighbors_IP = os_dispatcher:get_neighbors_with_ip(),
	
	% TODO: Add startup connection phase
	
	
	ghs:start(Neighbors, MyMac).
	
	
compile_all() ->
	compile:file("ghs.erl"),
	compile:file("os_dispatcher.erl").
	

