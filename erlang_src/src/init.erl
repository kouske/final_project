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
	% TODO: need to add registration of MAC address as a name + cookie stuff
	Neighbors = os_dispatcher:get_neighbors(),
	ghs:start(Neighbors, MyMac).
	
	
compile_all() ->
	compile:file("ghs.erl"),
	compile:file("os_dispatcher.erl").
	

