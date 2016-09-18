%% @author Maxim and Daniel
%% @doc This module contains simple commands that are translated to wpa_supplicant commands.
%%	The commands are then dispatched to wpa_supplicant to be executed.


-module(os_dispatcher).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_neighbors/0, scan/0, add_peer/1, remove_peer/1, block_connection/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

get_neighbors() ->
	Signal = [X || X <- string:tokens(os:cmd("iw dev mesh0 station dump | grep 'signal avg'"), "\n\t "), X /= "signal" , X /= "avg:", X /= "dBm"],
	MAC = [Y || Y <- string:tokens(os:cmd("iw dev mesh0 station dump | grep Station"), "\n\t "), Y /= "Station" , Y /= "(on", Y/= "mesh0)"],
	lists:zip(MAC, Signal).

block_connection() ->
	os:cmd("wpa_cli -i mesh0 set_network 0 no_auto_peer 1"),
	os:cmd("wpa_cli -i mesh0 set_network 0 scan_freq 1111").