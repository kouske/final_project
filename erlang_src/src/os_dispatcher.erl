%% @author Maxim and Daniel
%% @doc This module contains simple commands that are translated to wpa_supplicant commands.
%%	The commands are then dispatched to wpa_supplicant to be executed.


-module(os_dispatcher).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_neighbors/0, scan/2, add_peer/1, remove_peer/1, block_connection/0, get_self_mac/0, get_neighbors_with_ip/0, get_self_ip/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

get_neighbors() ->
	Signal = [X || X <- string:tokens(os:cmd("iw dev mesh0 station dump | grep 'signal avg'"), "\n\t "), X /= "signal" , X /= "avg:", X /= "dBm"],
	MAC = [list_to_atom(string:to_upper(string:join(string:tokens(Y, ":"),""))) ||
			Y <- string:tokens(os:cmd("iw dev mesh0 station dump | grep Station"), "\n\t "), Y /= "Station" , Y /= "(on", Y/= "mesh0)"],
	lists:zip3(MAC, Signal, [basic]).


%% parameters: the first field of an ip address
%% returns a list of tuples {MAC, IP}
get_neighbors_with_ip() ->
	IP = os_dispatcher:get_self_ip(),
	{ok, {SubNet, _, _, _}} = inet:parse_address(IP),
	os:cmd("sysctl net.ipv4.icmp_echo_ignore_broadcasts=0"),
	os:cmd("ping " ++ integer_to_list(SubNet) ++ ".255.255.255 -c 1"),
	os:cmd("sysctl net.ipv4.icmp_echo_ignore_broadcasts=1"),
	[{X} || X <- string:tokens(os:cmd("arp"), "\n\t() "), X /= "?", X /= "at", X /= "[ether]", X /= "on",X /= "mesh0"].
	

block_connection() ->
	os:cmd("wpa_cli -i mesh0 set_network 0 no_auto_peer 1").


add_peer(MAC) ->
	os:cmd("wpa_cli -i mesh0 mesh_peer_add " ++ MAC).


remove_peer(MAC) ->
	os:cmd("wpa_cli -i mesh0 mesh_peer_remove " ++ MAC).


scan(Network_Name, Channel) ->
	os:cmd("wpa_cli -i mesh0 scan"),
	WPA_Results = string:tokens(os:cmd("wpa_cli -i mesh0 scan_r | grep MESH | grep " ++ Network_Name ++ " | grep " ++ Channel), "\n\t "),
	[{X} || X <- WPA_Results, X /= "[MESH]", X /= Channel, X/= Network_Name].


get_self_mac() ->
	string:tokens(os:cmd("ifconfig | awk '$0 ~ /inet/ { print $2 }'"), "\n\t ").


get_self_ip() ->
	os:cmd("ifconfig mesh0 | grep 'inet addr:' | cut -d: -f2 | awk '{ print $1}'") -- "\n".

 