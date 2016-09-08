%% @author Maxim and Daniel
%% @doc this is a simple communication test for beglebone with mesh mode.

-module(test).

-export([start/2]).

start(Neighbors, MyMac) -> 
	FragID = MyMac,
	FragLevel = 0,
	find_cores(lists:keySort(2, Neighbors), MyMac, FragID, FragLevel). %Neighbors -> {MAC, RSSI}.

%%------------------------------------------------------%%
%% the first part of the algorithm.
%% 1. function sends a message on all edges.
%% - a YES message on the best edge
%% - a NO message on all the rest
%% 2. wait for receipt of messages from ALL edges.
%% - if a YES was sent from both sides of an edge -> this edge is a core.
%%
%% Edges will be identified via the MAC sddress of the connected node on the other side of the edge.
%%
%% Inputs :
%% Neighbors - an array of {MAC, RSSI} sorted by RSSI (lowest first)
%% MyMac - the MAC address of the node running the code
%% FragID - the ID of the fragment of which the node running the code is part
%% FragLevel - the level of the fragment of which the node running the code is part
%%------------------------------------------------------%%
find_cores(Neighbors, MyMac, FragID, FragLevel) -> 
	Best_Mac = snd_core_msgs(Neighbors, MyMac), %send messages to all neighbors
	Core_status = receive_core_msgs(Best_Mac), %receive and precess messages from neighbors
	if Core_status == core_found -> CORE = Best_Mac; %if a core branch was found, set CORE value
	   true -> CORE = 0 end,
	New_Frag_ID = math:min(Best_Mac, MyMac), %the fragment ID has to be common, min MAC address of the fragment will give the same result without message passing
	broadcast().

%%------------------------------------------------------%%
%% A function for sending core finding messages to all neighbors
%% 
%% Inputs :
%% Neighbors - an array of {MAC, RSSI} sorted by RSSI (lowest first)
%% MyMac - the MAC address of the node running the code
%%
%% Outpust :
%% Best_Mac - the MAC address of the neighbor with the lowers RSSI
%%------------------------------------------------------%%
snd_core_msgs(Neighbors, MyMac) ->
	{Best_Mac,_} = hd(Neighbors), %list is ordered, the first entry is the lowest RSSI
	{find_core, MyMac, yes} ! Best_Mac, %send "yes" to the best path
	[{find_core, MyMac, no} ! Dest_Mac || {Dest_Mac,_} <- Neighbors--[hd(Neighbors)]], %send "no" to all the rest.
	Best_Mac.

%%------------------------------------------------------%%
%% A function for receiving and precessing the core finding messages sent by neighbors
%% - if a YES message was received on the same branch the node sent a YES messages, thet branch is the core
%%
%% Inputs :
%% Best_Mac - the MAC address of the neighbor with the lowest RSSI
%%
%% Outputs :
%% Core_status - the status of the search for a core
%% - if a core branch was found, a "core_found" atom will be returned
%% - if a core branch was not found, a "core_not_found" atom will be returned
%%
%% notes :
%% - the core can only be on the Best_Mac branch, thus there is no need to send the MAC of the neighbour on the other side of the core as it is already known.
%% - 
%%------------------------------------------------------%%
receive_core_msgs(Best_Mac) -> 
	receive
		{find_cores, Src_Mac, yes} -> if Src_Mac == Best_Mac -> core_found;
										 true -> receive_core_msgs(Best_Mac) end;
		{find_cores, Src_Mac, no} -> receive_core_msgs(Best_Mac);
		_ -> erlang:display("unexpected message received, find cores")
	after
		5000 -> core_not_found
	end.  
	

%%------------------------------------------------------%%
%% initially, each node is a seperate fragment (accept for the core, the nodes connected via the core are a fragment themselves)
%%
%% 1. send messages including fragment ID and level on all nodes in the fragment (no messages on the core).
%%
%% this stage is not needed in the first iteration of the algorithm, as every node is a fragment.
%%------------------------------------------------------%%
broadcast() -> [].

%%------------------------------------------------------%%
%% 1. leaves send information on their best path out of the fragment (the closest neighbor that is not in the frsgment). --if no neighbor is apparent, infinity value will be sent.
%% 2. each intemediate node waits for all of its neighbors to send a message (accept for the father).
%% 3. the node will choose the best path out of all the messages it received and its own outgiong paths and forwards it to the node from whom the previous broadcast was received.
%% 4. the process will recursively continue untill it reaches the core.
%%------------------------------------------------------%%
convergecast() -> [].

%%------------------------------------------------------%%
%% 1. the two nodes connected to the core can now send the paths and decide on the best one.
%% 2. a message is sent down through the branches to the node that is connected to the path.
%% 3. the node will send a message through the path requesting to join the two fragments. the join method is dependent on the levels and Id's of the nodes on both sides of the path.
%%------------------------------------------------------%%
change_core() -> [].

%%------------------------------------------------------%%
%% this function will find the best outgoing path.
%%
%% 1. send a message to all paths with frag ID and level.
%% 2. wait for a response that will tell if the path is outgoing or not.
%% 3. 
%%------------------------------------------------------%%
find_min_outgoing() -> [].

