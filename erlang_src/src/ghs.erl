%% @author Maxim and Daniel
%% @doc this is a simple communication test for beglebone with mesh mode.

-module(test).

-export([start/2]).

start(Neighbors, MyMac) -> 
	FragID = MyMac, %define initial FragID
	FragLevel = 0, %define initial FragLevel
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
	Core_status = receive_core_msgs(Best_Mac, length(Neighbors), %receive and precess messages from neighbors
	if Core_status == core_found -> CORE = Best_Mac; %if a core branch was found, set CORE value
	   true -> CORE = 0 end, % CORE = 0 -> this node is not adjacent to the core
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
%% - messages arrive from all branches
%% - if a YES message was received on the same branch the node sent a YES messages, thet branch is the core
%%
%% Inputs :
%% Best_Mac - the MAC address of the neighbor with the lowest RSSI
%% msgs_left - the amount of messages the node needs to receive from this point. this variable is for keeping track of the iterations in the recursive function.
%% Status - core status, this will be the returned value in the end. initially, this value is core_not_found
%% 
%% Outputs :
%% Core_status - the status of the search for a core
%% - if a core branch was found, a "core_found" atom will be returned
%% - if a core branch was not found, a "core_not_found" atom will be returned
%%
%% notes :
%% - the core can only be on the Best_Mac branch, thus there is no need to send the MAC of the neighbour on the other side of the core as it is already known.
%% - all nodes send messages to all neighbours, this means that a message needs to be received on every branch
%%------------------------------------------------------%%
receive_core_msgs(_, 0, Status) -> %no more messages.
	Status.
receive_core_msgs(Best_Mac, Msgs_left, Status) -> 
	receive
		{find_cores, Src_Mac, yes} -> if Src_Mac == Best_Mac -> receive_core_msgs(Best_Mac, Msgs_left-1, core_found);
										 true -> receive_core_msgs(Best_Mac, Msgs_left-1, Status) end;
		{find_cores, Src_Mac, no} -> receive_core_msgs(Best_Mac, Msgs_left-1, Status);
		_ -> erlang:display("unexpected message received, find cores")
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
%% this function will find the best outgoing path out of the convergecast list.
%%------------------------------------------------------%%
find_min_outgoing([], Min) -> min;
find_min_outgoing(ConvergecastList, {Min_Mac, Min_Rssi}) -> 
	{Mac, Rssi} = hd(ConvergecastList),
	if Rssi < Min_Rssi -> 
		find_min_outgoing(ConvergecastList -- [hd(ConvergecastList)], hd(ConvergecastList));
	true -> 
		find_min_outgoing(ConvergecastList -- [hd(ConvergecastList)], {Min_Mac, Min_Rssi})
	end.

%%------------------------------------------------------%%
%% this function is the main loop of the algorithm.
%% 
%% Neighbors : {Mac, Rssi}
%% conveergecastList : {Mac, Rssi}
%%------------------------------------------------------%%
main_recceive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList) ->
	receive
		{connect, SrcMac, SrcFragID, SrcFragLevel} -> [];
		
		
		
		{test, SrcMac, SrcFragID, SrcFragLevel} -> 
			if FragID /= SrcFragID ->  %diferent fragments
				if FragLevel >= SrcFragLevel -> %levels comply
					{accept, MyMac, SrcFragID, SrcFragLevel} ! SrcMAC, %send accept
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList); %reiterate
				true -> %levels do not comply, add message to list and reiterate
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages ++ [{SrcMAC, SrcFragID, SrcFragLevel}], State, ConvergecastList) 
				end; 
			true -> %same fragment, send reject
				{reject, MyMac}, 
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList) %reiterate
			end; 
		

		
		{broadcast, SrcMac, SrcFragID, SrcFragLevel} -> 
			[{broadcast, MyMac, SrcFragID, SrcFragLevel} ! MAC | {MAC, _, Type] <- Neighbors, MAC /= SrcMac and Type == branch],
			main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, find, ConvergecastList);


			
		{accept, SrcMAC, SentFragID, SentFragLevel} -> 
			if SentFragID == FragID and SentFragLevel == FragLevel -> %properties are up to date
				Num_branches = length([0|{Mac,_,Type} <- Neighbors, Type == branch]),
				Accept_Node = filter(fun({Mac, Rssi}) -> Mac == SrcMAC end, Neighbors), %find Rssi
				if  Num_branches == 1 -> %if there're no branches except for Father (leaf)
					Min_basic = Accept_Node, %set min to be the node that sent the accept
				true -> %there are more branches
					if length(ConvergecastList ++ [Accept_Node]) == Num_branches -> %if we received all convergecast messages from all the children
						Min_basic = find_min_outgoing(ConvergecastList ++ [Accept_Node], hd(ConvergecastList)); %find minimum Rssi edge out of all candidates
					true -> %we need to wait for the convergecast messages
						main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, ConvergecastList ++ [Accept_Node]); %reiterate


				 
		{reject, SrcMac} -> [];



		{convergecast, SrcMAC, {DstMac, DstRssi}} -> [].