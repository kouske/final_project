%% @author Maxim and Daniel
%% @doc This module implements the GHS algorithm

-module(ghs).

-export([start/2]).

-define(MAX_PEERS, 3).
-define(NETWORK_NAME, final_project).
-define(CHANNEL, 2437).

%% parameters: Neighbors = {MAC, RSSI, Type}, MyMac = MAC address
%% 				
start(Neighbors, MyMac) -> 
	FragID = MyMac, %define initial FragID
	FragLevel = 0, %define initial FragLevel
	timer:sleep(1000),
	io:format("~p: Starting, list is ~p ~n", [MyMac, lists:reverse(lists:keysort(2, Neighbors))]),
	%timer:sleep(1000),
	find_cores(lists:reverse(lists:keysort(2, Neighbors)), MyMac, FragID, FragLevel).
	

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
%% Neighbors - an array of {MAC, RSSI, Type} sorted by RSSI (lowest first)
%% MyMac - the MAC address of the node running the code
%% FragID - the ID of the fragment of which the node running the code is part
%% FragLevel - the level of the fragment of which the node running the code is part
%%------------------------------------------------------%%
find_cores(Neighbors, MyMac, FragID, FragLevel) -> 
	timer:sleep(1000),
	%Best_Mac = snd_core_msgs(Neighbors, MyMac), %send messages to all neighbors
	%Core_status = receive_core_msgs(MyMac, Best_Mac, length(Neighbors), core_not_found), %receive and process messages from neighbors
	%io:format("~p: Got status ~p. Function: find_cores ~n", [MyMac, Core_status]),
	%timer:sleep(1000),
	%Best_Node = lists:keyfind(Best_Mac, 1, Neighbors),
	{Best_Mac, Best_Rssi, _} = hd(Neighbors),
	New_Neighbors = (Neighbors -- [hd(Neighbors)]) ++ [{Best_Mac, Best_Rssi, branch}],
	%case Core_status of 
	%	core_not_found -> 
			global:send(Best_Mac, {connect, MyMac, FragID, FragLevel}),
			main_receive(MyMac, MyMac, 0, [], New_Neighbors, [], found, [], [], [Best_Mac], []).
	%	core_found ->
	%		New_Frag_ID = min(Best_Mac, MyMac), %the fragment ID has to be common, min MAC address of the fragment
	%											% will give the same result without message passing
	%		if (New_Frag_ID == MyMac) -> %core
	%			broadcast(New_Neighbors, MyMac, FragID, FragLevel),
	%			io:format("Node ~p is the core, starting broadcast ~n", [MyMac]),
	%			Father = [];
	%		true -> 
	%			Father = Best_Mac
	%		end,
			
	%		main_receive(MyMac, New_Frag_ID, 1, Father, New_Neighbors, [], find, [], [])
	%end.	
	
	
	

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
%snd_core_msgs(Neighbors, MyMac) ->
%	{Best_Mac,_,_} = hd(Neighbors), %list is ordered, the first entry is the lowest RSSI
%	io:format("~p: Sending core messages. Best_mac = ~p~n", [MyMac, Best_Mac]),
%	global:send(Best_Mac, {find_cores, MyMac, yes}), %send "yes" to the best path
%	io:format("~p~n", [[global:send(Dest_Mac, {find_cores, MyMac, no}) || {Dest_Mac,_,_} <- (Neighbors--[hd(Neighbors)])]]), %send "no" to all the rest.
%	Best_Mac.

%%------------------------------------------------------%%
%% A function for receiving and precessing the core finding messages sent by neighbors
%% - messages arrive from all branches
%% - if a YES message was received on the same branch the node sent a YES messages, thet branch is the core
%%
%% Inputs :
%% Best_Mac - the MAC address of the neighbor with the lowest RSSI
%% msgs_left - the amount of messages the node needs to receive from this point. this variable is for keeping 
%%			   track of the iterations in the recursive function.
%% Status - core status, this will be the returned value in the end. initially, this value is core_not_found
%% 
%% Outputs :
%% Core_status - the status of the search for a core
%% - if a core branch was found, a "core_found" atom will be returned
%% - if a core branch was not found, a "core_not_found" atom will be returned
%%
%% notes :
%% - the core can only be on the Best_Mac branch, thus there is no need to send the MAC of the neighbour on the
%%	 other side of the core as it is already known.
%% - all nodes send messages to all neighbours, this means that a message needs to be received on every branch
%%------------------------------------------------------%%
%receive_core_msgs(MyMac, _, 0, Status) -> %no more messages.
%	io:format("~p: Received all messages, receive_core_msgs~n", [MyMac]),
%	Status;

%receive_core_msgs(MyMac, Best_Mac, Msgs_left, Status) -> 
%	
%	receive
%		{find_cores, Src_Mac, yes} ->
%			%io:format("~p: Receiving {find_cores, yes} from ~p, receive_core_msgs ~n", [MyMac, Src_Mac]),
%			if (Src_Mac == Best_Mac) -> 
%				receive_core_msgs(MyMac, Best_Mac, Msgs_left - 1, core_found);
%			true -> 
%				receive_core_msgs(MyMac, Best_Mac, Msgs_left - 1, Status) 
%			end;
%		
%		{find_cores, _, no} -> 
%			%io:format("~p: Receiving {find_cores, no} from ~p, receive_core_msgs ~n", [MyMac, Src_Mac]),
%			receive_core_msgs(MyMac, Best_Mac, Msgs_left - 1, Status)
%		
%		%Unexpected -> io:format("Node received UNEXPECTED ~p, receive_core_msgs ~n", [Unexpected])
%	end.
	

%%------------------------------------------------------%%
%% initially, each node is a seperate fragment (accept for the core, the nodes connected via the core are a fragment themselves)
%%
%% 1. send messages including fragment ID and level on all nodes in the fragment (no messages on the core).
%%
%% this stage is not needed in the first iteration of the algorithm, as every node is a fragment.
%%------------------------------------------------------%%

%broadcast(Neighbors, MyMac, FragID, FragLevel) -> 
% 	io:format("~p: Broadcasting to PIDs: ~p~n", [MyMac, [global:send(MAC, {broadcast, MyMac, FragID, FragLevel}) || {MAC,_,Type} <- (Neighbors ++ [{MyMac, -1000, branch}]), Type == branch]]).

%%------------------------------------------------------%%
%% this function will find the best outgoing path out of the convergecast list.
%%------------------------------------------------------%%
find_min_outgoing([], Min_Node) -> Min_Node;

find_min_outgoing(ConvergecastList, {Min_SrcMac, {Min_Mac, Min_Rssi, Min_Type}}) ->
	{_, {_, Rssi, _}} = hd(ConvergecastList),
	if (Rssi > Min_Rssi) ->
		find_min_outgoing(ConvergecastList -- [hd(ConvergecastList)], hd(ConvergecastList));
	true -> 
		find_min_outgoing(ConvergecastList -- [hd(ConvergecastList)], {Min_SrcMac, {Min_Mac, Min_Rssi, Min_Type}})
	end;
	
find_min_outgoing(Neighbors, {Min_Mac, Min_Rssi, Min_Type}) -> 
	{_, Rssi, _} = hd(Neighbors),
	if (Rssi > Min_Rssi) -> 
		find_min_outgoing(Neighbors -- [hd(Neighbors)], hd(Neighbors));
	true -> 
		find_min_outgoing(Neighbors -- [hd(Neighbors)], {Min_Mac, Min_Rssi, Min_Type})
	end.
	
find_min_outgoing(Neighbors) -> 
	Basic_Neighbors = [{Mac, Rssi, Type} || {Mac, Rssi, Type} <- Neighbors, Type == basic],
	if (Basic_Neighbors == []) -> 
		{Inf_Mac,_,_} = hd(Neighbors),
		{Inf_Mac, -1000, basic};
	true -> 
		find_min_outgoing(Basic_Neighbors -- [hd(Basic_Neighbors)], hd(Basic_Neighbors))
	end.

%%------------------------------------------------------%%
%% this function is the main loop of the algorithm.
%% 
%% Neighbors : {Mac, Rssi, Type} -- the full list of all neighbors
%% ConvergecastList : {SrcMac, {Mac, Rssi, Typr}} -- a list of nodes that are candidates for minimum outgoing basic edge.
%%					  the list consists of the local minimum basic edge and the minimum basic edges of the node's children.
%% Messages : list of test message pending reply.
%% State : find/found -- the state of the algorithm.
%%------------------------------------------------------%%
main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv) ->	
	io:format("~p, Neighbors : ~p ~n", [MyMac, Neighbors]),
	receive		
		%% after the convergecast, the core will send a connect message to the fragment's minimum outgoing basic edge.
		%% this message will merge or absorb the two fragments, depending on the state of the receipient.
		{connect, SrcMac, _, SrcFragLevel} ->
			Neighbor = lists:keyfind(SrcMac, 1, Neighbors), % find the Neighbor entry for the source node
			{_, SrcRssi, _} = Neighbor, % get his rssi
			%io:format("~p: Got connect message from ~p, source FragLevel ~p in function main_receive-connect ~n", [MyMac, SrcMac, SrcFragLevel]),
			New_Neighbors = ((Neighbors -- [Neighbor]) ++ [{SrcMac, SrcRssi, branch}]), %change the type of Acc_node to branch
			
			if (FragLevel == SrcFragLevel) -> % same level, according to the algorithm, this can only happen if both nodes sent connect on the same edge. 
											  %This means they are both in "found" state.
				IsMember = lists:member(SrcMac, ConSnd),
				if (IsMember) -> % already sent connect to this node
					IsMin = (min(MyMac, SrcMac) == MyMac),
					if (IsMin) -> % this is the new core
						io:format("~p: Is on the SAME level (~p) with ~p and symmetrical edge ,sending BROADCAST, main_receive-connect~n",[MyMac, FragLevel, SrcMac]),
						[global:send(MAC, {broadcast, MyMac, MyMac, FragLevel + 1})|| {MAC, _, Type} <- (New_Neighbors ++ [{MyMac, doesnt_matter, branch}]), Type == branch], %forward to all branches
						New_Father = [],
						New_FragID = MyMac,
						New_FragLevel = FragLevel +1,
						New_Messages = send_accept_messages(MyMac, New_FragLevel, FragID, Messages);
					true -> 
						New_Father = Father,
						New_FragID = FragID,
						New_FragLevel = FragLevel,
						New_Messages = Messages
					end;
				true -> % new connect
					if (State == found) ->
						io:format("~p: Got NEW CONNECT from ~p State is FOUND~n",[MyMac,SrcMac]),
						New_Father = Father,
						New_FragID = FragID,
						New_Messages = Messages,
						New_FragLevel = FragLevel;
					true ->
						global:send(SrcMac, {broadcast, MyMac, FragID, FragLevel}),
						New_Father = Father,
						New_FragID = FragID,
						New_Messages = Messages,
						New_FragLevel = FragLevel,
						io:format("~p: Got NEW CONNECT from ~p State is FIND~n",[MyMac,SrcMac])
					end
				end,
				main_receive(MyMac, New_FragID, New_FragLevel, New_Father, New_Neighbors, New_Messages, find, ConvergecastList, Acc_Mac, ConSnd, ConRcv ++ [SrcMac]);	
			   
			true -> % /=, probably >. different levels.
				io:format("~p: Is on a DIFFERENT level than ~p ABSORBINGGGG~n", [MyMac, SrcMac]),
				if (State == found) -> % no need for the other fragment to join the search. do nothing.
					io:format("~p: Is in FOUND state, no need for aid in searching, main_receive-connect ~n", [MyMac]),
					[];
				true -> %find, the other fragment will join the search. send broadcast to the other fragment.
					io:format("~p: Is in FIND state, sending broadcast to ~p ,main_receive-connect ~n", [MyMac, SrcMac]),
					global:send(SrcMac, {broadcast, MyMac, FragID, FragLevel}) %% TODO: maybe send self-bcast
				end,
				main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv ++ [SrcMac])
			end;			
			
		
		%% the core sends this message to the node in it's fragment that is connected to the minimum ougoing basic edge.
		%% the node receiving this message needs to sent a connect message to the node on the other side of the minimum edge.
		{change_core} -> 
			io:format("~p: Sending CONNECT to ~p , main_receive-change_core ~n", [MyMac, Acc_Mac]),
			if (Acc_Mac /= []) -> 
				Acc_node = lists:keyfind(Acc_Mac, 1, Neighbors),
				{_, Acc_Rssi, _} = Acc_node,
				global:send(Acc_Mac, {connect, MyMac, FragID, FragLevel}),
				New_Neighbors = ((Neighbors -- [Acc_node]) ++ [{Acc_Mac, Acc_Rssi, branch}]), %change the type of Acc_node to branch
				io:format("~p: Updating neighbors list, new list is: ~p~n", [MyMac, New_Neighbors]),
				IsMin = (min(Acc_Mac, MyMac) == MyMac),
				IsMember = (lists:member(Acc_Mac, ConRcv)),
				if ((IsMember) and (IsMin)) -> % a connect was already received from this node - symmetrical edge - this is the new core
					New_Father = [],
					New_FragID = MyMac,
					New_FragLevel = FragLevel + 1,
					[global:send(MAC, {broadcast, MyMac, MyMac, New_FragLevel})|| {MAC, _, Type2} <- (New_Neighbors ++ [{MyMac, doesnt_matter, branch}]), Type2 == branch], %forward to all branches
					New_Messages = send_accept_messages(MyMac, New_FragLevel, FragID, Messages);
				true -> 
					New_FragLevel = FragLevel,
					New_Father = Father,
					New_FragID = FragID,
					New_Messages = Messages
				end, % new connect, do nothing
				%send accept to all test messages with fragLevel lower or equal to our new fragLevel.
			main_receive(MyMac, New_FragID, New_FragLevel, New_Father, New_Neighbors, New_Messages, State, ConvergecastList, [], ConSnd ++ [Acc_Mac], ConRcv); % TODO : potentially change state
			true -> 
				io:format("~p: ASSERT: trying to send CONNECT to empty. exiting program ****************************************~n", [MyMac])
			end;
		
		
		%% test message is sent to the minimum outgoing basic edge to see whether the node on the other side is in the same fragment or not.
		%% if the nodes are of the same fragments, the edge between them is rejected.
		%% if the nodes are not on the same fragment, the edge can be a branch.
		{test, SrcMac, SrcFragID, SrcFragLevel} -> 
			Pend_Test = lists:keyfind(SrcMac, 1, Messages),
			
			io:format("~p: Got TEST message from ~p, main_receive-test ~n", [MyMac, SrcMac]),
			
			if (Pend_Test /= false) -> %found
				New_Messages = Messages -- [Pend_Test];
			true -> 
				New_Messages = Messages
			end,
			
			if (FragID /= SrcFragID) ->  %diferent fragments
				if (FragLevel >= SrcFragLevel) -> %levels comply
					io:format("~p: Sending ACCEPT message to ~p, main_receive-test ~n", [MyMac, SrcMac]),
					global:send(SrcMac, {accept, MyMac, SrcFragID, SrcFragLevel}), %send accept
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, New_Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv); %reiterate
				true -> %levels do not comply, add message to list and reiterate
					io:format("~p: Got test from ~p, levels do not comply, main_receive-test ~n", [MyMac, SrcMac]),
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, New_Messages ++ [{SrcMac, SrcFragID, SrcFragLevel}], State, ConvergecastList, Acc_Mac, ConSnd, ConRcv) 
				end; 
			true -> %same fragment, send reject
				io:format("~p: Got TEST from ~p, same branch - reject, main_receive-test ~n", [MyMac, SrcMac]),
				global:send(SrcMac, {reject, MyMac}), 
				Neighbor = lists:keyfind(SrcMac, 1, Neighbors), % find the neighbor
				{_, RSSI, _} = Neighbor, % get his rssi
				New_Neighbors = ((Neighbors -- [Neighbor]) ++ [{SrcMac, RSSI, reject}]), % change neighbor type to reject
				main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, New_Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv) %reiterate
			end; 

			
		%% a broadcast message is sent to update the fragID and fragLevel.
		%% after a broadcast message is received, the convergecast process should begin.
		{broadcast, SrcMac, SrcFragID, SrcFragLevel} ->  
			
			if (State == finish) -> 
				global:send(Father, {convergecast, {MyMac, {MyMac, -1000, basic}}}),
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, [], Acc_Mac, ConSnd, ConRcv);
			true -> 
			
				%send accept to all test messages with fragLevel lower or equal to our new fragLevel.
				New_Messages = send_accept_messages(MyMac, SrcFragLevel, SrcFragID, Messages),
				io:format("~p: Received broadcast, SrcMac = ~p, SrcFragID = ~p, SrcFragLevel = ~p~n", [MyMac, SrcMac, SrcFragID, SrcFragLevel]),
				{Min_Mac,Min_Rssi,_} = find_min_outgoing(Neighbors), %find the minimum outgoing basic edge
				io:format("~p: Minimum outgoing edge is : ~p @ RSSI : ~p main_receive-broadcast~n", [MyMac, Min_Mac, Min_Rssi]),
				if (Min_Rssi == -1000) -> %no basics
					io:format("~p: Has no basics ~n", [MyMac]),
					Branches_No = [0||{_,_,Type} <- Neighbors, Type == branch],
					if((length(Branches_No) == 1) and (SrcMac /= MyMac)) -> %leaf
						io:format("~p: This node is a leaf, sending CONVERGECAST to father ~n", [MyMac]),
						global:send(SrcMac, {convergecast, {MyMac, {Min_Mac, -1000, basic}}}),
						main_receive(MyMac, SrcFragID, SrcFragLevel, SrcMac, Neighbors, New_Messages, found, [], [], ConSnd, ConRcv);
					true -> [] % not leaf
					end;
				true -> %yes basics	
					io:format("~p: Still has basics, sending TEST to ~p ~n", [MyMac, Min_Mac]),
					global:send(Min_Mac, {test, MyMac, SrcFragID, SrcFragLevel}) %send a test message to the min edge
				end,
				if (SrcMac /= MyMac) -> %not core (not self message)
					io:format("~p: Is not core, sending BROADCAST to children, main_receive-broadcast ~n", [MyMac]),
				%forward to all branches except the father
					[global:send(MAC, {broadcast, MyMac, SrcFragID, SrcFragLevel}) || {MAC, _, Type} <- Neighbors, MAC /= SrcMac, Type == branch]; 
				true -> % yes core
					main_receive(MyMac, SrcFragID, SrcFragLevel, [], Neighbors, New_Messages, find, [], [], ConSnd, ConRcv)
				end,
				%reiterate with new FragID, FragLevel, Father and empty ConvergecastList.
				main_receive(MyMac, SrcFragID, SrcFragLevel, SrcMac, Neighbors, New_Messages, find, [], [], ConSnd, ConRcv)
			end;
		
		{accept, SrcMac, SentFragID, SentFragLevel} ->
			Accept_Node = lists:keyfind(SrcMac, 1, Neighbors), %find the node that sent the message.
			{_, _, Type} = Accept_Node,
			if ((SentFragID == FragID) and (SentFragLevel == FragLevel) and (Type /= branch)) -> %properties are up to date
				Num_branches = length([0 || {_, _, Type2} <- Neighbors, Type2 == branch]), %get the amount of branches for that node (including father).
				
				if (MyMac == FragID) -> %core
					Num_children = Num_branches; %no father
				true ->  %not core
					Num_children = (Num_branches - 1) %father
				end,
				
				if ((Num_children - (length(ConvergecastList))) == 0) -> %if all children reported - no need to wait for convergecast messages
						Min_basic = find_min_outgoing(ConvergecastList ,{MyMac, Accept_Node}), %find minimum Rssi edge out of all candidates
						io:format("~p: Best subtree node is ~p, main_receive_accept ~n",[MyMac, Min_basic]),
						{Min_Mac_father, {Min_Mac, _, _}} = Min_basic, % get mac of the best node
						if (MyMac == FragID) -> %core
							io:format("~p: I am the core, got ACCEPT from ~p, sending CHANGE_CORE to ~p, main_receive-accept ~n", [MyMac, Min_Mac, Min_Mac_father]), 
							global:send(Min_Mac_father, {change_core});
						true -> %not core
							io:format("~p: Got ACCEPT from ~p, sending CONVERGECAST to ~p, main_receive-accept ~n", [MyMac, SrcMac, Father]),
							global:send(Father, {convergecast, Min_basic})
						end,
						main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], SrcMac, ConSnd, ConRcv); %reiterate
				true -> %we need to wait for the convergecast messages
						io:format("~p: Received ACCEPT from ~p, need to wait, not all convergecast messages recevied ~n", [MyMac, SrcMac]),
						main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, SrcMac, ConSnd, ConRcv) 
				end;
			true -> %properties are outdated 
				io:format("~p: Got outdated accept from ~p~n", [MyMac, SrcMac]),
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv) %discard message and reiterate
			end;

		 
		{reject, SrcMac} ->
				Neighbor = lists:keyfind(SrcMac, 1, Neighbors), % find the neighbor
				{_, RSSI, TYPE} = Neighbor, % get his rssi
				
				if(TYPE /= branch) ->
					New_Neighbors = ((Neighbors -- [Neighbor]) ++ [{SrcMac, RSSI, reject}]); % change neighbor type to reject
				true -> 
					New_Neighbors = Neighbors
				end,
				
				io:format("~p: Got REJECT from ~p, new neighbors are: ~p~n", [MyMac,SrcMac,New_Neighbors]),
				Num_branches = length([0 || {_, _, Type} <- New_Neighbors, Type == branch]),
				Num_basic = length([0 || {_, _, Type} <- New_Neighbors, Type == basic]),
				
				if (Num_basic > 0) -> % if more than 1 basic remains, we can test
					{Candidate_Mac, _, _} = find_min_outgoing(New_Neighbors), % find a new outgoing edge
					io:format("~p: Got REJECT from ~p, sending TEST to ~p, main_receive-reject ~n", [MyMac, SrcMac, Candidate_Mac]),
					global:send(Candidate_Mac, {test, MyMac, FragID, FragLevel}), % test it
					main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv); % reiterate
				true -> % no more basic edges, check if to send convergecast or not
					if (MyMac == FragID) -> % this node is the core
						if (Num_branches == length(ConvergecastList)) -> % all branches already reported (last message we got was reject)
							{Candidate_Mac, {_,Candidate_Rssi,_}} = find_min_outgoing(ConvergecastList, hd(ConvergecastList)), % find minimum from the list we got from our children
							io:format("~p: I am the core, got REJECT from ~p, sending CHANGE_CORE to ~p, main_receive-reject ~n", [MyMac, SrcMac, Candidate_Mac]),
							if (Candidate_Rssi == -1000) -> % THIS IS THE END
								io:format("~p: *****************~n(reject) No more basics! Stage 1 COMPLETE!~n*************************~n", [MyMac]),
								[global:send(MAC, {stage_two}) || MAC <- global:registered_names()],
								stage_two(MyMac, New_Neighbors, FragID);
							true ->
								global:send(Candidate_Mac, {change_core}),
								main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, found, [], Acc_Mac, ConSnd, ConRcv)
							end;
						true -> % not all branched reported, return to main - wait for convergecasts
							main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv)
						end;
					true -> % this node is not the core & no more basic edges
						if (Num_branches == (length(ConvergecastList) + 1)) -> % no more convergecasts (we got reject from our candidate)
							if (ConvergecastList == []) -> %leaf
								ConvergecastRecord = {MyMac, {MyMac, -1000, basic}},
								New_State = finish;
							true -> %not leaf, not core
								ConvergecastRecord = find_min_outgoing(ConvergecastList, hd(ConvergecastList)), % we can report to father
								New_State = State
							end,
							io:format("~p: Got REJECT from ~p, sending CONVERGECAST to ~p, main_receive-reject ~n", [MyMac, SrcMac, Father]),
							global:send(Father, {convergecast, ConvergecastRecord}),
							main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, New_State, [], Acc_Mac, ConSnd, ConRcv);
						true -> % we still have more convergecasts to receive
							main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv)
						end
					end
				end;
						
					  
		{convergecast, ConvergecastRecord} -> 
			{SrcMac, {_, _, _}} = ConvergecastRecord,
			
			New_Neighbors = Neighbors,
			Num_branches = length([0 || {_, _, Type} <- New_Neighbors, Type == branch]),
			Num_basic = length([0 || {_, _, Type} <- New_Neighbors, Type == basic]),
			
			Acc_Node = lists:keyfind(Acc_Mac, 1, New_Neighbors),
			io:format("~p: Received convergecast: ~p, Num_branches: ~p, Num_basic: ~p ,main_receive-convergecast ~n", [MyMac, ConvergecastRecord, Num_branches, Num_basic]),
			
			% if someone accepted, or we have 0 basic
			if ((Acc_Mac /= []) or (Num_basic == 0)) -> 
				Count_accept = 0; %  
			true -> % else if no one reported
				Count_accept = -1 % 
			end,
			%io:format("~p: Count_accept : ~p , main_receive-convergecast~n", [MyMac, Count_accept]),
			%io:format("~p: Convergecastlist : ~p , main_receive-convergecast~n", [MyMac, ConvergecastList]),
			
			if (MyMac == FragID) -> % this node is the core
				
				if (Num_branches == (length(ConvergecastList) + Count_accept + 1)) -> % all branches + one basic already reported					
					% find minimum in the list we got from our children
					{Min_Mac_father, {_, Min_Rssi, _}} = find_min_outgoing(ConvergecastList ++ (if (Acc_Node /= false) -> [{MyMac, Acc_Node}]; true -> [] end), ConvergecastRecord),
					if (Min_Rssi == -1000) -> % THIS IS THE END
						io:format("~p: ***************************~n (convergecast) No more basics! Stage 1 COMPLETE! ~n *******************~n", [MyMac]),
						[global:send(MAC, {stage_two}) || MAC <- global:registered_names()],
						stage_two(MyMac, New_Neighbors, FragID);
					true -> 
						io:format("~p: I am the core, got CONVERGECAST from ~p, sending CHANGE_CORE to ~p, main_receive-convergecast ~n", [MyMac, SrcMac, Min_Mac_father]),
						global:send(Min_Mac_father, {change_core}),
						main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, found, [], Acc_Mac, ConSnd, ConRcv)
					end;
				true -> % not all branches reported, return to main
					main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList ++ [ConvergecastRecord], Acc_Mac, ConSnd, ConRcv)
				end;
			true -> % this node is not the core
				if (Num_branches == (length(ConvergecastList) + 2 + Count_accept)) -> % all branches + one basic already reported
					ConvergecastCandidate = find_min_outgoing(ConvergecastList  ++ (if (Acc_Node /= false) -> [{MyMac, Acc_Node}]; true -> [] end), ConvergecastRecord),
					io:format("~p: Got CONVERGECAST from ~p, sending CONVERGECAST to ~p, main_receive-convergecast ~n", [MyMac, SrcMac, Father]),
					{_, {_, Rssi, _}} = ConvergecastCandidate,
					global:send(Father, {convergecast, ConvergecastCandidate}),
					if (Rssi == -1000) -> % THIS IS THE END
						io:format("~p: *****************~n (convergecast) No more basics! Stage 1 COMPLETE! ~n************************~n", [MyMac]),
						main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, finish, [], Acc_Mac, ConSnd, ConRcv);
					true ->
						main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, found, [], Acc_Mac, ConSnd, ConRcv)
					end;
				true -> % not all branched reported, return to main
					main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList ++ [ConvergecastRecord], Acc_Mac, ConSnd, ConRcv)
				end
			end;

			{stage_two} ->
				stage_two(MyMac, Neighbors, FragID);
			
			
			
		%%%%%%%%%%%%%%% Debug commands %%%%%%%%%%%%%%%%%
		{print_stats} -> 
			io:format("MyMac: ~p~nFragID: ~p~nFragLevel: ~p~nFather: ~p~nNeighbors: ~p~nMessages: ~p~nState: ~p~nConvergecastList: ~p~nAcc_Mac: ~p~nTHANK YOU, COME AGAIN!!~n~n", 
					  [MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac]),
					  main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac, ConSnd, ConRcv);
		
		{exit} -> [];
		
		CatchAll -> io:format("Main receive loop got unknown message ~p~n", [CatchAll])
end.


stage_two(MyMac, Neighbors, CoreID) ->
	os_dispatcher:block_auto_peering(),
	AllVisibleNodes = os_dispatcher:scan(?NETWORK_NAME, ?CHANNEL),
	[os_dispatcher:remove_peer(MAC) || {MAC, _, Type} <- Neighbors, Type /= branch],
	Num_Neighbors = length([0 || {_, _, Type} <- Neighbors, Type == branch]), %check if compiles
	New_Neighbors = [{Mac, Rssi} || {Mac, Rssi, Type} <- Neighbors, Type == branch],
	ha_algorithm(MyMac, CoreID, [], [], AllVisibleNodes, (?MAX_PEERS - Num_Neighbors), New_Neighbors). % AllVisibleNodes should be in {MAC, Metric} format
	
	
ha_algorithm(MyMac, CoreID, BestMac, Reports, AllVisibleNodes, Resources, Neighbors) ->
	% Neighbors here is {MAC, RSSI}
	if (MyMac == CoreID) -> % this is the core
		[global:send(MAC, {broadcast_S2, MyMac}) || MAC <- global:registered_names()]; 
	true -> []
	end,
		
	receive
		{broadcast_S2, SrcMac} -> 
			if ((Resources /= 0) and (length(AllVisibleNodes) > length(Neighbors))) -> % we still have room for more nodes
				% calculate metric on graph
				UnDirectCandidates = [os_dispatcher:send_probe(MAC) || {MAC, _} <- AllVisibleNodes], %send_probe should return a sorted list of {MAC, Metric}
				{BestMac, Metric} = find_min_metric([{MAC, DirectMetric - UnDirectMetric} || {MAC, DirectMetric} <- AllVisibleNodes, {_, UnDirectMetric} <- UnDirectCandidates]),
				global:send(SrcMac, {report, Metric, MyMac}), % send best to core
				ha_algorithm(MyMac, CoreID, BestMac, [], AllVisibleNodes, Resources, Neighbors); % save best
			true -> 
				global:send(SrcMac, {report, finish, MyMac}), % send NO_BASIC to core
				ha_algorithm(MyMac, CoreID, [], [], AllVisibleNodes, Resources, Neighbors)
			end;
			
			
		{connect_S2} -> 
			os_dispatcher:add_peer(BestMac), % connect to the best
			ha_algorithm(MyMac, MyMac, [], [], AllVisibleNodes, Resources - 1, Neighbors ++ [BestMac]); % change fragid to mymac
			
		
		{report, Metric, SrcMac} ->
			if (MyMac == CoreID) -> 
				Num_Nodes = global:registered_nodes(),
				if ((length(Reports) + 1) == Num_Nodes) -> %all reports arrived
					{BestMac, BestMetric} = find_min_metric(Reports ++ [{SrcMac, Metric}]),
					if (BestMetric == finish) -> % no node has an edge to add
						io:format("Algorithm DONE ~n"),
						[global:send(MAC, terminate) || MAC <- global:registered_names()];
					true -> % there are more potential edges
						global:send(BestMac, {connect_S2}),
						ha_algorithm(MyMac, BestMac, [], [], AllVisibleNodes, Resources, Neighbors)
					end;
				true -> % not all reports arrived
					ha_algorithm(MyMac, CoreID, BestMac, Reports ++ [{SrcMac, Metric}], AllVisibleNodes, Resources, Neighbors) % save report
				end;
			true -> % not core
				io:format("~p: Got REPORT and not core... ~n", [MyMac])
			end;	
			
		{terminate} -> io:format("Bye bye");
		
		Unknown -> io:format("~p: Got message ~p~n", [MyMac, Unknown])
	end.



%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%

send_accept_messages(_, _, _, [], Remaining) -> 
	Remaining;
send_accept_messages(MyMac, FragLevel, FragID ,[H|Messages], Remaining) -> 
	{SrcMac, SrcFragID, SrcFragLevel} = H,
	if((SrcFragLevel =< FragLevel) and (SrcFragID /= FragID)) -> 
		global:send(SrcMac, {accept, MyMac, SrcFragID, SrcFragLevel}),
		New_Remaining = Remaining;
	(SrcFragID == FragID) -> 
		New_Remaining = Remaining,
		global:send(SrcMac, {reject, MyMac});
	true -> 
		New_Remaining = Remaining ++ [H]
	end,
	send_accept_messages(MyMac, FragLevel, FragID, Messages, New_Remaining).

send_accept_messages(MyMac, FragLevel, FragID, Messages) -> 
	send_accept_messages(MyMac, FragLevel, FragID, Messages, []).
	

find_min_metric([H|Reports]) -> % welcome function
	find_min_metric(Reports, length(Reports), H). % real function
	
find_min_metric([], 0, Min) -> % final condition
	Min;
find_min_metric([H|Reports], ReportsLeft, {MinMetric, Mac}) -> 
	{Mac, Metric} = H, 
	if (Metric < MinMetric) -> 
		New_MinMetric = Metric;
	true -> 
		New_MinMetric = MinMetric
	end,
	
	find_min_metric(Reports, ReportsLeft-1, {New_MinMetric, Mac}).