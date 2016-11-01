%% @author Maxim and Daniel
%% @doc This module implements the GHS algorithm

-module(ghs).

-export([start/2]).

%% parameters: Neighbors = {MAC, RSSI, Type}, MyMac = MAC address
%% 				
start(Neighbors, MyMac) -> 
	FragID = MyMac, %define initial FragID
	FragLevel = 0, %define initial FragLevel
	io:format("Node ~p starting, start ~n", [MyMac]),
	find_cores(lists:keysort(2, Neighbors), MyMac, FragID, FragLevel).
	

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
	Best_Mac = snd_core_msgs(Neighbors, MyMac), %send messages to all neighbors
	Core_status = receive_core_msgs(Best_Mac, length(Neighbors), core_not_found), %receive and process messages from neighbors
	io:format("Node ~p got status ~p, find_cores ~n", [MyMac, Core_status]),
	New_Frag_ID = math:min(Best_Mac, MyMac), %the fragment ID has to be common, min MAC address of the fragment
											 % will give the same result without message passing
	if (New_Frag_ID == MyMac) -> %core
		broadcast(Neighbors, MyMac, FragID, FragLevel),
		io:format("Node ~p is the core, starting broadcast ~n", [MyMac])
	end,
	main_receive(MyMac, New_Frag_ID, 0, [], Neighbors, [], find, [], []).

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
	{Best_Mac,_,_} = hd(Neighbors), %list is ordered, the first entry is the lowest RSSI
	io:format("sending core messages best_mac = ~p", [Best_Mac]),
	global:send(Best_Mac, {find_core, MyMac, yes}), %send "yes" to the best path
	[global:send(Dest_Mac, {find_core, MyMac, no}) || {Dest_Mac,_,_} <- Neighbors--[hd(Neighbors)]], %send "no" to all the rest.
	Best_Mac.

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
receive_core_msgs(_, 0, Status) -> %no more messages.
	Status;

receive_core_msgs(Best_Mac, Msgs_left, Status) -> 
	
	receive
		{find_cores, Src_Mac, yes} ->
			io:format("Receiving {find_cores, yes} from ~p, receive_core_msgs ~n", [Src_Mac]),
			if (Src_Mac == Best_Mac) -> 
				receive_core_msgs(Best_Mac, Msgs_left - 1, core_found);
			true -> 
				receive_core_msgs(Best_Mac, Msgs_left - 1, Status) end;
		
		{find_cores, _, no} -> 
			receive_core_msgs(Best_Mac, Msgs_left - 1, Status);
		
		_ -> io:format("Node received UNEXPECTED, receive_core_msgs ~n")
	end.
	

%%------------------------------------------------------%%
%% initially, each node is a seperate fragment (accept for the core, the nodes connected via the core are a fragment themselves)
%%
%% 1. send messages including fragment ID and level on all nodes in the fragment (no messages on the core).
%%
%% this stage is not needed in the first iteration of the algorithm, as every node is a fragment.
%%------------------------------------------------------%%

broadcast(Neighbors, MyMac, FragID, FragLevel) -> 
	[global:send(MAC, {broadcast, MyMac, FragID, FragLevel}) || {MAC,_,Type} <- Neighbors, Type == branch].


%%------------------------------------------------------%%
%% this function will find the best outgoing path out of the convergecast list.
%%------------------------------------------------------%%
find_min_outgoing([], Min_Node) -> Min_Node;

find_min_outgoing(ConvergecastList, {Min_SrcMac, {Min_Mac, Min_Rssi, Min_Type}}) ->
	{_, {_, Rssi, _}} = hd(ConvergecastList),
	if (Rssi > Min_Rssi) ->
		find_min_outgoing(ConvergecastList -- hd(ConvergecastList), hd(ConvergecastList));
	true -> 
		find_min_outgoing(ConvergecastList -- hd(ConvergecastList), {Min_SrcMac, {Min_Mac, Min_Rssi, Min_Type}})
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
main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac) ->	
	
	BasicList = lists:keyfind(basic, 3, Neighbors),
	if(BasicList == []) -> 
		io:format("Finished")
	end,
	
	receive		
		%% after the convergecast, the core will send a connect message to the fragment's minimum outgoing basic edge.
		%% this message will merge or absorb the two fragments, depending on the state of the receipient.
		{connect, SrcMac, _, SrcFragLevel} ->
			io:format("Node ~p got connect message from ~p, source FragLevel ~p , main_receive-connect ~n", [MyMac, SrcMac, SrcFragLevel]),
			Neighbor = lists:keyfind(SrcMac, 1, Neighbors), % find the Neighbor entry for the source node
			{_, SrcRssi, _} = Neighbor, % get his rssi
			
			New_Neighbors = (Neighbors -- [Neighbor]) ++ [{SrcMac, SrcRssi, branch}], %change the type of Acc_node to branch

			if (FragLevel == SrcFragLevel) -> % same level, according to the algorithm, this can only happen if both nodes sent connect on the same edge. 
											  %This means they are both in "found" state.
				Core = math:min(MyMac, SrcMac),
				if (MyMac == Core) -> % core, send new broadcast.
					io:format("Node ~p got CONNECT, and is now the new core, sending BROADCAST, main_receive-connect ~n", [MyMac]),
					[global:send(MAC, {broadcast, MyMac, Core, FragLevel + 1})|| {MAC, _, Type} <- New_Neighbors, Type == branch]; %forward to all branches
				true -> % not core, the other node is the core. do nothing.
					[]
				end,
				% TODO: what is this? FragLevel changed, check messages.

				%send accept to all test messages with fragLevel lower or equal to our new fragLevel.
				[global:send(MAC, {accept, MyMac, MsgFragID, MsgFragLevel})|| {MAC, MsgFragID, MsgFragLevel} <- Messages, MsgFragLevel =< (FragLevel +1)],
		
				main_receive(MyMac, Core, FragLevel +1, if (MyMac == Core) -> []; true -> Core end, New_Neighbors, Messages, find, [], []);	
			   
			true -> % /=, probably >. different levels.
				if (State == found) -> % no need for the other fragment to join the search. do nothing.
				io:format("Node ~p is in FOUND state, no need for aid in searching, main_receive-connect ~n", [MyMac]),
					[];
				true -> %find, the other fragment will join the search. send broadcast to the other fragment.
					io:format("Node ~p is in FIND state, sending broadcast to ~p ,main_receive-connect ~n", [MyMac, SrcMac]),
					global:send(SrcMac, {broadcast, MyMac, FragID, FragLevel})
				end,
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac)
			end;			
			
		
		%% the core sends this message to the node in it's fragment that is connected to the minimum ougoing basic edge.
		%% the node receiving this message needs to sent a connect message to the node on the other side of the minimum edge.
		{change_core} -> 
			io:format("Node ~p sending CONNECT to ~p , main_receive-change_core ~n", [MyMac, Acc_Mac]),
			global:send(Acc_Mac, {connect, MyMac, FragID, FragLevel}),
			
			Acc_node = lists:keyfind(Acc_Mac, 1, Neighbors),
			{_, Acc_Rssi, _} = Acc_node,
			
			New_Neighbors = (Neighbors -- [Acc_node]) ++ [{Acc_Mac, Acc_Rssi, branch}], %change the type of Acc_node to branch
			io:format("Updating neighbors list, new list is: ~n"),
			[io:format("~p ", [X]) || X <- New_Neighbors],
			main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, []);
		
		
		%% test message is sent to the minimum outgoing basic edge to see whether the node on the other side is in the same fragment or not.
		%% if the nodes are of the same fragments, the edge between them is rejected.
		%% if the nodes are not on the same fragment, the edge can be a branch.
		{test, SrcMac, SrcFragID, SrcFragLevel} -> 
			Pend_Test = lists:keyfind(SrcMac, 1, Messages),
			
			io:format("Node ~p got TEST message from ~p, main_receive-test ~n", [MyMac, SrcMac]),
			
			if (Pend_Test /= false) -> %found
				New_Messages = Messages -- [Pend_Test];
			true -> 
				New_Messages = Messages
			end,
			
			if (FragID /= SrcFragID) ->  %diferent fragments
				if (FragLevel >= SrcFragLevel) -> %levels comply
					io:format("Node ~p sending ACCEPT message to ~p, main_receive-test ~n", [MyMac, SrcMac]),
					{accept, MyMac, SrcFragID, SrcFragLevel} ! SrcMac, %send accept
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, New_Messages, State, ConvergecastList, Acc_Mac); %reiterate
				true -> %levels do not comply, add message to list and reiterate
					io:format("Node ~p got test from ~p, levels do not comply, main_receive-test ~n", [MyMac, SrcMac]),
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, New_Messages ++ [{SrcMac, SrcFragID, SrcFragLevel}], State, ConvergecastList, Acc_Mac) 
				end; 
			true -> %same fragment, send reject
				io:format("Node ~p got TEST from ~p, same branch - reject, main_receive-test ~n", [MyMac, SrcMac]),
				global:send(SrcMac, {reject, MyMac}), 
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, New_Messages, State, ConvergecastList, Acc_Mac) %reiterate
			end; 

			
		%% a broadcast message is sent to update the fragID and fragLevel.
		%% after a broadcast message is received, the convergecast process should begin.
		{broadcast, SrcMac, SrcFragID, SrcFragLevel} -> 
			{Min_Mac,Min_Rssi,_} = find_min_outgoing(Neighbors), %find the minimum outgoing basic edge
			if (Min_Rssi == 1000) -> %no basics
				Branches_No = [0||{_,_,Type} <- Neighbors, Type == branch],
				if(Branches_No == [0]) -> %leaf
					global:send(Father, {convergecast, {MyMac, {Min_Mac, 1000, basic}}});
					true -> []
				end;
			true -> %yes basics	
				global:send(Min_Mac, {test, MyMac, SrcFragID, SrcFragLevel}) %send a test message to the min edge
			end,
			%forward to all branches except the father
			[global:send(MAC, {broadcast, MyMac, SrcFragID, SrcFragLevel}) || {MAC, _, Type} <- Neighbors, MAC /= SrcMac, Type == branch], 
 			%reiterate with new FragID, FragLevel, Father and empty ConvergecastList.
			io:format("Node ~p got BROADCAST from ~p, sending test to ~p and forwarding to sons, main_receive-broadcast~n", [MyMac, SrcMac, Min_Mac]),
			main_receive(MyMac, SrcFragID, SrcFragLevel, SrcMac, Neighbors, Messages, find, [], Acc_Mac);
			
		
		{accept, SrcMac, SentFragID, SentFragLevel} ->
			if (SentFragID == FragID) and (SentFragLevel == FragLevel) -> %properties are up to date
				Num_branches = length([0 || {_, _, Type} <- Neighbors, Type == branch]), %get the amount of branches for that node (including father).
				Accept_Node = lists:keyfind(SrcMac, 1, Neighbors), %find the node that sent the message.
				
				if (MyMac == FragID) -> %core
					Num_children = Num_branches; %no father
				true ->  %not core
					Num_children = (Num_branches - 1) %father
				end,
				
				if ((Num_children - (length(ConvergecastList))) == 0) -> %if there're no branches except for Father (leaf) - no need to wait for convergecast messages
						Min_basic = find_min_outgoing(ConvergecastList ,{MyMac, Accept_Node}), %find minimum Rssi edge out of all candidates
						{_, {Min_Mac, _, _}} = Min_basic, % get mac of the best node
						if (MyMac == FragID) -> %core
							io:format("Node ~p (the core) got ACCEPT from ~p, sending CHANGE_CORE to ~p, main_receive-accept", [MyMac, SrcMac, Min_Mac]),
							global:send(Min_Mac, {change_core});
						true -> %not core
							io:format("Node ~p got ACCEPT from ~p, sending CONVERGECAST to ~p, main_receive-accept", [MyMac, SrcMac, Father]),
							global:send(Father, {convergecast, Min_basic})
						end,
						main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], Min_basic); %reiterate
				true -> %we need to wait for the convergecast messages
						%update ConvergecastList and reiterate
						main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList ++ [{MyMac, Accept_Node}], Acc_Mac) 
				end;
			true -> %properties are outdated 
				io:format("Node ~p got outdated accept from ~p~n", [MyMac, SrcMac]),
			%%
			%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			%%
			%% TODO : discarding is no good. we need to check if we can still accept or to reject (and test again). 
			%%		  doing nothing makes the process halt because there will never be another accept or revect because we did not send another test.
			%%
			%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			%%
				main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac) %discard message and reiterate
			end;

		 
		{reject, SrcMac} ->
				Neighbor = lists:keyfind(SrcMac, 1, Neighbors), % find the neighbor
				{_, RSSI, _} = Neighbor, % get his rssi
				New_Neighbors = Neighbors -- [Neighbor] ++ [{SrcMac, RSSI, reject}], % change neighbor type to reject

				Num_branches = length([0 || {_, _, Type} <- New_Neighbors, Type == branch]),
				Num_basic = length([0 || {_, _, Type} <- New_Neighbors, Type == basic]),
				
				if (Num_basic > 0) -> % if more than 1 basic remains, we can test
					{Candidate_Mac, _, _} = find_min_outgoing(New_Neighbors), % find a new outgoing edge
					io:format("Node ~p got REJECT from ~p, sending TEST to ~p, main_receive-reject", [MyMac, SrcMac, Candidate_Mac]),
					global:send(Candidate_Mac, {test, MyMac, FragID, FragLevel}), % test it
					main_receive(MyMac, FragID, FragLevel, Father, New_Neighbors, Messages, State, ConvergecastList, Acc_Mac); % reiterate
				true -> % no more basic edges, check if to send convergecast or not
					if (MyMac == FragID) -> % this node is the core
						if (Num_branches == length(ConvergecastList)) -> % all branches already reported (last message we got was reject)
							{Candidate_Mac, _, _} = find_min_outgoing(ConvergecastList), % find minimum from the list we got from our children
							io:format("Node ~p (the core) got REJECT from ~p, sending CHANGE_CORE to ~p, main_receive-reject", [MyMac, SrcMac, Candidate_Mac]),
							global:send(Candidate_Mac, {change_core}),
							main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], Acc_Mac);
						true -> % not all branched reported, return to main
							main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac)
						end;
					true -> % this node is not the core & no more basic edges
						if (Num_branches == (length(ConvergecastList) + 1)) -> % no more branches as well (we got reject from our candidate)
							ConvergecastRecord = find_min_outgoing(ConvergecastList), % we can report to father
							io:format("Node ~p got REJECT from ~p, sending CONVERGECAST to ~p, main_receive-reject", [MyMac, SrcMac, Father]),
							global:send(Father, {convergecast, ConvergecastRecord}),
							main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], Acc_Mac);
						true -> % we still have more branches
							main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac)
						end
					end
				end;
						
					  
		{convergecast, ConvergecastRecord} -> 
			Num_branches = length([0 || {_, _, Type} <- Neighbors, Type == branch]),
			Num_basic = length([0 || {_, _, Type} <- Neighbors, Type == basic]),
			{SrcMac, {_, _, _}} = ConvergecastRecord,
			
			% if someone accepted, and we have more than 0 basic
			if ((Acc_Mac /= []) and (Num_basic > 0)) -> 
				Count_accept = 1; % count the accept 
			true -> % else if no one reported/we have no more basic (which means no one can report anyway)
				Count_accept = 0 % don't count
			end,
			
			if (MyMac == FragID) -> % this node is the core
				if (Num_branches == (length(ConvergecastList) + Count_accept)) -> % all branches + one basic already reported					
					% find minimum in the list we got from our children
					{Candidate_Mac, {_, _, _}} = find_min_outgoing(ConvergecastList ++ [ConvergecastRecord]),
					io:format("Node ~p (the core) got CONVERGECAST from ~p, sending CHANGE_CORE to ~p, main_receive-convergecast", [MyMac, SrcMac, Candidate_Mac]),
					global:send(Candidate_Mac, {change_core}),
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], Acc_Mac);
				true -> % not all branches reported, return to main
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac)
				end;
			true -> % this node is not the core
				if (Num_branches == length(ConvergecastList + 1 + Count_accept)) -> % all branches + one basic already reported
					ConvergecastCandidate = find_min_outgoing(ConvergecastList ++ [ConvergecastRecord]),
					io:format("Node ~p got CONVERGECAST from ~p, sending CONVERGECAST to ~p, main_receive-convergecast", [MyMac, SrcMac, Father]),
					global:send(Father, {convergecast, ConvergecastCandidate}),
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, found, [], Acc_Mac);
				true -> % not all branched reported, return to main
					main_receive(MyMac, FragID, FragLevel, Father, Neighbors, Messages, State, ConvergecastList, Acc_Mac)
				end
			end;
		
		CatchAll -> io:format("Main receive loop got unknown message ~p~n", [CatchAll])
	end.