agent_model
===========

Multi-Agent System Model to investigate performance
---------------------------------------------------

This is a project using Erlang to model a contract-based multi-agent system and investigate performance trade-offs.

The standard agents represent edges in a directed graph.  For a single node system, since all agents at each phase
of the search are polled for a response, the approach operates as breadth-first search.

For a multi-node system, problems are shared between brokers and the agents within a given processing node form the local
search neighborhood.

**Major components:**

1 broker_agent per processing node
n standard_agents per processing node
     where each agent is defined by simple configuration information.
     
The broker receives a problem description, logs it, and sends the rfp to agents in its agent list.
If an agent can solve the problem itself, it sends a bid.  If the agent can start the problem, but doesn't get
all the way to the desired end state, it sends the broker agent a new problem that starts at its output and goes
to the desired end state.  If the agent cannot start the problem, it sends the broker agent a no bid message.
If all the agents provide a no bid message, then the broker knows that it cannot solve the problem and informs the
requester of the inability to solve the problem.

The current implementation does support multiple brokers to communicate with each other to solve problems.  And it works
on some problems and configurations.  However, there remain termination issues on some problems and endless loops can occur.

Broker_agents currently support the following messages:

   1.  rfp,
   2.  best_current,
   3.  bid,
   4.  no_bid,
   5.  status,
   6.  reset, and
   7.  stop.

The **rfp message** is sent to the broker_agent and triggers the search for the path from an input node to an output node.
If successfully completed, the broker_agent will send a bid message to all processes in the Reply_to list for each solution
that is found.  The rfp message is of the form

```erlang
   {rfp, Problem}
```

where rfp is the atom rfp, Problem is a variable of the form

```erlang
   Problem = {Reply_to, Input, Output, Prior_Nodes}
```

and

   Reply_to = [ list of processes to provide the results of the search ],
   Input = [ list providing a unique description of the input node ],
   Output = [ list providing a unique description of the output node],
   Prior_Nodes = [ list of nodes that the problem has already been through (to help prevent getting stuck in cycles in the graph) ].

The **best_current message** is sent to the broker_agent to find the best, currently known path from an input node to an output node.
The provided result is a lookup of the best answer that meets the specified criteria.  The best_current message is of the form

```erlang
   {best_current, Problem}
```

where best_current is the atom best_current, Problem is a variable of the form

```erlang
   Problem = {Reply_to, Input, Output, Prior_Nodes}
```

and

   Reply_to = [ list of processes to provide the results of the search ],
   Input = [ list providing a unique description of the input node ],
   Output = [ list providing a unique description of the output node],
   Prior_Nodes = [ list of nodes that the answer to the problem should not include -  an empty lists means that any nodes can be passed through ].


The **bid message** is of the form

```erlang
   {bid, proposal}
```

where bid is the atom bid, Proposal is a variable of the form

```erlang
   Proposal = {Input, Output, Services, Cost}
```

and

   Input = [ list providing a unique description of the input node ],
   Output = [ list providing a unique description of the output node],
   Services = [ ordered list of service edges to traverse to go from the input node to the output node],
   Cost = [ list describing the total cost of the traverse described by Services].

The **no_bid message** is of the form

```erlang
   {no_bid, {Name, {Input, Output}}}
```

where bid is the atom bid and

   Name = Agent_name (uniquely assigned for each graph edge)
   Input = [ list providing a unique description of the input node for the problem being no-bid ],
   Output = [ list providing a unique description of the output node for the problem being no-bid ].


The **status message** is of the form

```erlang
   {status, Pid}
```

where status is the atom status and Pid is the process id to deliver the status information.


The **reset message** is of the form

```erlang
   reset
```

where reset is the atom reset.


The **stop message** is of the form

   stop

where stop is the atom stop.

