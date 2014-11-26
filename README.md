agent_model
===========

Multi-Agent System Model to investigate performance

This is a project using Erlang to model a contract-based multi-agent system and investigate performance trade-offs.

The standard agents represent edges in a directed graph.  For a single node system, since all agents at each phase
of the search are polled for a response, the approach operates as breadth-first search.

For a multi-node system, problems are shared between brokers and the agents within a given processing node form the local
search neighborhood.

Major components:

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


