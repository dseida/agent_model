agent_model
===========

Multi-Agent System Model to investigate performance

This is a project using Erlang to model a contract-based multi-agent system and investigate performance trade-offs.

The standard agents represent edges in a directed graph.  Given that all agents at each level of the search are polled 
for a response puts the approach in the category of a breadth-first search.

Major components:

1 broker_agent per node
n standard_agents per node
     where each agent is defined by simple configuration information.
     
The broker receives a problem description, assigns an rfp number, and sends the rfp to all the standard agents.
If an agent can solve the problem itself, it sends a bid.  If the agent can start the problem, but doesn't get
all the way to the desired end state, it sends the broker agent a new problem that starts at its output and goes
to the desired end state.  If the agent cannot start the problem, it sends the broker agent a no bid message.
If all the agents provide a no bid message, then the broker knows that it cannot solve the problem.

In the future, I am looking at having multiple brokers that work together to solve problems across nodes.  The current
structure needs a couple of minor extensions to support this.
