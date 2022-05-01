# FLP project 1 - Hamiltonian Cycle
**Author**: Marek Sarvas \
**Login**: xsarva00
## Run
- *make* compiles the programme
- *make run* runs programme on test input taken from assignment
## Implementation
1. input is read into array of lines, where every line is stored as [V1, , V2], where V1, V2 are nodes of graph
2. empty space is removed and list of nodes is created
3. from list of edges, new clauses edge(V1, V2) are made using *assertz*
4. finds all cycles
    - gets starting symbol (always first node) and add it into list of seen nodes
    - recursively find all next nodes that creates edge between previous node and themselfs and were not seen before
    - add such nodes into list of seend nodes and remove them from list of available nodes
    - after using all available nodes add first node as last if such edge exists
    - seen nodes are concatenated and cretes all hamilton cycles, including duplicate one (duplicate in reverse direction)
5. remove duplicates by taking out cycle and removing also its reverse from list of all cycles 
6. print in correct format