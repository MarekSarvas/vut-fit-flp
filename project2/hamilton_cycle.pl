
remove_spaces([], []).
remove_spaces([[A, _, B]|Ls], Rem) :- 
    remove_spaces(Ls, Rem2), 
    append([[A, B]], Rem2, Rem).

nodes([], []).
nodes([[N1, N2]|Edges], Ns) :-
    nodes(Edges, NsRec),
    append([N1, N2], NsRec, Ns).

is_in_list(H, [H|_]) :- !.
is_in_list(H, [_|T]) :- is_in_list(H, T).

remove_duplicates(X, Y) :- remove_duplicates(X, Y, []).
remove_duplicates([], [], _).
remove_duplicates([H|T1], [H|T2], Seen) :- \+ member(H, Seen), remove_duplicates(T1, T2, [H|Seen]).
remove_duplicates([H|T1], T2, Seen) :- member(H, Seen), remove_duplicates(T1, T2, Seen).

get_last([T], T).
get_last([_|T], L) :-
    get_last(T, L).

/** create clauses for edge of the graph, both ways because they are not oriented */
create_edges([]).
create_edges([[X, Y]|T]) :-
    assertz(edge(X, Y)),
    assertz(edge(Y, X)),
    create_edges(T).


del_item([], _, []).
del_item([H|T], H, New) :-
    del_item(T, H, New).
del_item([H|T], Item, New) :-
    del_item(T, Item, NewRec),
    append([H], NewRec, New).

create_cycles([], [H|T], H, Cycle) :-
    edge(H, Next),
    last([H|T], Next),
    append([Next], [H|T],Seen2),
    Cycle = Seen2.

create_cycles(Vertices, Seen, Prev, Cycle) :-
    edge(Prev, NextV),
    member(NextV, Vertices),
    del_item(Vertices, NextV, NewVert),
    create_cycles(NewVert, [NextV|Seen], NextV, Cycle).




/**Prevzate z wis input2.pl: nacita riadky(hrany) zo standardneho vstupu a skonci na EOF alebo EOL */
read_line(L, C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !; read_line(LL, _), [C|LL] = L).

isEOFEOL(C) :-
        C == end_of_file;
        (char_code(C, Code), Code==10).

read_lines(Ls) :-
    read_line(L,C),
    (C == end_of_file, Ls = [] ; read_lines(LLs), Ls = [L|LLs]).

/** main part */
start :- 
    prompt(_, ''),
    read_lines(LL),
    remove_spaces(LL, Edges),
    nodes(Edges, Nodes),
    remove_duplicates(Nodes, Cleaned),
    create_clause(Edges),
    write(Edges),
    nl,
    write(Cleaned),
    nl,
    halt.

