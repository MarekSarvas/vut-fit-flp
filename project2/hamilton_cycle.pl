/** remove space from line to create edge in format [A, B] */
remove_spaces([], []).
remove_spaces([[A, _, B]|Ls], Rem) :- 
    remove_spaces(Ls, Rem2), 
    append([[A, B]], Rem2, Rem).

/** create list of graph nodes from edges */
nodes([], []).
nodes([[N1, N2]|Edges], Ns) :-
    nodes(Edges, NsRec),
    append([N1, N2], NsRec, Ns).

/** remove duplicate nodes(creating when loading edges) from graph */
remove_duplicates(X, Y) :- remove_duplicates(X, Y, []).
remove_duplicates([], [], _).
remove_duplicates([H|T1], [H|T2], Seen) :- \+ member(H, Seen), remove_duplicates(T1, T2, [H|Seen]).
remove_duplicates([H|T1], T2, Seen) :- member(H, Seen), remove_duplicates(T1, T2, Seen).

/** get last item of the list */
get_last([T], T).
get_last([_|T], L) :-
    get_last(T, L).

/** create clauses for edge of the graph, both ways because they are not oriented */
create_edges([]).
create_edges([[X, Y]|T]) :-
    assertz(edge(X, Y)),
    assertz(edge(Y, X)),
    create_edges(T).

/** deletes vertex from list of available vertices */
del_item([], _, []).
del_item([H|T], H, New) :-
    del_item(T, H, New).
del_item([H|T], Item, New) :-
    del_item(T, Item, NewRec),
    append([H], NewRec, New).

/** create hamilton cycles */
create_cycles([], [H|T], H, Cycle) :-
    edge(H, Next),
    get_last([H|T], Next),
    append([Next], [H|T], Cycle).

create_cycles(Vertices, Seen, Prev, Cycle) :-
    edge(Prev, NextV),
    \+ member(NextV, Seen),
    del_item(Vertices, NextV, NewVert),
    create_cycles(NewVert, [NextV|Seen], NextV, Cycle).

prep_cycles([H|T], H, T).

/** create reverse list, used for creating reverse hamilton cycle for removing duplicates*/
rev([], []).
rev([H|T], New) :-
    rev(T, Tmp),
    append(Tmp, [H], New).

/** remove cycle C from list [H|T] */
del_rev(_, [], []).
del_rev(C, [C|T], NewL) :-
   del_rev(C, T, NewL).
del_rev(C, [H|T], [H|Tl]):-
    H \= C,
    del_rev(C, T, Tl).

/** remove duplicate cycles -> removes take cycle H and removes its reverse cycle(duplicate) */
remove_dup_cycles([], []).
remove_dup_cycles([H|T], New) :-
    rev(H, Hrev),
    del_rev(Hrev, T, TNoRev),
    remove_dup_cycles(TNoRev, Seen),
    append([H], Seen, New).


/** Print one cycle in correct format */
print_cycle([]):- !.
print_cycle([_]) :- !.
print_cycle([A,B|T]) :-
    write(A),
    write('-'),
    write(B),
    write(' '),
    print_cycle([B|T]).

/** print all cycles */
print_cycles([]) :- !.
print_cycles([H|T]) :-
    print_cycle(H),
    nl,
    print_cycles(T).

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
    create_edges(Edges),
    prep_cycles(Cleaned, H, T),
    findall(Cycle, create_cycles(T, [H], H, Cycle), FinDup),
    remove_dup_cycles(FinDup, Fin),
    print_cycles(Fin),
    halt.
