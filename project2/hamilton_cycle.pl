
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
    write(Edges),
    nl,
    write(Cleaned),
    nl,
    halt.

