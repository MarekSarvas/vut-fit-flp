
remove_spaces([], []).
remove_spaces([[A, _, B]|Ls], Rem) :- 
    remove_spaces(Ls, Rem2), 
    append([[A, B]], Rem2, Rem).
    

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
    remove_spaces(LL, S),
    write(S),
    halt.

