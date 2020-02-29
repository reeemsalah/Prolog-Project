%predicate#3
num_gen(T,T,[T]).
num_gen(F,L,[F|T]):-F=<L,F1 is F+1 , num_gen(F1,L,T).
%predicate#1
grid_build(N,R):- helper(0,N,[],R).
helper(N,N,R,R).
helper(X,N,Acc,R):- X\=N , length(L,N),appendList(L,Acc,NewAcc), X1 is X+1, helper(X1,N,NewAcc,R).
appendList(H,L,[H|L]).
%predicate2
grid_gen(N,M):-
	grid_build(N,M),
	grid_helper(N,M).
grid_helper(_,[]).
grid_helper(N,[H|T]):-
	grid_helper2(N,H),
	grid_helper(N,T).
grid_helper2(N,[H|T]):-
	grid_helper3(N,H),
	grid_helper2(N,T).
grid_helper2(_,[]).
grid_helper3(N,X):-
	N>1,
	N1 is N-1,
	grid_helper3(N1,X);
	X=N.
%predicate4
findmax([H|T],X):-find_max([H|T],H,X).
find_max([],Acc,Acc).
find_max([H|T],Acc,Max):- 
						Acc<H,
						find_max(T,H,Max).
find_max([H|T],Acc,Max):-
						Acc>=H,
						find_max(T,Acc,Max).
						
find_elem([H|_],H).
find_elem([H|T],X):-
						X\=H,
						find_elem(T,X).
check_num_grid(G):-
					
					find_max_grid(G,M),
					present_all(G,1,M).
present_all(_,M,M).						
present_all(G,X,M):-
						X<M,
						find_elem_grid(G,X),
						X1 is X+1,
						present_all(G,X1,M).
	
find_max_grid([],0).
find_max_grid([H|T],X):-
						findmax(H,M1),
						find_max_grid(T,M2),
						greater(M1,M2,X).
greater(M1,M2,M1):- M1>=M2.
greater(M1,M2,M2):- M2>M1.
find_elem_grid([H|_],X):-
					find_elem(H,X).
find_elem_grid([H|T],X):-
					\+ find_elem(H,X),
					find_elem_grid(T,X).
%predicate5
acceptable_distribution(G):- length(G,N),helper2(1,N,G).
helper2(N1,N,_):- N1 is N+1.
helper2(X,N,G):-X=<N,nth1(X,G,Row),length(G,N),get_column(X,N,G,[],Col,1),Row\=Col,X1 is X+1,helper2(X1,N,G).
get_column(_,N,_,Acc,Acc,N2):- N2 is N+1.
get_column(X,N,G,Acc,Col,X1):-X1=<N,nth1(X1,G,RowX),nth1(X,RowX,Temp),append(Acc,[Temp],NewAcc),X2 is X1+1,get_column(X,N,G,NewAcc,Col,X2).
%predicate7
distinct_rows([_]).
distinct_rows([H|T]):- compare(H,T),distinct_rows(T).

compare(_,[]).
compare(L,[H|T]):- L\=H,compare(L,T).
%predicate 6
trans([[H|T] |Tail], [[H|NT] |NTail]) :- 
	firstCol(Tail, NT, Rest), trans(Rest, NRest), firstCol(NTail, T, NRest).
trans([], []).
firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- firstCol(Tail, Col, Rows).
firstCol([], [], []).
%predicate 8
distinct_columns(M):- trans(M,M1),distinct_rows(M1).
%additional_predicate
row_col_match([H|T]):-trans([H|T],Trans),acceptable_distribution([H|T]), helperRowColMatch([H|T], Trans).
helperRowColMatch([],_).
helperRowColMatch([H|T], Trans):-
					helper4(H, Trans),
					helperRowColMatch(T, Trans).
helper4(H,[H|_]).
helper4(H,[X|T]):- X\=H,helper4(H,T).
%acceptable_permutation
acceptable_permutation(L,R):- permutation(L,R),no_common(L,R).
no_common([],[]).
no_common([H1|T1],[H2|T2]):- H1\=H2, no_common(T1,T2).
%helsinki
helsinki(N,M):- grid_gen(N,M), row_col_match(M),distinct_rows(M),check_num_grid(M),distinct_columns(M).