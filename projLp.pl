
%---------------------------
% Madalena Galrinho - 87546
%---------------------------


%-----------------------------------------------------------------------------
% membro_linha(Elem, Puzzle, Linha): Linha corresponde ao
% termometro que contem a posicao Pos
%-----------------------------------------------------------------------------
membro_linha(Pos, [L|_], L) :- 
	member(Pos,L).
membro_linha(Pos, [_|R], Linha) :- 
	membro_linha(Pos, R, Linha).


%-----------------------------------------------------------------------------
% preenche(Pos, Linha, Lista): Lista tem todas as posicoes 
% que sao necessarias preencher para a posicao Pos ser preenchida
%-----------------------------------------------------------------------------
preenche(Pos, [Pos|_], [Pos]).
preenche(Pos, [P|R], [P|Res]) :- 
	preenche(Pos, R, Res).


%-----------------------------------------------------------------------------
% propaga(Puz, Pos, Posicoes): Posicoes e' uma lista ordenada com
% todas as posicoes que sao necessarias preencher para a 
% posicao Pos ser preenchida.
%-----------------------------------------------------------------------------
propaga([P|_], Pos, Posicoes) :-
	membro_linha(Pos, P, Linha),
	preenche(Pos, Linha, Lista),
	sort(Lista, Posicoes).


%-----------------------------------------------------------------------------
% nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas): 
% verifica se todas as posicoes da lista Posicoes, que pertencem a 
% linhas anteriores a L, estao presentes na lista Ja_Preenchidas.
%-----------------------------------------------------------------------------
nao_altera_linhas_anteriores([], _, _).
nao_altera_linhas_anteriores([(X,Y)|R], L, Ja_Preenchidas) :-
	X < L,
	member((X,Y), Ja_Preenchidas),
	nao_altera_linhas_anteriores(R, L, Ja_Preenchidas).
nao_altera_linhas_anteriores([(X,_)|R], L, Ja_Preenchidas) :-
	X >= L,
	nao_altera_linhas_anteriores(R, L, Ja_Preenchidas).


%-----------------------------------------------------------------------------
% posicoes_de_uma_coluna(C, L, L_Coluna): dada uma coluna C e uma 
% lista L, todos os elementos de L que pertencem a' coluna C 
% estao presentes na lista L_Coluna.
%-----------------------------------------------------------------------------
posicoes_de_uma_coluna(_, [], []).
posicoes_de_uma_coluna(C, [(X,Y)|R], [(X,Y)|Res]) :-
	C == Y,
	posicoes_de_uma_coluna(C, R, Res).
posicoes_de_uma_coluna(C, [_|R], Lista) :- 
	posicoes_de_uma_coluna(C, R, Lista).


%-----------------------------------------------------------------------------
% junta_posicoes_coluna(C, L, L_Coluna): dada uma coluna C, CTotal 
% representa a lista com apenas os elementos das listas L1 e L2
% que estao na coluna C, sem elementos repetidos.
%-----------------------------------------------------------------------------
junta_posicoes_coluna(C, L1, L2, CTotal) :-
	posicoes_de_uma_coluna(C, L1, CL1),
	posicoes_de_uma_coluna(C, L2, CL2),
	append(CL1, CL2, LTotal),
	sort(LTotal, CTotal).


%-----------------------------------------------------------------------------
% total_coluna(Puz, C, Total): Total e' o total da coluna C do puzzle Puz
%-----------------------------------------------------------------------------
total_coluna(Puz, C, Total) :-
	nth1(3, Puz, Linha),
    nth1(C, Linha, Total).


%-----------------------------------------------------------------------------
% total_coluna(Puz, C, Total): Total e' o total da coluna C do puzzle Puz
%-----------------------------------------------------------------------------
linha_das_posicoes([(X,_)|_], L) :-
	L = X.

%-----------------------------------------------------------------------------
% total_coluna(Puz, C, Total): Total e' o total da coluna C do puzzle Puz
%-----------------------------------------------------------------------------
dim_puzzle(L, Dim) :-
	last(L, (_,Y)),
	Dim = Y.

%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_parcial(_, _, 0, _).
verifica_parcial(Puz, L1, Dim, L2) :-
	junta_posicoes_coluna(Dim, L1, L2, L_Total),
	!,
	length(L_Total, Dim_Coluna),
	total_coluna(Puz, Dim, Total_Coluna),
	Dim_Coluna =< Total_Coluna,
	Dim_Menos_1 is Dim-1,
	verifica_parcial(Puz, L1, Dim_Menos_1, L2).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_colunas(Puz, Ja_Preenchidas, Possibilidades_L, Posicoes_linha) :-
	dim_puzzle(Posicoes_linha, Dim),
	verifica_parcial(Puz, Ja_Preenchidas, Dim, Possibilidades_L).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
igual_total_linha(Total, Lista, Lista_Posicoes) :-
	linha_das_posicoes(Lista_Posicoes, L),
	posicoes_de_uma_linha(L, Lista, Posicoes), !,
    length(Posicoes, Dim_Lista),
    Total == Dim_Lista.


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
propaga_combinacao(_, [], []).
propaga_combinacao(Puz, [P|R], Final) :-
	propaga(Puz, P, Posicoes),
	propaga_combinacao(Puz, R, Posicoes_C),
	append(Posicoes, Posicoes_C, Final).

%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
propaga_combinacao_sem_repetidos(Puz, L, S_Final) :-
	propaga_combinacao(Puz, L, Final),
	sort(Final, S_Final).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_alteracao_pos(Possibilidades_L, Ja_Preenchidas, Posicoes_linha):-
	linha_das_posicoes(Posicoes_linha, L),
	nao_altera_linhas_anteriores(Possibilidades_L, L, Ja_Preenchidas).



%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
posicoes_de_uma_linha(_, [], []).
posicoes_de_uma_linha(L, [(X,Y)|R], [(X,Y)|Res]) :-
	L == X,
	posicoes_de_uma_linha(L, R, Res).
posicoes_de_uma_linha(L, [_|R], Lista) :- 
	posicoes_de_uma_linha(L, R, Lista).



%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_posicoes_de_linha([], _).
verifica_posicoes_de_linha([P|R], L) :- 
	member(P,L),
	verifica_posicoes_de_linha(R, L).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_posicoes_de_linha_L_ja_preenchidas(Lista, Poss, Ja_Preenchidas) :-
	linha_das_posicoes(Lista, L),
	posicoes_de_uma_linha(L, Ja_Preenchidas, Ja_Preenchidas_L),
	!,
	verifica_posicoes_de_linha(Ja_Preenchidas_L, Poss).



comb(0, _, []).
comb(K, L, [E | C_L_E]) :- 
	K > 0,
	append(_, [E | L_E], L),
	K1 is K - 1,
	comb(K1, L_E, C_L_E).

combinacoes_totais(L,N,Res) :-
	findall(T, comb(N,L,T), Res).



%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_combinacao(Puz, Total, Posicoes_linha, Ja_Preenchidas, Combinacao_L, Final) :-
	propaga_combinacao_sem_repetidos(Puz, Combinacao_L, Final),
	igual_total_linha(Total, Final,Posicoes_linha),
	verifica_alteracao_pos(Final, Ja_Preenchidas, Posicoes_linha),
	verifica_posicoes_de_linha_L_ja_preenchidas(Posicoes_linha, Final, Ja_Preenchidas),
	verifica_colunas(Puz, Ja_Preenchidas, Final, Posicoes_linha).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
verifica_combinacoes(_, _, _, _, [], []).
verifica_combinacoes(Puz, Posicoes_linha, Total, Ja_Preenchidas, [P|R], [Possibilidade|Poss]) :-
	verifica_combinacao(Puz, Total, Posicoes_linha, Ja_Preenchidas, P, Possibilidade),
	verifica_combinacoes(Puz, Posicoes_linha, Total, Ja_Preenchidas, R, Poss).
verifica_combinacoes(Puz, Posicoes_linha, Total, Ja_Preenchidas, [_|R], Poss) :-
	verifica_combinacoes(Puz, Posicoes_linha, Total, Ja_Preenchidas, R, Poss).



%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------
possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L) :-
	combinacoes_totais(Posicoes_linha, Total, Combinacoes),
	verifica_combinacoes(Puz, Posicoes_linha, Total, Ja_Preenchidas, Combinacoes, Poss),
	sort(Poss, Possibilidades_L).


%-----------------------------------------------------------------------------
% dada uma coluna C, CTotal representa a lista com apenas os 
% elementos das listas L1 e L2 que estao na coluna C,
% sem elementos repetidos.
%-----------------------------------------------------------------------------

total_linha(Puz, L, Total) :-
	nth1(2, Puz, Linha),
    nth1(L, Linha, Total).

fill([], _, Dim, Cont) :-
  Cont =:= Dim + 1.
fill([P|R], L, Dim, Cont) :-
  P = (L, Cont),
  Cont1 is Cont + 1,
  fill(R, L, Dim, Cont1).


informacoes_linha(Puz, L, Posicoes_linha, Total, Dim) :-
  length(Posicoes_linha, Dim),
  total_linha(Puz, L, Total),
  fill(Posicoes_linha, L, Dim, 1).



resolve_linhas(_, Cont_Linha, Ja_Preenchidas,Sol,Dim) :-
  Dim1 is Dim + 1,
  Cont_Linha =:= Dim1,
  Sol = Ja_Preenchidas.
resolve_linhas(Puz, Cont_Linha, Ja_Preenchidas, Sol, Dim) :-
  informacoes_linha(Puz, Cont_Linha, Posicoes_linha, Total, Dim),
  possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Poss),
  tenta_com_possibilidades(Puz, Cont_Linha, Ja_Preenchidas, Poss, Sol, Dim),
  !.


tenta_com_possibilidades(_, _, _,[],_,_).
tenta_com_possibilidades(Puz, Cont_Linha, Ja_Preenchidas, [P|R], Sol, Dim) :-
  append(P, Ja_Preenchidas, Ja_Preenchidas_P),
  sort(Ja_Preenchidas_P, S_Ja_Preenchidas),
  Cont_Linha_1 is Cont_Linha + 1,
  resolve_linhas(Puz, Cont_Linha_1, S_Ja_Preenchidas, Sol, Dim),
  tenta_com_possibilidades(Puz, Cont_Linha, Ja_Preenchidas, R, Sol, Dim).


resolve([P,Y|R], Sol) :-
  length(Y, Dim),
  resolve_linhas([P,Y|R], 1, [], Sol, Dim).