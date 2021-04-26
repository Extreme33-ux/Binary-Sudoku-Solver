%----------------------------------------------------------------------------------
%                                 PUZZLE BINARIO
%
%                          Autor: Miguel Nunes Goncalves
%                          Numero de Aluno IST: 194238
%----------------------------------------------------------------------------------

:- consult(codigo_comum).
:- use_module(library(clpfd)).

%------------------------------------------------------------
% aplica_R1_triplo(Lista,N_Lista): 
% Recebe uma lista, devolve uma lista que respeita a Regra 1.
%------------------------------------------------------------

aplica_R1_triplo(L,R):- conta_variaveis(L,N), N>=2,!, R=L.
aplica_R1_triplo([A,B,C],_):- A==B,A==C,B==C,nonvar(A),!,fail.
aplica_R1_triplo([A,B,C],R):- A\==B,A\==C, B\==C,!, R=[A,B,C].
aplica_R1_triplo([A,_,C],R):- A==C,!,funcao_inverso(A,K), R=[A,K,C].
aplica_R1_triplo([A,B,_],R):- A==B,!,funcao_inverso(A,K), R=[A,B,K].
aplica_R1_triplo([_,B,C],R):- B==C,funcao_inverso(B,K), R=[K,B,C].

%------------------------------------------------------------------------------
% conta_variaveis(Lista,N): 
% Recebe uma lista, devolve o numero de espacos ainda para preencher na lista.
%-----------------------------------------------------------------------------

conta_variaveis([],0):- !.
conta_variaveis([L|R],N):- var(L),!,conta_variaveis(R,N1), N is N1+1.
conta_variaveis([L|R],N):- nonvar(L), conta_variaveis(R,N).

%----------------------------------------------------------------------
% funcao_inverso(X,N): 
% Recebe um valor X (1 ou 0), e devolve o outro valor.
%
% Funcao auxiliar de aplica_R1_triplo
%----------------------------------------------------------------------

funcao_inverso(X,N):- X==1,!, N=0.
funcao_inverso(X,N):- X==0, N=1.

%----------------------------------------------------------------------------------
% aplica_R1_fila_aux(Fila,N_Fila): 
% Recebe uma fila, devolve a fila resultante de aplicar a Regra 1(apenas um ciclo).
%----------------------------------------------------------------------------------

aplica_R1_fila_aux([A,B,C],N_Fila):- !,aplica_R1_triplo([A,B,C],N_Fila).

aplica_R1_fila_aux([A,B,C|R],N_Fila):- aplica_R1_triplo([A,B,C],[X,Y,Z]),
									   aplica_R1_fila_aux([Y,Z|R],I), 
									   N_Fila=[X|I].

%-----------------------------------------------------------------
% aplica_R1_fila(Fila,N_Fila): 
% Recebe uma fila, devolve a fila resultante de aplicar a Regra 1,
% ate nao poder ser mais aplicada.
%-----------------------------------------------------------------

aplica_R1_fila(Fila,N_Fila):- conta_variaveis(Fila,N), 
							  aplica_R1_fila_aux(Fila,N_Fila),
							  conta_variaveis(N_Fila,M),N==M,!.

aplica_R1_fila(Fila,N_Fila):- aplica_R1_fila_aux(Fila,Temporary),
							  aplica_R1_fila(Temporary,N_Fila).

%--------------------------------------------------------------------------
% aplica_R2_fila(Fila,N_Fila): 
% Recebe uma fila, devolve N_Fila resultante de aplicar a Regra 2 a Fila.
%--------------------------------------------------------------------------

aplica_R2_fila(Fila, N_Fila):-findall(X, (member(X,Fila),X==1), X1),length(X1,A),
							  length(Fila,T),H is T/2,A==H,!,substitui(Fila,0,L),
							  N_Fila=L.

aplica_R2_fila(Fila, N_Fila):-findall(X, (member(X,Fila),X==0), X0),length(X0,A),
							  length(Fila,T),H is T/2,A==H,!,substitui(Fila,1,L),
							  N_Fila=L.

aplica_R2_fila(Fila, N_Fila):-findall(X, (member(X,Fila),X==1), X1),length(X1,A),
							  findall(X, (member(X,Fila),X==0), X0),length(X0,B),
							  length(Fila,T),H is T/2,A<H,B<H,N_Fila=Fila.

%---------------------------------------------------------------------------------------
% substitui(Lista,N,N_Lista): 
% Recebe uma lista, devolve N_Lista resultante de substituir os espacos em branco por N.
%
% Funcao auxiliar de aplica_R2_fila.
%--------------------------------------------------------------------------------------

substitui([],_,[]):- !.
substitui([C|R],N,L):- var(C),!,substitui(R,N,I), L=[N|I].
substitui([C|R],N,L):- nonvar(C), substitui(R,N,I), L=[C|I].

%---------------------------------------------------------------------------------
% aplica_R1_R2_fila(Fila,N_Fila): 
% Recebe uma fila, devolve N_Fila resultante de aplicar a Regra 1 e 2 nesta ordem, 
% num ciclo.
%---------------------------------------------------------------------------------

aplica_R1_R2_fila(Fila,N_Fila):-aplica_R1_fila(Fila,K),aplica_R2_fila(K,N_Fila),!.

%------------------------------------------------------------------------
% aplica_R1_R2_puzzle(Puz,N_Puz): 
% Recebe um puzzle, devolve N_Puz resultante de aplicar a Regra 1 e 2 , 
% tanto nas linhas como nas colunas(apenas um ciclo).
%------------------------------------------------------------------------

aplica_R1_R2_puzzle(Puz,N_Puz):-aplica_R1_R2_puzzle_aux(Puz,Q0_Puz),
								mat_transposta(Q0_Puz,N0_Puz),
								aplica_R1_R2_puzzle_aux(N0_Puz,Q1_Puz),
								mat_transposta(Q1_Puz,N_Puz).

%------------------------------------------------------------------------
% aplica_R1_R2_puzzle_aux(Puzzle,N_Puz):
% Recebe um Puzzle, devolve N_Puz resultante de aplicar a Regra 1 e 2 
% apenas uma vez, de ordem respetiva.
%
% Funcao auxiliar de aplica_R1_R2_puzzle
%------------------------------------------------------------------------
aplica_R1_R2_puzzle_aux([],[]):-!.
aplica_R1_R2_puzzle_aux([C|R],Q_Puz):-aplica_R1_R2_fila(C,Temp_Puz),
									  aplica_R1_R2_puzzle_aux(R,I_Puzz),
									  Q_Puz=[Temp_Puz|I_Puzz].

%-----------------------------------------------------------------------------
% inicializa(Puz,N_Puz): 
% Recebe um puzzle, devolve um puzzle inicializado(as Regras 1 e 2 
% foram aplicadas a todas as colunas ate nao haver mais alteracoes possiveis).
%-----------------------------------------------------------------------------

 inicializa(Puz,N_Puz):- conta_variaveis_global(Puz,N), aplica_R1_R2_puzzle(Puz,N_Puz),
 						 conta_variaveis_global(N_Puz,M),N==M,!.
 inicializa(Puz,N_Puz):- aplica_R1_R2_puzzle(Puz,Temp_Puz),inicializa(Temp_Puz,N_Puz).

%--------------------------------------------------------------------
% conta_variaveis_global(Puzzle,N):
% Recebe um Puzzle, devolve N igual ao numero de espacos brancos por 
% preencher no Puzzle.
%
% Funcao auxilir de inicializa
%--------------------------------------------------------------------

conta_variaveis_global([],N):-N=0.
conta_variaveis_global([L|R],N):-conta_variaveis(L,M),conta_variaveis_global(R,P),N is M+P.

%----------------------------------------------------------------------------
% verifica_R3(Puz): 
% Recebe um puzzle, e avalia se o puzzle respeita a Regra 3.
%----------------------------------------------------------------------------

verifica_R3(Puz):-horizontal(Puz),
				  mat_transposta(Puz,Trans_Puz),
				  horizontal(Trans_Puz),
				  mat_transposta(Trans_Puz,Puz).

%--------------------------------------------------------
% horizontal(Puzzle):
% Recebe um puzzle, obtem as linhas sem espacos brancos, 
% e verifica se algum par e igual.
%
% Funcao auxiliar de verifica_R3
%--------------------------------------------------------
horizontal(Puz):-filtra_linhas(Puz,Filt_Puz),check_linhas_iguais(Filt_Puz).

%--------------------------------------------------------------------
% filtra_linhas(Puzzle, Filt_Puzzle):
% Recebe um Puzzle, devolve Filt_Puzzle que contem as filas de Puzzle
% sem espacos brancos.
%
% Funcao auxiliar de horizontal
%--------------------------------------------------------------------
filtra_linhas([],[]):-!.
filtra_linhas([A|B],Filt_Puz):-conta_variaveis(A,N),N==0,!,
							   filtra_linhas(B,D), Filt_Puz=[A|D].

filtra_linhas([_|B],Filt_Puz):-filtra_linhas(B,Filt_Puz).

%-----------------------------------------------------------
% check_linhas_iguais(Lista):
% Recebe uma lista, verifica se algum par de linhas e igual.
%
% Funcao auxiliar de horizontal
%-----------------------------------------------------------

check_linhas_iguais([]):-!.
check_linhas_iguais([A|B]):- ciclo_check(A,B),check_linhas_iguais(B).

%-----------------------------------------------------------------------
% ciclo_check(Fila, Lista):
% Recebe uma Fila (de um Puzzle), compara-se a fila com as outras filas de
% Puzzle. Devolve "true" se nao houver linhas iguais.
%
% Funcao auxiliar de check_linhas_iguais
%------------------------------------------------------------- ---------

ciclo_check(_,[]):-!.
ciclo_check(A,[B|C]):- \+linhas_iguais(A,B),ciclo_check(A,C).

%----------------------------------------------------------------
% linhas_iguais(Fila_1,Fila_2):
% Recebe Fila_1 e Fila_2, e devolve o valor booleano da igualdade 
% de Fila_1 e Fila_2.
%
% Funcao auxiliar de ciclo_check
%----------------------------------------------------------------
linhas_iguais([], []):-!.
linhas_iguais([H1|R1],[H2|R2]):- H1==H2,linhas_iguais(R1, R2).

%---------------------------------------------------------------------------
% propaga_posicoes(Lista_Posicoes,Puzzle,N_Puzzle):
% Recebe uma Lista_Posicoes que contem as posicoes a serem propagadas, 
% e um Puzzle. N_Puzzle e o resultado de propagar recursivamente,as posicoes
% e mudancas de posicoes. contidas na Lista_Posicoes.
%---------------------------------------------------------------------------

propaga_posicoes([],LC_Puz,N_Puz):-!,verifica_R3(LC_Puz),!,N_Puz=LC_Puz.

propaga_posicoes([(L,C)|R],Puz,N_Puz):- nth1(L,Puz,Linha),
									    mat_elementos_coluna(Puz,C,Col),
									    aplica_R1_R2_fila(Linha,N_Linha),
									    aplica_R1_R2_fila(Col,N_Col),
									    nth1(L,Puz,Linha_Aux),
									    mat_elementos_coluna(Puz,C,Col_Aux),
									    diferencas_linha(0,L,Linha_Aux,N_Linha,L_Mud),
									    diferencas_coluna(0,C,Col_Aux,N_Col,C_Mud),
									    mat_muda_linha(Puz, L, N_Linha,L_Puz),
									    mat_muda_coluna(L_Puz, C, N_Col, LC_Puz),
									    append(L_Mud,C_Mud,L_total),
									    append(L_total,R,L_Posicoes),
									    propaga_posicoes(L_Posicoes,LC_Puz,N_Puz).

%---------------------------------------------------------------------------
% diferencas_linha(N,Num_Linha,Fila,N_Fila,Fila_Final):
% Recebe Num_Linha, Fila e N_Fila, devolve uma lista com as posicoes em que
% as listas Fila e N_Fila sejam diferentes. A variavel N serve como contador
% para as colunas, para efeitos de criar as coordenadas.
%
% Funcao auxiliar de propaga_posicoes
%----------------------------------------------------------------------------
diferencas_linha(_,_,[],[],[]):-!.
diferencas_linha(N,L,[A|B],[C|D],L_Mud):- N0 is N+1,var(A),nonvar(C),!,
										  cria_coordenadas(L,N0,Coor),
										  diferencas_linha(N0,L,B,D,I),L_Mud=[Coor|I].

diferencas_linha(N,L,[_|B],[_|D],L_Mud):- N0 is N+1,diferencas_linha(N0,L,B,D,L_Mud).

%-------------------------------------------------------------------------
% diferencas_coluna(N.Num_Coluna,Coluna,N_Coluna,Coluna_Final):
% Recebe Num_Coluna, Coluna e N_Coluna, devolve uma lista com as posicoes em que
% as listas Coluna e N_Coluna sejam diferentes. A variavel N serve como contador
% para as filas, para efeitos de criar as coordenadas.
%
% Funcao auxiliar de propaga_posicoes
%----------------------------------------------------------------------------
diferencas_coluna(_,_,[],[],[]):-!.
diferencas_coluna(N,C,[E|F],[G|H],C_Mud):- N0 is N+1,var(E),nonvar(G),!,
										   cria_coordenadas(N0,C,Coor),
										   diferencas_coluna(N0,C,F,H,I),C_Mud=[Coor|I].

diferencas_coluna(N,C,[_|F],[_|H],C_Mud):- N0 is N+1,diferencas_coluna(N0,C,F,H,C_Mud).


%---------------------------------------------------------------
% cria_coordenadas(Linha,Coluna,Coordenada):
% Recebe Linha e Coluna, devolve Coordenada, que e igual a (L,C)
%
% Funcao auxiliar de diferencas_coluna e diferencas_linha
%---------------------------------------------------------------
cria_coordenadas(L,C,Coor):-Coor=(L,C).

%-------------------------------------------------------------------------------
%resolve(Puz,Sol): Recebe um puzzle, devolve uma solucao do puzzle, se possivel.
%-------------------------------------------------------------------------------
resolve(Puz,Sol):- inicializa(Puz,Puz_Inic), 
				   verifica_R3(Puz_Inic), 
				   conta_variaveis_global(Puz_Inic,N),
				   N==0,!,Sol=Puz_Inic.

resolve(Puz,Sol):- inicializa(Puz,Puz_Inic), 
				   verifica_R3(Puz_Inic), 
				   encontra_variavel(Puz_Inic,1,List_Coor),
				   mat_muda_posicoes(Puz_Inic,List_Coor,[1],Puz_Alt),
				   propaga_posicoes(List_Coor,Puz_Alt,Fim_Puz),
				   conta_variaveis_global(Fim_Puz,N),
				   N==0,!,Sol=Fim_Puz.

resolve(Puz,Sol):- inicializa(Puz,Puz_Inic), 
				   verifica_R3(Puz_Inic), 
				   encontra_variavel(Puz_Inic,1,List_Coor),
				   mat_muda_posicoes(Puz_Inic,List_Coor,[0],Puz_Alt),
				   propaga_posicoes(List_Coor,Puz_Alt,Fim_Puz),
				   conta_variaveis_global(Fim_Puz,N),
				   N==0,!,Sol=Fim_Puz.

resolve(Puz,Sol):- inicializa(Puz,Puz_Inic), 
				   verifica_R3(Puz_Inic), 
				   encontra_variavel(Puz_Inic,1,List_Coor),
				   mat_muda_posicoes(Puz_Inic,List_Coor,[1],Puz_Alt),
				   propaga_posicoes(List_Coor,Puz_Alt,Fim_Puz),!,
				   resolve(Fim_Puz,Sol).

resolve(Puz,Sol):- inicializa(Puz,Puz_Inic), 
				   verifica_R3(Puz_Inic), 
				   encontra_variavel(Puz_Inic,1,List_Coor),
				   mat_muda_posicoes(Puz_Inic,List_Coor,[0],Puz_Alt),
				   propaga_posicoes(List_Coor,Puz_Alt,Fim_Puz),
				   resolve(Fim_Puz,Sol).

%------------------------------------------------------------------------------
% encontra_variavel(Puzzle,L,Lista_Posicoes):
% Recebe um Puzzle, devolve Lista_Posicoes que contem as posicoes com variaveis.
% A variavel L serve como posicionador de linha.
%
% Funcao auxiliar de resolve
%-------------------------------------------------------------------------------

encontra_variavel([A|_],L,[Coor]):-conta_variaveis(A,N),N\=0,!,
								   encontra_variavel_aux(A,L,1,Coor).

encontra_variavel([_|B],L,Coor):-I is L+1,
								 encontra_variavel(B,I,Coor).

%--------------------------------------------------------------------------
% encontra_variavel_aux(Fila,Linha,Col,Coor):
% Recebe uma Fila, e devolve a coordenada do primeiro espaco branco de Fila.
%
% Funcao auxiliar de encontra_variavel
%--------------------------------------------------------------------------

encontra_variavel_aux([A|_],L,C,Coor):- var(A),!,cria_coordenadas(L,C,Coor).
encontra_variavel_aux([A|B],L,C,Coor):- nonvar(A),I is C+1,
										encontra_variavel_aux(B,L,I,Coor).