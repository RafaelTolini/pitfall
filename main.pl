:-dynamic posicao/3.
:-dynamic memory/3.
:-dynamic visitado/2.
:-dynamic certeza/2.
:-dynamic energia/1.
:-dynamic pontuacao/1.
:-dynamic ouro/1. %Contador de ouro
:-dynamic blocked/2. %Posições bloqueadas

:-consult('mapas/mapa_dificil.pl').

delete([], _, []).
delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        delete(Tail, Del, Rest)
    ).
	


reset_game :- retractall(memory(_,_,_)), 
			retractall(visitado(_,_)), 
			retractall(certeza(_,_)),
			retractall(energia(_)),
			retractall(pontuacao(_)),
			retractall(posicao(_,_,_)),
            retractall(ouro(_)),  %Contador de ouro  
            retractall(blocked(_,_)), %Posições bloqueadas
			assert(energia(100)),
			assert(pontuacao(0)),
			assert(posicao(1,1, norte)),
            assert(ouro(0)).  %Contador de ouro

:-reset_game.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controle de Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%atualiza pontuacao
atualiza_pontuacao(X):- pontuacao(P), retract(pontuacao(P)), NP is P + X, assert(pontuacao(NP)),!.

%atualiza energia
atualiza_energia(N):- energia(E), retract(energia(E)), NE is E + N, 
					(
					 (NE =<0, assert(energia(0)),posicao(X,Y,_),retract(posicao(_,_,_)), assert(posicao(X,Y,morto)),!);
					 (NE >100, assert(energia(100)),!);
					  (NE >0,assert(energia(NE)),!)
					 ).

%verifica situacao da nova posicao e atualiza energia e pontos
verifica_player :- posicao(X,Y,_), tile(X,Y,'P'), atualiza_energia(-100), atualiza_pontuacao(-1000),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'D'),  atualiza_energia(-50),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'d'),  atualiza_energia(-20),!.
verifica_player :- posicao(X,Y,Z), tile(X,Y,'T'), 
					map_size(SX,SY), random_between(1,SX,NX), random_between(1,SY,NY),
				retract(posicao(X,Y,Z)), assert(posicao(NX,NY,Z)), atualiza_obs, verifica_player,!.
verifica_player :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comandos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%virar direita
virar_direita :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.

%virar esquerda
virar_esquerda :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.

%andar
andar :- posicao(X,Y,P), P = norte, map_size(_,MAX_Y), Y < MAX_Y, YY is Y + 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
andar :- posicao(X,Y,P), P = sul,  Y > 1, YY is Y - 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = leste, map_size(MAX_X,_), X < MAX_X, XX is X + 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = oeste,  X > 1, XX is X - 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
% pegar ouro
pegar :- 
    posicao(X,Y,_), 
    tile(X,Y,'O'), 
    retract(tile(X,Y,'O')), assert(tile(X,Y,'')), 
    atualiza_pontuacao(-5), 
    atualiza_pontuacao(1000 ), 
    set_real(X,Y),
    ouro(Qtd), retract(ouro(Qtd)), QtdNovo is Qtd + 1, assert(ouro(QtdNovo)),!.

% pegar powerup
pegar :-
    posicao(X,Y,_), 
    tile(X,Y,'U'), 
    retract(tile(X,Y,'U')), assert(tile(X,Y,'')), 
    atualiza_energia(20),
    atualiza_pontuacao(-5),
    set_real(X,Y),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funcoes Auxiliares de navegação e observação
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		 
%Define as 4 adjacencias		 
adjacente(X, Y) :- posicao(PX, Y, _), map_size(MAX_X,_),PX < MAX_X, X is PX + 1.  
adjacente(X, Y) :- posicao(PX, Y, _), PX > 1, X is PX - 1.  
adjacente(X, Y) :- posicao(X, PY, _), map_size(_,MAX_Y),PY < MAX_Y, Y is PY + 1.  
adjacente(X, Y) :- posicao(X, PY, _), PY > 1, Y is PY - 1.  

%cria lista com a adjacencias
adjacentes(L) :- findall(Z,(adjacente(X,Y),tile(X,Y,Z)),L).

%define observacoes locais
observacao_loc(brilho,L) :- member('O',L).
observacao_loc(reflexo,L) :- member('U',L).

%define observacoes adjacentes
observacao_adj(brisa,L) :- member('P',L).
observacao_adj(palmas,L) :- member('T',L).
observacao_adj(passos,L) :- member('D',L).
observacao_adj(passos,L) :- member('d',L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tratamento de KB e observações
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%consulta e processa observações
atualiza_obs:-adj_cand_obs(LP), observacoes(LO), iter_pos_list(LP,LO), observacao_certeza, observacao_vazia.

%adjacencias candidatas p/ a observacao (aquelas não visitadas)
adj_cand_obs(L) :- findall((X,Y), (adjacente(X, Y), \+visitado(X,Y)), L).

%cria lista de observacoes
observacoes(X) :- adjacentes(L), findall(Y, observacao_adj(Y,L), X).

%itera posicoes da lista para adicionar observacoes
iter_pos_list([], _) :- !.
iter_pos_list([H|T], LO) :- H=(X,Y), 
							((corrige_observacoes_antigas(X, Y, LO),!);
							adiciona_observacoes(X, Y, LO)),
							iter_pos_list(T, LO).							 

%Corrige observacoes antigas na memoria que ficaram com apenas uma adjacencia
corrige_observacoes_antigas(X, Y, []):- \+certeza(X,Y), memory(X,Y,[]).
corrige_observacoes_antigas(X, Y, LO):-
	\+certeza(X,Y), \+ memory(X,Y,[]), memory(X, Y, LM), intersection(LO, LM, L), 
	retract(memory(X, Y, LM)), assert(memory(X, Y, L)).

%Adiciona observacoes na memoria
adiciona_observacoes(X, Y, _) :- certeza(X,Y),!.
adiciona_observacoes(X, Y, LO) :- \+certeza(X,Y), \+ memory(X,Y,_), assert(memory(X, Y, LO)).

%Quando há apenas uma observação e uma unica posição incerta, deduz que a observação está na casa incerta
%e marca como certeza
%observacao_certeza:- findall((X,Y), (adjacente(X, Y), 
%						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,ZZ),ZZ\=[])),
%						memory(X,Y,Z), Z\=[]), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).
						
observacao_certeza:- observacao_certeza('brisa'),
						observacao_certeza('palmas'),
						observacao_certeza('passos').
						
observacao_certeza(Z):- findall((X,Y), (adjacente(X, Y), 
						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,[Z]))),
						memory(X,Y,[Z])), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).						

%Quando posição não tem observações
observacao_vazia:- adj_cand_obs(LP), observacao_vazia(LP).
observacao_vazia([]) :- !.
observacao_vazia([H|T]) :- H=(X,Y), ((memory(X,Y,[]), \+certeza(X,Y),assert(certeza(X,Y)),!);true), observacao_vazia(T).

%Quando posicao é visitada, atualiza memoria de posicao com a informação real do mapa 
set_real(X,Y):- ((retract(certeza(X,Y)), assert(certeza(X,Y)),!); assert(certeza(X,Y))), set_real2(X,Y),!.
set_real2(X,Y):- tile(X,Y,'P'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brisa])),!);assert(memory(X,Y,[brisa]))),!.
set_real2(X,Y):- tile(X,Y,'O'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brilho])),!);assert(memory(X,Y,[brilho]))),!.
set_real2(X,Y):- tile(X,Y,'T'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[palmas])),!);assert(memory(X,Y,[palmas]))),!.
set_real2(X,Y):- ((tile(X,Y,'D'),!); tile(X,Y,'d')), ((retract(memory(X,Y,_)),assert(memory(X,Y,[passos])),!);assert(memory(X,Y,[passos]))),!.
set_real2(X,Y):- tile(X,Y,'U'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[reflexo])),!);assert(memory(X,Y,[reflexo]))),!.
set_real2(X,Y):- tile(X,Y,''), ((retract(memory(X,Y,_)),assert(memory(X,Y,[])),!);assert(memory(X,Y,[]))),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa real
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_player(X,Y) :- posicao(X,Y, norte), write('^'),!.
show_player(X,Y) :- posicao(X,Y, oeste), write('<'),!.
show_player(X,Y) :- posicao(X,Y, leste), write('>'),!.
show_player(X,Y) :- posicao(X,Y, sul), write('v'),!.
show_player(X,Y) :- posicao(X,Y, morto), write('+'),!.

%show_position(X,Y) :- show_player(X,Y),!.
show_position(X,Y) :- (show_player(X,Y); write(' ')), tile(X,Y,Z), ((Z='', write(' '));write(Z)),!.

show_map :- map_size(_,MAX_Y), show_map(1,MAX_Y),!.
show_map(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_position(X,Y), write(' | '), XX is X + 1, show_map(XX, Y),!.
show_map(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_map(1, YY),!.
show_map(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa conhecido
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_mem_info(X,Y) :- memory(X,Y,Z), 
		((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
		((member(brisa, Z), write('P'));write(' ')),
		((member(palmas, Z), write('T'));write(' ')),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!.

show_mem_info(X,Y) :- \+memory(X,Y,[]), 
			((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
			write('     '),!.		
		
		

show_mem_position(X,Y) :- posicao(X,Y,_), 
		((visitado(X,Y), write('.'),!); (certeza(X,Y), write('!'),!); write(' ')),
		write(' '), show_player(X,Y),
		((memory(X,Y,Z),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!);
		(write('   '),!)).

		
show_mem_position(X,Y) :- show_mem_info(X,Y),!.


show_mem :- map_size(_,MAX_Y), show_mem(1,MAX_Y),!.
show_mem(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_mem_position(X,Y), write('|'), XX is X + 1, show_mem(XX, Y),!.
show_mem(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_mem(1, YY),!.
show_mem(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.

capture_mem(String) :- with_output_to(string(String), show_mem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Código adicionado
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Outros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gerenciamento de bloqueio A*
add_blocked(X,Y) :- blocked(X,Y), !.
add_blocked(X,Y) :- assertz(blocked(X,Y)).

clear_blocked :- retractall(blocked(_,_)).

not_blocked(X,Y) :- \+ blocked(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "Definições" simples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dano máximo do monstro
max_monster_damage(50).   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Monstro CERTO numa posição
monster_cert_cell(X,Y) :-
    certeza(X,Y),
    memory(X,Y,L),
    member(passos,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bloco suspeito de monstro, considera sempre o pior caso  
monster_sus_cell(X,Y) :-
    memory(X,Y,L),
    member(passos, L),
    \+ member(palmas, L),
    \+ member(brisa, L),
    \+ certeza(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bloco suspeito de morcego, considera sempre o pior caso
bat_sus_cell(X,Y) :-
    memory(X,Y,L),
    member(palmas, L),
    \+ member(passos, L),
    \+ member(brisa, L),
    \+ certeza(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bloco suspeito de poço (brisa), considera sempre o pior caso
pit_sus_cell(X,Y) :-
    memory(X,Y,L),
    member(brisa, L),
    \+ certeza(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verifica se há ao menos um bloco suspeito de monstro (não certeza)
known_monster :-
    monster_sus_cell(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mapeia cada direção em um delta (DX,DY).
possible_dir(norte, 0,  1).
possible_dir(leste,  1,  0).
possible_dir(sul,    0, -1).
possible_dir(oeste, -1,  0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Retorna um index relacionado a direção
dir_index(norte, 0).
dir_index(leste, 1).
dir_index(sul,   2).
dir_index(oeste, 3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Seguro(X,Y) é verdade se o agente tem CERTEZA de que (X,Y) é um local seguro,
seguro(1,1).

seguro(X,Y) :-
    certeza(X,Y),
    memory(X,Y, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Diz quem é o bloco a frente do agente
% Recebe -> Local X,Y e direção do agente.
% Retorna -> Cordenadas do bloco a frente.
proximo(X,Y,norte,  X, Y1) :- Y1 is Y+1.
proximo(X,Y,sul,    X, Y1) :- Y1 is Y-1.
proximo(X,Y,leste,  X1, Y) :- X1 is X+1.
proximo(X,Y,oeste,  X1, Y) :- X1 is X-1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "Funções" auxiliares
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Identifica o caso específico: agente preso entre monstros suspeitos e um monstro certo
% Recebe -> 
% Retorna ->
trapped_monster_dir(DirM) :-
    posicao(X,Y,_),

    possible_dir(DirM,DXM,DYM),         % Existe um monstro certo em alguma direção
    MX is X+DXM, MY is Y+DYM,
    monster_cert_cell(MX,MY),
          
    map_size(MAX_X,MAX_Y),              % Todos os outros vizinhos (diferentes de DirM) são monstros suspeitos
    forall(
      ( possible_dir(Dir2,DX2,DY2),
        Dir2 \= DirM,
        NX is X+DX2, NY is Y+DY2,
        between(1,MAX_X,NX),
        between(1,MAX_Y,NY)
      ),
      (
      monster_sus_cell(NX,NY))
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Identifica o caso específico: agente preso entre poços/morcegos suspeitos e um monstro certo
% Recebe -> 
% Retorna -> Direção do mosntro
trapped_bat_pit_dir(DirM) :-
    posicao(X,Y,_),

    possible_dir(DirM,DXM,DYM),         % Existe um monstro certo em alguma direção
    MX is X+DXM, MY is Y+DYM,
    monster_cert_cell(MX,MY),
          
    map_size(MAX_X,MAX_Y),              % Todos os outros vizinhos (diferentes de DirM) são poços/morcegos suspeitos
    forall(
      ( possible_dir(Dir2,DX2,DY2),
        Dir2 \= DirM,
        NX is X+DX2, NY is Y+DY2,
        between(1,MAX_X,NX),
        between(1,MAX_Y,NY)
      ),
      ( pit_sus_cell(NX,NY)
      ; bat_sus_cell(NX,NY)
      )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Informa para qual direção o agente deve virar de acordo para onde ele quer ir
% Recebe -> Direção atual do agente
% Retorna -> virar a esquerda ou virar a direita
turn_action(DirAtual, DirAlvo, virar_direita) :-
    dir_index(DirAtual, I1),     % Pega index da direção do jogador
    dir_index(DirAlvo,  I2),     % Pega index da direção alvo
    D is (I2 - I1 + 4) mod 4,    % Diferença
    D =:= 1, !.                  % Se a diferença é 1, vire a direita

turn_action(DirAtual, DirAlvo, virar_esquerda) :-
    dir_index(DirAtual, I1),	% Pega index da direção do jogador
    dir_index(DirAlvo,  I2),	% Pega index da direção alvo
    D is (I2 - I1 + 4) mod 4, 	% Diferença
    D =:= 3, !.					% Se a diferença é 3, vire a esquerda

% Se for oposto (diferença 2), vira a direita por convenção
turn_action(_DirAtual, _DirAlvo, virar_direita).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Informa a direção de uma das coordenadas "válidas" (não visitada e sem obstáculo) em relação a um local
% Recebe -> Coordenadas do bloco de origem
% Retorna -> Uma das quatro direções válidas
valid_direction(X,Y,Dir) :-
    possible_dir(Dir,DX,DY),           % Para cada uma das possíveis direções
    NX is X + DX,
    NY is Y + DY,
    map_size(MAX_X,MAX_Y),             % Se está dentro dos limites do mapa
    between(1,MAX_X,NX),               
    between(1,MAX_Y,NY),
    memory(NX,NY,Percepts),	           % Se não há percepção de perigo
    Percepts = [],
    \+ visitado(NX,NY),                % Se não foi visitada ainda
    !.								   % Retorna a primeira válida

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verifica se o bloco tem pelo menos um vizinho não visitado e sem avisos
% Recebe -> Local X,Y do bloco.
% Retorna -> Falso ou verdadeiro.
has_safe_frontier(X,Y) :-
    member((DX,DY), [(1,0),(-1,0),(0,1),(0,-1)]),          % Para todos incluidos em blocos ao lado de (X,Y) - (soma/subtrai 1 x e y), como (NX, NY), cheque:
	NX is X + DX, NY is Y + DY,
    map_size(MAX_X,MAX_Y),								   % Range do mapa (12x12)
    between(1,MAX_X,NX), between(1,MAX_Y,NY),              % Se a cordenada adicionada estiver no range do mapa (entre 1 e 12) para x e y
    \+ visitado(NX,NY),									   % E se visitado(NX,NY) não existir (o bloco não foi visitado)
    memory(NX,NY,Percepts),	Percepts = [],				   % E se Percepts em memory(NX,NY) = Null ([], lista vazia) (não há suspeita de nada no bloco)
    !.		 											   % Retorna true imediatamente quando acha um válido

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verifica se um bloco visitado faz fronteira com um bloco suspeito de monstro
% Recebe -> VX, VY: coordenadas de um bloco que já foi visitado
% Retorna -> Dir: direção (norte, leste, sul ou oeste) que aponta do bloco (VX,VY) ao bloco onde há monstro
monster_frontier(VX,VY,Dir) :-  
    ( visitado(VX,VY) ; posicao(VX,VY,_) ),               % Ja foi visitado
    possible_dir(Dir,DX,DY),           % Pra cada direção (Dir) obtém (DX,DY) correspondente  
    MX is VX+DX,  MY is VY+DY,         % Calcula coordenadas (MX,MY) do bloco adjacente em direção Dir  
    monster_sus_cell(MX,MY),           % Verifica se é um bloco suspeito de monstro 
    seguro(VX,VY).                     % Garante que (VX,VY) é seguro

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bloco visitado que faz fronteira com um bloco suspeito de morcego
% Recebe -> VX,VY  bloco visitado e seguro
% Retorna -> Dir  direção que aponta para o morcego suspeito
bat_frontier(VX,VY,Dir) :-
    ( visitado(VX,VY) ; posicao(VX,VY,_) ),               % Já visitado
    possible_dir(Dir,DX,DY),            % Cada direção ortogonal
    MX is VX+DX, MY is VY+DY,           % (MX,MY) adjacente
    bat_sus_cell(MX,MY),                % É suspeita de morcego
    seguro(VX,VY).                      % (VX,VY) é seguro

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bloco visitado que faz fronteira com um bloco suspeito de poço
% Recebe -> VX,VY  bloco visitado e seguro
% Retorna -> Dir   direção que aponta para o poço suspeito
pit_frontier(VX,VY,Dir) :-
    ( visitado(VX,VY) ; posicao(VX,VY,_) ),                 % Já visitado
    possible_dir(Dir,DX,DY),            % Cada direção ortogonal
    MX is VX+DX, MY is VY+DY,           % (MX,MY) adjacente
    pit_sus_cell(MX,MY),                % É suspeita de poço
    seguro(VX,VY).                      % (VX,VY) é seguro

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "Funções" principais
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encontra o bloco “aberto” (já visitado) mais próximo de onde o agente está
% Recebe -> 
% Retorna -> Coordernadas do bloco "aberto" mais próximo

nearest_open(TX,TY,D) :-
    posicao(X0,Y0,_),						 % Acha a posição atual (posição é unica, sempre que se registra uma nova em andar se apaga a antiga logo so há uma)
    findall(								 % Acha todos 
     Dist-(VX,VY),                           % Monta uma lista de Dist-(VX,VY)
      ( visitado(VX,VY),				     % Onde VX, VY são os visitados (blocos armazenados em visitados ou simplesmente blocos já visitados)			
        Dist is abs(VX-X0) + abs(VY-Y0),     % Dist é a distancia delta X + delta Y (em relação a posição do jogador e o bloco visitado em questão)
        has_safe_frontier(VX,VY)		     % Que retornam verdadeiro em "has_safe_frontier" (ou seja tem um bloco liberado)
      ),									 
      Pairs),								 % Guarda em Pairs os que atendem a condição
    Pairs \= [],				             % Precisa ter ao menos um candidato
    keysort(Pairs, [D-(TX,TY)|_]).			 % Da keysort baseado na menor distância (ordena as chaves D de forma crescente) e corta todos os elementos depois do primeiro (é o retorno)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encontra o bloco da “fronteira de monstro” (visitado, seguro e adjacente a monstro)
% Recebe -> 
% Retorna -> Coordenadas (TX,TY) do bloco visitado mais próximo que faz fronteira com um bloco suspeito de monstro

nearest_monster_frontier(TX,TY,Dir,D) :-
    posicao(X0,Y0,_),                            % Obtém a posição atual do agente
    findall( Dist-(VX,VY,Dir1),                  % Para todo (x,y,direção)
             ( monster_frontier(VX,VY,Dir1),     % Blocos visitados e seguros com fronteira a suspeitas de monstro  
               Dist is abs(VX-X0)+abs(VY-Y0) ),  % Distância
             Pairs), Pairs \= [],                % Se houver algum
    keysort(Pairs,[D-(TX,TY,Dir)|_]).            % Filtra o com a menor distância   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encontra o bloco com poção mais próximo 
% Recebe -> 
% Retorna -> Coordenadas (TX,TY) coordenadas do bloco onde há poção 

nearest_potion(TX,TY,D) :-
    posicao(X0,Y0,_),                            % Obtém a posição atual do agente (X0,Y0)
    findall(
      Dist-(PX,PY),                              % Para cada par (PX,PY), calcula Dist seguido das coordenadas
      (
        memory(PX,PY,[reflexo]),                 % Bloco com percepção de poção (reflexo) na memória
        Dist is abs(PX-X0) + abs(PY-Y0)          % Calcula a distância  
      ),
      Pairs), Pairs \= [],                       % Se houver algum
    keysort(Pairs,[D-(TX,TY)|_]).                % Filtra o com a menor distância 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encontra o bat_frontier mais próximo do agente
% Recebe -> 
% Retorna -> TX,TY   bloco visitado seguro mais próximo

nearest_bat_frontier(TX,TY,Dir,D) :-                     
    posicao(X0,Y0,_),                                % Obtém a posição atual do agente (X0,Y0)    
    findall( Dist-(VX,VY,Dir1),                      % Para todo (x,y,direção)
             ( bat_frontier(VX,VY,Dir1),             % Blocos visitados e seguros com fronteira a suspeitas de morcego  
               Dist is abs(VX-X0)+abs(VY-Y0) ),      % Distância
             Pairs), Pairs \= [],                    % Se houver algum
    keysort(Pairs,[D-(TX,TY,Dir)|_]).                % Filtra o com a menor distância   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encontra o pit_frontier mais próximo do agente
% Recebe -> 
% Retorna -> TX,TY  bloco visitado seguro mais próximo

nearest_pit_frontier(TX,TY,Dir,D) :-                     
    posicao(X0,Y0,_),                                % posição atual
    findall( Dist-(VX,VY,Dir1),
             ( pit_frontier(VX,VY,Dir1),
               Dist is abs(VX-X0)+abs(VY-Y0) ),
             Pairs), Pairs \= [],
    keysort(Pairs,[D-(TX,TY,Dir)|_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ordem de execução de ações
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1- ao coletar 3 ouros, manda ir para o início (1,1) usando go_to (A*)
executa_acao(X) :-
    ouro(Qtd), 
    Qtd >= 3,
    X = go_to(1,1).

% 2- se estiver em cima de ouro, executa pegar
executa_acao(X) :-
    posicao(X0,Y0,_), 
    memory(X0,Y0,[brilho]),      
    !,
    X = pegar.

% 3- se está em cima de power-up e energia for 50 ou menos, pega o item
executa_acao(X) :-
    posicao(X0,Y0,_),
    memory(X0,Y0,[reflexo]),     
    energia(E),
    E =< 50,
    !,
    X = pegar.

% 4- anda para frente se não houver barreira e o próximo bloco não foi visitado
executa_acao(andar) :-
    posicao(X,Y,Dir),
    proximo(X,Y,Dir,NX,NY),
    memory(NX,NY,Percepts),
    Percepts = [],
    \+ visitado(NX,NY),
    !.

% 5- se houver espaço livre ao redor do jogador, gira para a direção desse local
executa_acao(X) :-
    posicao(XN,YN,DirAtual),            
    valid_direction(XN,YN,DirAlvo),     
    turn_action(DirAtual, DirAlvo, X), 
    !.

% 6- busca o bloco aberto mais próximo e usa go_to (A*), só se não estiver já lá
executa_acao(X) :-
    posicao(CX, CY, _),
    nearest_open(TX, TY, _),
    not_blocked(TX,TY),
    (CX \= TX ; CY \= TY),          
    X = go_to(TX, TY),
    !.

% 7- se estiver cercado, tenta passar por monstros suspeitos se tiver energia suficiente
executa_acao(X) :-
    max_monster_damage(MaxD),                   % verifica dano máximo do monstro
    energia(E), E > MaxD,                       % só se energia for maior que o dano
    nearest_monster_frontier(TX,TY,DirM,_),     % acha o bloco suspeito de monstro mais próximo
    not_blocked(TX,TY),
    posicao(CX,CY,DirNow),                      % posição e direção atuais
    (   (CX \= TX ; CY \= TY)                   % se não está no bloco alvo
    ->  X = go_to(TX,TY)                        % vai até o bloco
    ;   ( DirNow \= DirM                        % se não está olhando para o monstro
        -> turn_action(DirNow,DirM,X)           % gira para o monstro
        ;  X = andar)                           % se já está olhando, anda
    ),
    !.

% 8- se não tem energia para passar por monstros, procura poção conhecida
executa_acao(X) :-
    max_monster_damage(MaxD),   % pega o dano máximo do monstro
    energia(E), E =< MaxD,      % se energia for menor ou igual ao dano
    known_monster,              % existe monstro suspeito
    nearest_potion(TX,TY,_),    % existe poção conhecida
    not_blocked(TX,TY),    
    X = go_to(TX,TY),
    !.

% 9- tenta chegar até o morcego suspeito mais próximo
executa_acao(X) :-
    nearest_bat_frontier(TX,TY,DirB,_),    % encontra fronteira de morcego suspeito
    not_blocked(TX,TY), 
    posicao(CX,CY,DirNow),                 % posição e direção do jogador
    (   (CX \= TX ; CY \= TY)              % se não está no bloco alvo
    ->  X = go_to(TX,TY)                   % vai até o bloco
    ;   ( DirNow \= DirB                   % se não está olhando para o morcego
        -> turn_action(DirNow,DirB,X)      % gira para o morcego
        ;  X = andar)                      % se já está olhando, anda
    ),
    !.

% 10 – se estiver preso entre monstros suspeitos e um monstro confirmado atrás, anda
executa_acao(X) :-
    trapped_monster_dir(DirM),          % verifica se está nessa situação
    X = andar,                          % anda para frente
    !.

% 11 – se estiver encurralado entre poços/morcegos suspeitos e monstro certo atrás, gira ou anda
executa_acao(X) :-
    trapped_bat_pit_dir(DirM),        % verifica se está nessa situação
    max_monster_damage(MaxD),
    energia(E),                       % verifica energia
    E  > MaxD,                       
    posicao(_,_,DirNow),
    ( DirNow \= DirM                  % se não está virado para o monstro
    -> turn_action(DirNow,DirM,X)     % gira para ele
    ;  X = andar                      % se já está virado, anda
    ),
    !.

% 12 – se nada acima funcionar, vai para o poço suspeito mais próximo
executa_acao(X) :-
    nearest_pit_frontier(TX,TY,DirP,_),        % encontra fronteira de poço suspeito
    not_blocked(TX,TY), 
    posicao(CX,CY,DirNow),
    (   (CX \= TX ; CY \= TY)                  % se ainda não chegou lá
    ->  X = go_to(TX,TY)                       % vai até o local
    ;   ( DirNow \= DirP                       % se não está virado para o poço
        -> turn_action(DirNow,DirP,X)          % gira para o poço
        ;  X = andar)                          % se já está virado, avança
    ).