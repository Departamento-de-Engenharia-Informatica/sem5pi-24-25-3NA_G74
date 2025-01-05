%Instruções: Activar os writes em inicializar para escolher condições de seleção e dimensão dos dados.
%Usar generate. para correr o algoritmo.
%Variar as condições de paragem caso desejado, nº gerações, tempo, valor otimo e consistencia.
	
:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.
:-dynamic solution_found/1.
:-dynamic tasks/1.
:-dynamic surgery_id/3.
:-dynamic best_solution_storage/1.
:-dynamic agenda_staff2/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(1380,1440,c02)]).
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).
agenda_staff(da001,20241028,[]).
agenda_staff(da002,20241028,[]).
agenda_staff(in001,20241028,[]).
agenda_staff(cn001,20241028,[]).
agenda_staff(an001,20241028,[]).
agenda_staff(maa001,20241028,[]).
agenda_staff(in002,20241028,[]).
agenda_staff(cn002,20241028,[]).
agenda_staff(an002,20241028,[]).
agenda_staff(maa002,20241028,[]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
timetable(da001,20241028,(480,1200)).
timetable(da002,20241028,(500,1440)).
timetable(in001,20241028,(520,1320)).
timetable(cn001,20241028,(480,1200)).
timetable(an001,20241028,(500,1440)).
timetable(maa001,20241028,(520,1320)).
timetable(in002,20241028,(480,1200)).
timetable(cn002,20241028,(500,1440)).
timetable(an002,20241028,(520,1320)).
timetable(maa002,20241028,(520,1320)).

staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003,doctor,orthopaedist,[so2,so3,so4]).
staff(da001,doctor,anaesthetist,[so2,so3,so4]).
staff(da002,doctor,anaesthetist,[so2,so3,so4]).
staff(in001,staff,instrumentingnurse,[so2,so3,so4]).
staff(cn001,staff,circulatingnurse,[so2,so3,so4]).
staff(an001,staff,anaesthetistnurse,[so2,so3,so4]).
staff(maa001,staff,medicalactionassistant,[so2,so3,so4]).
staff(in002,staff,instrumentingnurse,[so2,so3,so4]).
staff(cn002,staff,circulatingnurse,[so2,so3,so4]).
staff(an002,staff,anaesthetistnurse,[so2,so3,so4]).
staff(maa002,staff,medicalactionassistant,[so2,so3,so4]).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_to_schedule(so100001,so2).
surgery_to_schedule(so100002,so3).
surgery_to_schedule(so100003,so4).
surgery_to_schedule(so100004,so2).
surgery_to_schedule(so100005,so4).
surgery_to_schedule(so100006,so4).
surgery_to_schedule(so100007,so4).

assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).
assignment_surgery(so100005,d003).
assignment_surgery(so100006,d002).
assignment_surgery(so100007,d003).

agenda_operation_room(or1,20241028,[(520,579,so100000),(1000,1059,so099999)]).
agenda_operation_room(or2,20241028,[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado principal para atribuir as salas às cirurgias
assign_surgery_room:-
    % Lista todas as salas disponíveis
    findall(Room, agenda_operation_room(Room, _, _), Rooms),
    % Recupera todas as cirurgias a serem agendadas
    findall(Surgery, surgery_to_schedule(Surgery,_),Surgeries),
    findall(Type, surgery_to_schedule(_,Type),Types),
    % Atribui as salas às cirurgias de forma sequencial
    assign_rooms_to_surgeries(Surgeries,Types,Rooms,0).

% Atribui as salas às cirurgias usando um índice
assign_rooms_to_surgeries([],_,_,_). % Caso base: sem cirurgias para agendar
assign_rooms_to_surgeries([Surgery|Rest],[Type|RestTypes],Rooms,Index) :-
    % Calcula o índice da sala atual (cíclico)
    length(Rooms, NumRooms),
    RoomIndex is Index mod NumRooms,
    nth0(RoomIndex, Rooms, AssignedRoom),
    assertz(surgery_id(Surgery,Type,AssignedRoom)), 
    % Passa para a próxima cirurgia e incrementa o índice
    NextIndex is Index + 1,
    assign_rooms_to_surgeries(Rest,RestTypes,Rooms,NextIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Começo do algoritmo:
startGA(Day):-
    findall(Room,agenda_operation_room(Room,_,_),Rooms),
    retractall(surgery_id(_,_,_)),
    retractall(best_solution_storage(_)),
    assign_surgery_room,
    run(Rooms,Day),
    findall(Solution, best_solution_storage(Solution), BestSolutions),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    retractall(agenda_staff2(_,_,_)),
    definitive_schedule(BestSolutions,Day,Rooms),
    write("\nAgendas do staff:\n"),
    findall(Staff, agenda_staff2(Staff, Day, _), Staffs),
    sort(Staffs,SortedStaff),
        forall(member(Staff, Staffs),
           (agenda_staff2(Staff, Day, AgendaStaffs),
            format("  Staff ~w: ~w~n", [Staff, AgendaStaffs]))
    ).

definitive_schedule([],_,_):-!.
definitive_schedule([LOpCode|Rest],Day,[Room|RestRooms]):-
    retractall(agenda_operation_room1(_,_,_)),
    schedule_all_surgeries(Room,Day,LOpCode,TFin),
    format("Sala de Operações ~w~n", [Room]),
    (   agenda_operation_room1(Room, Day, AgendaRoom),
        AgendaRoom \= [] ->
        format("  Agenda para sala ~w no dia ~w: ~w~n", [Room, Day, AgendaRoom])
    ;   write("  Nenhuma cirurgia agendada.\n")
    ),
    findall(Staff, agenda_staff1(Staff, Day, _), Staffs),
    forall(
        member(Staff, Staffs),
        (
            (retract(agenda_staff1(Staff, Day, Agenda)) -> true; Agenda = []),
            (retract(agenda_staff2(Staff, Day, Agenda1)) -> true; Agenda1 = []),
            append(Agenda, Agenda1, TotalAgenda),
            assertz(agenda_staff2(Staff, Day, TotalAgenda))
        )
    ),
    definitive_schedule(Rest,Day,RestRooms).


run([],_):-!.
run([Room|Rest],Day):-
    generate(Room,Day,BestSolution),
    % Extrair apenas a parte relevante da solução.
    extract_solution(BestSolution, CleanSolution),
    % Adicionar a solução à variável dinâmica.
    assertz(best_solution_storage(CleanSolution)),
    run(Rest,Day).

% Extrai apenas a lista de termos do formato [so100002, so100006, so100004]*1125.
extract_solution(Solution*_, Solution) :- !. % Descarta tudo após o '*'.

%Ler e organizar os parametros iniciais.
% parameters initialization
initialize:-
    (retract(generations(5));true), asserta(generations(5)),
	(retract(population(5));true), asserta(population(5)),
    PC is 50/100,
	(retract(prob_crossover(50));true), asserta(prob_crossover(PC)),
    PM is 50/100,
	(retract(prob_mutation(50));true), asserta(prob_mutation(PM)).

generate(Room,Day,BestSolution):-
    initialize,
    %Gera populaçao inicial de individuos
    findall(OpCode,surgery_id(OpCode,_,Room),LOpCode),
    length(LOpCode, NumT),
    assertz(tasks(NumT)),
    generate_population(Room,Pop),
    evaluate_population(Room,Day,Pop,PopValue),
    order_population(PopValue,PopOrd),
    generations(NG),
    get_time(StartTime),
   
    % Variável para controlar se uma solução foi encontrada
    assertz(solution_found(false)),
   
    catch(
        generate_generation(Room, Day, StartTime, 0, NG, PopOrd, _),
        best_solution_reached(BestSolution),
        (
            % Se a melhor solução for encontrada, armazena e altera a flag
            write('Best solution reached: '), write(BestSolution), nl,
            retract(solution_found(false)),
            assertz(solution_found(true))
        )
    ),
   
    % Verifica se a solução foi encontrada
    solution_found(SolutionFound),
    (
        % Se a solução foi encontrada durante as gerações
        SolutionFound = true
    ->
        % Não exibe a mensagem "Completed all generations"
        
       	true
    ;
        % Se não encontrou a solução durante as gerações, exibe a mensagem de conclusão
        write('Completed all generations, final solution: '), nl,
        order_population(PopOrd,PopOrdShow),
        PopOrdShow = [BestSolution|_],
        write(BestSolution), nl
    ),
        %format("Sala de Operações ~w~n", [Room]),
    (   agenda_operation_room1(Room, Day, AgendaRoom),
        AgendaRoom \= [] ->
        format("  Agenda para sala ~w no dia ~w: ~w~n", [Room, Day, AgendaRoom])
    ;   write("  Nenhuma cirurgia agendada.\n")
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_population(Room,Pop):-
    %Tamanho da populaçao
    population(PopSize),
    findall(OpCode,surgery_id(OpCode,_,Room),LOpCode),
    length(LOpCode, NumT),
    generate_population(PopSize,LOpCode,NumT,Pop).

generate_population(0,_,_,[]):-!.
generate_population(PopSize,LOpCode,NumT,[Ind|Rest]):-
    PopSize1 is PopSize-1,
    generate_population(PopSize1,LOpCode,NumT,Rest),
    %Gera individuo
    generate_individual(LOpCode,NumT,Ind),
    %Verifica se não é duplicado
    not(member(Ind,Rest)).
generate_population(PopSize,LOpCode,NumT,L):-
    generate_population(PopSize,LOpCode,NumT,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%isto equivale a gerar os cromossomas
generate_individual([G],1,[G]):-!.
generate_individual(TasksList,NumT,[G|Rest]):-
    NumTemp is NumT + 1, % to use with random
    %Gera tarefa aleatoriamente
    random(1,NumTemp,N),
    %Remove essa tarefa para nao haver duplicados
    remove(N,TasksList,G,NewList),
    NumT1 is NumT-1,
    generate_individual(NewList,NumT1,Rest).
remove(1,[G|Rest],G,Rest).
remove(N,[G1|Rest],G,[G1|Rest1]):- N1 is N-1,
            remove(N1,Rest,G,Rest1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate_population(_,_,[],[]).
evaluate_population(Room,Day,[Ind|Rest],[Ind*TFin|EvaluatedRest]) :-
    schedule_all_surgeries(Room,Day,Ind,TFin),
    evaluate_population(Room,Day,Rest,EvaluatedRest).

%Ordena populaçao do mais forte para o mais fraco(numero menor melhor resultado)
order_population(PopValue,PopValueOrd):-
    bsort(PopValue,PopValueOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
    bsort(Xs,Zs),
    bchange([X|Zs],Ys).

bchange([X],[X]):-!.
bchange([X*VX,Y*VY|L1],[Y*VY|L2]):-
    VX>VY,!,
    bchange([X*VX|L1],L2).
bchange([X|L1],[X|L2]):-bchange(L1,L2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Aqui é gerado as proximas gerações com possibilidade de crossover e mutation.
generate_generation(_,_,_,G,G,Pop,_):-!.
%write('Generation '), write(G), write(':'), nl, write(Pop), nl.
generate_generation(Room,Day,StartTime,N,G,Pop,StableCount):-
    population(PopSize),
    order_population(Pop,PopOrdShow),
%write('Generation '), write(N), write(':'), nl, write(PopOrdShow), nl,
    %%%%%%%%%%%%%%%%%%%%%%%
    % Paragem por tempo
    get_time(CurrentTime),
    TotalTime is CurrentTime - StartTime,
    % Check if the time limit is exceeded
    (   TotalTime > 1000
    ->  write('Time limit reached! Terminating...'), nl,
        PopOrdShow = [BestSolution|_],
        throw(best_solution_reached(BestSolution))
    ;   true
    ),
    (  
    PopOrdShow = [_*FirstIndValue | _],
    FirstIndValue < 940
    ->  write('Specific evaluation reached! Terminating...'), nl,
        PopOrdShow = [BestSolution|_],
        throw(best_solution_reached(BestSolution))
    ;   true
    ),
    %%%%%%%%%%%%%%%%%%%%%%%%
    %Randomiza a lista para quando ocorrer crossover nao ser sempre entre os mesmos
    %Individuos
    random_permutation(Pop,RPop),
	crossover(RPop,NPop1),
	mutation(NPop1,NPop),
    %write('NPop'),write(NPop),nl,
	evaluate_population(Room,Day,NPop,NPopValue),
	order_population(NPopValue,NPopOrd),
    non_elitist_selection(Pop,NPopOrd,FilteredPop,Best),
    replace_worst_in_pop(FilteredPop,Pop,PopSize,NewPop),
    replace_worst(NewPop,Best,Result),
	N1 is N+1,
    % Verifica estabilidade da população
    (   Result = Pop
    ->  NewStableCount is StableCount + 1
    ;   NewStableCount is 0
    ),
   
    % Se a população estiver estável por 10 gerações, interrompe
    (   NewStableCount >= 10
    ->  write('Population stable! Terminating...'), nl,
        PopOrdShow = [BestSolution|_],
        throw(best_solution_reached(BestSolution))
    ;   true
    ),
	generate_generation(Room,Day,StartTime,N1,G,Result,NewStableCount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_worst_in_pop(FilteredPop, Pop, PopSize, NewPop) :-
    % Filtrar os elementos de FilteredPop que já existem em Pop
    exclude(already_in_pop(Pop), FilteredPop, UniqueFilteredPop),
    % Combinar os indivíduos de FilteredPop (apenas os únicos) com Pop
    append(UniqueFilteredPop, Pop, CombinedPop),
    % Garantir o tamanho máximo da população e manter os melhores
    sort(CombinedPop, SortedPop),
    length(NewPop, PopSize),
    append(NewPop, _, SortedPop).

% Verifica se um indivíduo já está na população
already_in_pop(Pop, Individual) :-
    member(Individual, Pop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Seleçao nao elitista
% Predicado para combinar populações
merge_populations(CurrentPop, NewPop, CombinedPop) :-
    append(CurrentPop, NewPop, CombinedPop).
% Predicado para remover duplicados
remove_duplicates(Pop, UniquePop) :-
    sort(Pop, UniquePop).
% Seleção não elitista
non_elitist_selection(CurrentPop, NewPop, TopP,Best) :-
    merge_populations(CurrentPop, NewPop, CombinedPop),
    remove_duplicates(CombinedPop, FilteredPop),
    order_population(FilteredPop,OrderedFPop),
    select_top_20_percent(OrderedFPop,TopP,Remaining),
    apply_random_factor(Remaining,Result),
    order_population3(Result,Sorted),
    find_best_individual(Sorted,BestZ),
    remove_Z(BestZ,Best).


% Selecionar os Top P indivíduos, onde P é 20% do tamanho da lista
select_top_20_percent(Population, TopP,Remaining) :-
    length(Population, N),          
    P is max(1, ceiling(0.2 * N)),
    length(TopP, P),                
    append(TopP, Remaining, Population).
   
% Aplica um fator aleatório a cada elemento da lista Remaining.
apply_random_factor([], []).
apply_random_factor([Remaining*Y | Tail], [Remaining*Y*Z | ResultTail]) :-
    random(0.0, 1.0, Random),
    Z is Y * Random,  
    apply_random_factor(Tail, ResultTail).

% Ordena a população com base no valor Z (terceiro valor) do mais fraco para o mais forte (número menor é o melhor resultado).
order_population3(PopValue, PopValueOrd) :-
    bsort3(PopValue, PopValueOrd).

bsort3([X], [X]) :- !.
bsort3([X|Xs], Ys) :-
    bsort3(Xs, Zs),
    bchange3([X|Zs], Ys).

% Função auxiliar para comparar os valores Z
bchange3([X], [X]) :- !.
bchange3([X*VX*VZ, Y*VY*VZ2 | L1], [Y*VY*VZ2 | L2]) :-
    VZ > VZ2, !,  % Compara o valor Z
    bchange3([X*VX*VZ | L1], L2).
bchange3([X*VX*VZ | L1], [X*VX*VZ | L2]) :-
    bchange3(L1, L2).

% Encontrar o melhor indivíduo (apenas [Indiv, Eval], sem o Product)
find_best_individual([R | _], R).

remove_Z(Item*Y*Z, Item*Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_worst(Pop, BestInd, Result) :-
    % Verificar se o BestInd já está em Pop
    (   member(BestInd, Pop)
    ->  % Se já está, não há substituição: Result é igual a Pop
        Result = Pop
    ;   % Caso contrário, substituímos o pior indivíduo
        append(Front, [_WorstInd], Pop),
        append(Front, [BestInd], Result)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Gera os crossovers
crossover([ ],[ ]).
crossover([Ind*_],[Ind]).
crossover([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]):-
generate_crossover_points(P1,P2),
prob_crossover(Pcruz),random(0.0,1.0,Pc),
((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
 cross(Ind2,Ind1,P1,P2,NInd2))
;
(NInd1=Ind1,NInd2=Ind2)),
crossover(Rest,Rest1).


generate_crossover_points(P1,P2):- generate_crossover_points1(P1,P2).
generate_crossover_points1(P1,P2):-
tasks(N),
NTemp is N+1,
random(1,NTemp,P11),
random(1,NTemp,P21),
P11\==P21,!,
((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
generate_crossover_points1(P1,P2):-
generate_crossover_points1(P1,P2).

fillh([ ],[ ]).

fillh([_|R1],[h|R2]):-
fillh(R1,R2).

sublist(L1,I1,I2,L):-I1 < I2,!,
    sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!, fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
N4 is N2 - 1,
sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):- tasks(N),
T is N - K,
rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):- N1 is N - 1,
append(R,[X],R1),
rr(N1,R1,R2).

remove([],_,[]):-!.

remove([X|R1],L,[X|R2]):- not(member(X,L)),!,
        remove(R1,L,R2).

remove([_|R1],L,R2):-
    remove(R1,L,R2).

insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
    tasks(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insert1(X,N1,L,L1),
    N2 is N + 1,
    insert(R,L1,N2,L2).


insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insert1(X,N1,L,L1).

cross(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
    tasks(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    remove(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insert(Sub2,Sub1,P3,NInd1),
    removeh(NInd1,NInd11).


removeh([],[]).

removeh([h|R1],R2):-!,
    removeh(R1,R2).

removeh([X|R1],[X|R2]):-
    removeh(R1,R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Gera mutação
mutation([],[]).
mutation([Ind|Rest],[NInd|Rest1]):-
prob_mutation(Pmut),
random(0.0,1.0,Pm),
((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
mutation(Rest,Rest1).

mutacao1(Ind,NInd):-
generate_crossover_points(P1,P2),
mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
!, P21 is P2-1,
mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
P11 is P1-1, P21 is P2-1,
mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
P1 is P-1,
mutacao23(G1,P1,Ind,G2,NInd).



free_agenda0([],[(0,1440)]).
free_agenda0([(0,Tfin,_)|LT],LT1):-!,free_agenda1([(0,Tfin,_)|LT],LT1).
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT],LT1).

free_agenda1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda1([(_,_,_)],[]).
free_agenda1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda1([(T1,Tfin2,_)|LT],LT1).
free_agenda1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT],LT1).


adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).


intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):- intersect_availability(D,LA,LI,LA1),
intersect_2_agendas(LD,LA1,LID),
append(LI,LID,LIT).

intersect_availability((_,_),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
Ini>Fim1,!,
intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
Fim1>Fim,!,
min_max(Ini,Ini1,_,Imax),
min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
Fim>=Fim1,!,
min_max(Ini,Ini1,_,Imax),
min_max(Fim,Fim1,Fmin,_),
intersect_availability((Fim1,Fim),LD,LI,LA).


min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).

calculate_free_time([], 0).  % Caso base: se a lista estiver vazia, o tempo livre é 0
calculate_free_time([(In, Fin)|LT], TotalFreeTime) :-
    % Para cada intervalo (In, Fin), calcula a diferença (Fin - In) e soma recursivamente
    FreeTime is Fin - In,
    calculate_free_time(LT, RemainingTime),
    TotalFreeTime is RemainingTime + FreeTime.

get_total_time_free(D,Date,TotalFreeTime):-
    agenda_staff(D,Date,L),
free_agenda0(L,LFA),
    adapt_timetable(D,Date,LFA,LFA2),
    calculate_free_time(LFA2,TotalFreeTime).

calculate_working_hours(D, Date, R) :-
    timetable(D, Date, (InTime, FinTime)),
    R is FinTime - InTime.

staff_occupation_percentage(D,Date,R):-
    get_total_time_free(D,Date,TFT),
    calculate_working_hours(D,Date,WH),
    R is (TFT/WH) * 100.

% Caso base: lista vazia retorna resultado vazio.
all_staff_occupation(_, [], []).
% Caso recursivo: calcula a ocupação para o primeiro membro e continua com o restante.
all_staff_occupation(Date, [Staff | RestStaff], [(Staff, OccupationPercent) | RestResult]) :-
    staff_occupation_percentage(Staff, Date, OccupationPercent),
    all_staff_occupation(Date, RestStaff, RestResult).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ordena os médicos pela ocupação em ordem decrescente
sort_staff_by_occupation(RestResult, SortedStaff) :-
    % Ordena a lista de médicos pela ocupação (em ordem decrescente)
    sort(2, @=<, RestResult, SortedStaffWithOccupation),
    % Extrai apenas os médicos da lista ordenada
    findall(Staff, member((Staff, _), SortedStaffWithOccupation), SortedStaff).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all_anaesthologist_doctors(LADoctors):-
    findall(ADoctor,staff(ADoctor,doctor,anaesthetist,_),LADoctors).
get_all_anaesthologist_nurses(LANurses):-
    findall(ANurse,staff(ANurse,staff,anaesthetistnurse,_),LANurses).
get_all_instrumenting_nurses(LINurses):-
    findall(INurse,staff(INurse,staff,instrumentingnurse,_),LINurses).
get_all_circulating_nurses(LCNurses):-
    findall(CNurse,staff(CNurse,staff,circulatingnurse,_),LCNurses).
get_all_medicalactionassistant_staff(LMAAs):-
    findall(MAA,staff(MAA,staff,medicalactionassistant,_),LMAAs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_time(TAnaes,TSurgery,TTotal):-
    TTotal is TAnaes+TSurgery.

% Seleciona o primeiro elemento e move-o para o final da lista
select_element([Head|Tail], Selected, Result) :-
    Selected = Head,                % Seleciona o primeiro elemento
    append(Tail, [Head], Result).    % Coloca o Head no final da lista.


schedule_best_solution(Room,Day,LOpCode):-
	findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
	get_all_anaesthologist_doctors(LADoctors),
    all_staff_occupation(Day,LADoctors,Result1),
    sort_staff_by_occupation(Result1,SortedStaff1),
    get_all_anaesthologist_nurses(LANurses),
    all_staff_occupation(Day,LANurses,Result2),
    sort_staff_by_occupation(Result2,SortedStaff2),
    get_all_medicalactionassistant_staff(LMAAs),
    all_staff_occupation(Day,LMAAs,Result3),
    sort_staff_by_occupation(Result3,SortedStaff3),
    get_all_instrumenting_nurses(LINurses),
    all_staff_occupation(Day,LINurses,Result4),
    sort_staff_by_occupation(Result4,SortedStaff4),
    get_all_circulating_nurses(LCNurses),
    all_staff_occupation(Day,LCNurses,Result5),
    sort_staff_by_occupation(Result5,SortedStaff5),
    availability_all_surgeries(SortedStaff1,SortedStaff2,SortedStaff3,SortedStaff4,SortedStaff5,LOpCode,Room,Day).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule_all_surgeries(Room,Day,LOpCode,FinTime1):-
get_time(Ti),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    %findall(OpCode,surgery_id(OpCode,_,Room),LOpCode),
	get_all_anaesthologist_doctors(LADoctors),
    all_staff_occupation(Day,LADoctors,Result1),
    sort_staff_by_occupation(Result1,SortedStaff1),
    get_all_anaesthologist_nurses(LANurses),
    all_staff_occupation(Day,LANurses,Result2),
    sort_staff_by_occupation(Result2,SortedStaff2),
    get_all_medicalactionassistant_staff(LMAAs),
    all_staff_occupation(Day,LMAAs,Result3),
    sort_staff_by_occupation(Result3,SortedStaff3),
    get_all_instrumenting_nurses(LINurses),
    all_staff_occupation(Day,LINurses,Result4),
    sort_staff_by_occupation(Result4,SortedStaff4),
    get_all_circulating_nurses(LCNurses),
    all_staff_occupation(Day,LCNurses,Result5),
    sort_staff_by_occupation(Result5,SortedStaff5),
    availability_all_surgeries(SortedStaff1,SortedStaff2,SortedStaff3,SortedStaff4,SortedStaff5,LOpCode,Room,Day),!,
    %format([Room]),
    %(  
	%	agenda_operation_room1(Room, Day, AgendaRoom),
    %    AgendaRoom \= [] -> format([Room, Day, AgendaRoom])
    %    ;true
    %),
%Exportaçao dos resultados para consola.
    %write("Resultados de agendamento para o dia "), write(Day), write(":\n\n"),
    %format("Sala de Operações ~w~n", [Room]),
    agenda_operation_room1(Room, Day, AgendaRoom),
    %(   agenda_operation_room1(Room, Day, AgendaRoom),
    %    AgendaRoom \= [] ->
    %    true
    %;   true
    %),
    %format("Sala de Operações ~w~n", [Room]),
    %(   agenda_operation_room1(Room, Day, AgendaRoom),
    %    AgendaRoom \= [] ->
    %    format("  Agenda para sala ~w no dia ~w: ~w~n", [Room, Day, AgendaRoom])
    %;   write("  Nenhuma cirurgia agendada.\n")
    %),
    %write("\nAgendas do staff:\n"),
    %findall(Doctor, agenda_staff1(Doctor, Day, _), Doctors),
    %forall(member(Doctor, Doctors),
    %       (agenda_staff1(Doctor, Day, AgendaDoc),
    %        format("  Staff ~w: ~w~n", [Doctor, AgendaDoc]))
    %),
%get_time(Tf),
%T is Tf-Ti,
    %write('Execution Time: '),write(T),nl,
    reverse(AgendaRoom,AgendaR),
    evaluate_final_time(AgendaR,LOpCode,FinTime1).



availability_all_surgeries(_,_,_,_,_,[],_,_).
availability_all_surgeries(SortedStaff1,SortedStaff2,SortedStaff3,SortedStaff4,SortedStaff5,[OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType,Room),
    surgery(OpType,TAnaes,TSurgery,TClean),
    %write('OpCode'),write(OpCode),nl,
    total_time(TAnaes,TSurgery,TATotal),
    total_time(TATotal,TClean,TTotal),
    select_element(SortedStaff1,AD,SortedStaff10),
    %write(AD),
    select_element(SortedStaff2,AN,SortedStaff20),
    %write(AN),
    select_element(SortedStaff3,MAA,SortedStaff30),
    %write(MAA),
    select_element(SortedStaff4,IN,SortedStaff40),
    %write(IN),
    select_element(SortedStaff5,CN,SortedStaff50),

    %write(CN),
    %write(OpCode),
	findall(Doctor, assignment_surgery(OpCode, Doctor), LDoctors),
    %write('aqui'),
	append([[AD],[AN], [IN], [CN], LDoctors, [MAA]], TotalStaff),
	intersect_all_agendas(TotalStaff, Day, ATotalStaff),
	%write(TotalStaff),
    %write(ATotalStaff),
    agenda_operation_room1(Room,Day,LAgenda),
    %write(LAgenda),
    free_agenda0(LAgenda,LFAgRoom),
    intersect_2_agendas(LFAgRoom,ATotalStaff,GlobalAgenda),
    remove_unf_intervals(TTotal,GlobalAgenda,LPossibilities),
    (
        LPossibilities \= [] ->
        (
            schedule_first_interval(TTotal, LPossibilities, (TinS, TfinS)),
            TfinAnae is TinS + TATotal,
            TinDoc is TinS + TAnaes,
            TfinDoc is TinDoc + TSurgery,
            TinMAA is TinS + TATotal,

            retract(agenda_operation_room1(Room, Day, Agenda)),
            insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),
            assertz(agenda_operation_room1(Room, Day, Agenda1)),
            insert_agenda_doctors((TinDoc, TfinDoc, OpCode), Day, LDoctors),
            insert_agenda_staff((TinS, TfinAnae, OpCode), Day, AD),
            insert_agenda_staff((TinS, TfinAnae, OpCode), Day, AN),
            insert_agenda_staff((TinMAA, TfinS, OpCode), Day, MAA),
            insert_agenda_staff((TinDoc, TfinDoc, OpCode), Day, IN),
            insert_agenda_staff((TinDoc, TfinDoc, OpCode), Day, CN)
            %write('Cirurgia marcada.')
        )
        ;
        true
    ),

    availability_all_surgeries(SortedStaff10,SortedStaff20,SortedStaff30,SortedStaff40,SortedStaff50,LOpCode, Room, Day).


insert_agenda_staff((TinS,TfinS,OpCode),Day,Staff):-
    retract(agenda_staff1(Staff,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Staff,Day,Agenda1)).

remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).


schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).



obtain_better_sol(Room,Day,AgOpRoomBetter,LAgDoctorsBetter,TFinOp):-
get_time(Ti),
(obtain_better_sol1(Room,Day);true),
retract(better_sol(Day,Room,AgOpRoomBetter,LAgDoctorsBetter,TFinOp)),
            write('Final Result: AgOpRoomBetter='),write(AgOpRoomBetter),nl,
            write('LAgDoctorsBetter='),write(LAgDoctorsBetter),nl,
            write('TFinOp='),write(TFinOp),nl,
get_time(Tf),
T is Tf-Ti,
write('Tempo de geracao da solucao:'),write(T),nl.


obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_,Room),LOC),!,

    permutation(LOC,LOpCode),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(_,(agenda_staff1(D,Day,L),free_agenda0(L,LFA),adapt_timetable(D,Day,LFA,LFA2),assertz(availability(D,Day,LFA2))),_),
    availability_all_surgeries(LOpCode,Room,Day),
    agenda_operation_room1(Room,Day,AgendaR),
update_better_sol(Day,Room,AgendaR,LOpCode),
fail.

update_better_sol(Day,Room,Agenda,LOpCode):-
                better_sol(Day,Room,_,_,FinTime),
                reverse(Agenda,AgendaR),
                evaluate_final_time(AgendaR,LOpCode,FinTime1),
             write('Analysing for LOpCode='),write(LOpCode),nl,
             write('now: FinTime1='),write(FinTime1),write(' Agenda='),write(Agenda),nl,
FinTime1<FinTime,
             write('best solution updated'),nl,
                retract(better_sol(_,_,_,_,_)),
                findall(Doctor,assignment_surgery(_,Doctor),LDoctors1),
                remove_equals(LDoctors1,LDoctors),
                list_doctors_agenda(Day,LDoctors,LDAgendas),
asserta(better_sol(Day,Room,Agenda,LDAgendas,FinTime1)).

evaluate_final_time([],_,1441).
evaluate_final_time([(_,Tfin,OpCode)|_],LOpCode,Tfin):-member(OpCode,LOpCode),!.
evaluate_final_time([_|AgR],LOpCode,Tfin):-evaluate_final_time(AgR,LOpCode,Tfin).

list_doctors_agenda(_,[],[]).
list_doctors_agenda(Day,[D|LD],[(D,AgD)|LAgD]):-agenda_staff1(D,Day,AgD),list_doctors_agenda(Day,LD,LAgD).

remove_equals([],[]).
remove_equals([X|L],L1):-member(X,L),!,remove_equals(L,L1).
remove_equals([X|L],[X|L1]):-remove_equals(L,L1).



% Base case: if either list is empty, the percentage is 0.
evaluate_final_schedule([], _, 0).
evaluate_final_schedule(_, [], 0).
evaluate_final_schedule(AgendaRoom, LOpCode, RoundedPercentage) :-
    % Count surgeries in LOpCode that are scheduled in AgendaRoom
    include(member_in_agenda(AgendaRoom), LOpCode, ScheduledSurgeries),
    length(ScheduledSurgeries, ScheduledCount),
    length(LOpCode, TotalCount),
    TotalCount > 0, % Avoid division by zero
    Percentage is (ScheduledCount / TotalCount) * 100,
    RoundedPercentage is round(Percentage).

% Helper predicate: check if an OpCode is in the AgendaRoom
member_in_agenda(AgendaRoom, OpCode) :-
    member((_, _, OpCode), AgendaRoom).