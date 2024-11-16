:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]).
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).

% first example
%agenda_staff(d001,20241028,[(720,840,m01),(1080,1200,c01)]).
%agenda_staff(d002,20241028,[(780,900,m02),(901,960,m02),(1080,1440,c02)]).
%agenda_staff(d003,20241028,[(720,840,m01),(900,960,m02)]).

%timetable(d001,20241028,(480,1200)).
%timetable(d002,20241028,(720,1440)).
%timetable(d003,20241028,(600,1320)).


staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003,doctor,orthopaedist,[so2,so3,so4]).

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).

% Cirurgias que necessitam de x medico.
assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).
assignment_surgery(so100005,d003).

% Agenda da sala.
agenda_operation_room(or1,20241028,[]).

%Bloco para calculcar tempo livre de uma agenda.
%free_agenda0/2([(i,f,_),...],R)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Este bloco adapta a timetable do funcionario para corresponder
%ao horario disponivel.
%adapt_timetable/4(Staff,Data,ListaTemposLivres,R)
adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Procura a disponibilidade comum de uma lista de staff.
%Devolve os intervalos de tempo em comum.
%intersect_all_agendas/3([Nomes],Data,R)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Marca a cirurgia no primeiro intervalo disponivel, sem considerar se existe
%tempo disponivel ou nao para concluir.
%schedule_first_interval/3(Cirurgia,Tempo disponivel,R)
schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Insere um intervalo de tempo associado a uma cirurgia.
%Nao verifica sobreposiçao apenas insere na agenda.
%insert_agenda/3(Intervalo,Agenda,NovaAgenda)
insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Insere os intervalos de tempo na agenda de cada medico selecionado.
%insert_agenda_doctors(Intervalo,Data,ListaMedicos)
insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Bloco para verificar se é possivel marcar cirrugia em algum intervalo.
remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Começo da euristica dois%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Bloco para calcular ocupaçao dos medicos
%%all_doctor_ocupancy/2(listamedicos,R)
% Predicado principal para somar os intervalos de tempo livre
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

% Função principal para calcular a ocupação de uma lista de médicos.
all_doctor_occupation(Date,[(Doctor, OccupationPercent) | R]) :-
    findall((Doctor, OccupationPercent), (staff_occupation_percentage(Doctor, Date, OccupationPercent)), R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ordena os médicos pela ocupação em ordem decrescente
sort_doctors_by_occupation(DoctorOccupations, SortedDoctors) :-
    % Ordena a lista de médicos pela ocupação (em ordem decrescente)
    sort(2, @>=, DoctorOccupations, SortedDoctorsWithOccupation),
    % Extrai apenas os médicos da lista ordenada
    findall(Doctor, member((Doctor, _), SortedDoctorsWithOccupation), SortedDoctors).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Bloco para encontrar medico mais ocupado.
% Encontra o médico com maior ocupação a partir da lista de ocupações
find_most_occupied_doctor(DoctorOccupations, SortedDoctors) :-
    sort_doctors_by_occupation(DoctorOccupations, SortedDoctors).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Verifica se existem cirurgias para o medico selecionado e seleciona todas.
search_surgery_for_doctor([], _, []) :-
    !.  % Caso base: se a lista de médicos está vazia, não há cirurgias para marcar.
   
search_surgery_for_doctor(_, [], []) :-
    !.  % Caso base: se a lista de cirurgias pendentes está vazia, não há mais cirurgias para marcar.

search_surgery_for_doctor(LOpCode, [Doctor | Rest], OpCode) :-
    % Filtra as cirurgias da lista LOpCode que são associadas ao médico atual (Doctor)
    findall(OpCode, (member(OpCode, LOpCode), assignment_surgery(OpCode, Doctor)), LOpCodeForDoctor),
   
    % Se encontrar cirurgias, retorna a primeira OpCode encontrada
    (  
        LOpCodeForDoctor \= []
    ->  select(OpCode, LOpCodeForDoctor, _),  % Seleciona uma cirurgia da lista
        true  % Sai e utiliza a cirurgia encontrada
    ;  
        % Se não encontrou cirurgias, tenta o próximo médico
        search_surgery_for_doctor(LOpCode, Rest, OpCode)
    ).

select_surgery([OpCode | _], OpCode).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Aqui faço a remoçao da cirurgia marcada/analisada da lista de cirurgias.
remove_surgery_from_list(_, [], []).  
remove_surgery_from_list(OpCode, [OpCode | Rest], Rest).
remove_surgery_from_list(OpCode, [Head | Rest], [Head | NewRest]) :-
    remove_surgery_from_list(OpCode, Rest, NewRest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado para tentar marcar todas as cirurgias de uma lista de cirurgias para os médicos
attempt_to_mark_surgeries([],_,_) :-
    write('Todas as cirurgias foram marcadas com sucesso!').
attempt_to_mark_surgeries(LOpCode,Room,Day) :-
    all_doctor_occupation(Day, DoctorOccupations),
    find_most_occupied_doctor(DoctorOccupations, SortedDoctors),
    search_surgery_for_doctor(LOpCode,SortedDoctors, OpCode),
    mark_surgery_if_possible(OpCode,Room,Day),
    remove_surgery_from_list(OpCode,LOpCode,NewLOpCode),
    attempt_to_mark_surgeries(NewLOpCode,Room,Day).

%Aqui é onde esta a marcaçao nas agendas.
% Verifica se é possível marcar uma cirurgia, verificando as agendas dos médicos e da sala de operação
mark_surgery_if_possible(OpCode,Room,Date) :-
    surgery_id(OpCode,OpType),surgery(OpType,_,TSurgery,_),
    findall(Doctor,assignment_surgery(OpCode,Doctor),LDoctors),
    intersect_all_agendas(LDoctors, Date, LA),
    agenda_operation_room1(Room, Date, LAgenda),
    free_agenda0(LAgenda, LFAgRoom),
    intersect_2_agendas(LA, LFAgRoom, LIntAgDoctorsRoom),
    remove_unf_intervals(TSurgery, LIntAgDoctorsRoom, LPossibilities),
    (  
        LPossibilities \= []
    ->  % Se houver horários disponíveis, marca a cirurgia
        schedule_first_interval(TSurgery,LPossibilities,(TinS,TfinS)),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors),
        write('Cirurgia marcada com sucesso!')
    ;  
        % Se não houver horários disponíveis, apenas avisa e continua sem interromper
        write('Não há horários disponíveis para marcar a cirurgia.'), nl
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_all_surgeries(Room,Day):-
	get_time(Ti),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),
    attempt_to_mark_surgeries(LOpCode,Room,Day),!,
   
%Exportaçao dos resultados para consola.
    write("Resultados de agendamento para o dia "), write(Day), write(":\n\n"),
    format("Sala de Operações ~w~n", [Room]),
    (   agenda_operation_room1(Room, Day, AgendaRoom),
        AgendaRoom \= [] ->
        format("  Agenda para sala ~w no dia ~w: ~w~n", [Room, Day, AgendaRoom])
    ;   write("  Nenhuma cirurgia agendada.\n")
    ),
    write("\nAgendas dos Médicos:\n"),
    findall(Doctor, agenda_staff1(Doctor, Day, _), Doctors),
    forall(member(Doctor, Doctors),
           (agenda_staff1(Doctor, Day, AgendaDoc),
            format("  Médico ~w: ~w~n", [Doctor, AgendaDoc]))
    ),
	get_time(Tf),
	T is Tf-Ti,
	write('Tempo de geracao da solucao:'),write(T),nl.