%Como testar:
%No swi-prolog carregar o ficheiro e correr o seguinte comando:
%schedule_all_surgeries(Sala,Data).
%Substituir a sala e data pelo desejado.

:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]).
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

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).


assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).
assignment_surgery(so100005,d003).




agenda_operation_room(or1,20241028,[]).


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

schedule_all_surgeries(Room,Day):-
	get_time(Ti),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),
	%Inicio da alteraçao de codigo. Heuristica de tempo.
    heuristic_by_time(LOpCode,Room,Day,SortedLOpCode),
    extract_ids(SortedLOpCode,IdList),
    availability_all_surgeries(IdList,Room,Day),!,
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

availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),
    surgery(OpType,TAnaes,TSurgery,TClean),
    total_time(TAnaes,TSurgery,TATotal),
    total_time(TATotal,TClean,TTotal),
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
    select_element(SortedStaff1,AD),
    %write(AD),
    select_element(SortedStaff2,AN),
    %write(AN),
    select_element(SortedStaff3,MAA),
    %write(MAA),
    select_element(SortedStaff4,IN),
    %write(IN),
    select_element(SortedStaff5,CN),
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
        nl
    ),
    heuristic_by_time(LOpCode,Room,Day,SortedLOpCode),
    extract_ids(SortedLOpCode,IdList),
    availability_all_surgeries(IdList,Room,Day).

%Aqui está a grande lógica, verifico quando é que está disponivel a cirurgia com
%o medico mais cedo, e faço deteçao de erro, por exemplo, se a sala ja estiver ocupada,
%entao paro o agendamento, se ainda for possivel marcar cirurgias continuo até,
%nao ter cirurgias para marcar.
heuristic_by_time([], _, _, []).
heuristic_by_time([OpCode | LOpCode], Room, Day, SortedLOpCode) :-
    surgery_id(OpCode, OpType),
    surgery(OpType, _, TSurgery, _), 
    availability_operation(OpCode, Room, Day, LPossibilities, _),
    (LPossibilities \= [] ->
        schedule_first_interval(TSurgery, LPossibilities, (TinS, _)),            
        heuristic_by_time(LOpCode, Room, Day, PartialSorted),
        append([(OpCode, TinS)], PartialSorted, PartialSorted1),
        inverter(PartialSorted1, NewPartialSorted),
        sort_op_list(NewPartialSorted, SortedList),
        SortedLOpCode = SortedList;   
    SortedLOpCode = []
    ).

% Predicado auxiliar para ordenar pelo segundo elemento da tupla
sort_op_list(UnsortedList, SortedList) :-
    sort(2, @=<, UnsortedList, SortedList).

% Extrai o primeiro elemento (ID) de cada tupla
extract_ids(SortedList, IdList) :-
    maplist(first_element, SortedList, IdList).

% Função que vai buscar o primeiro elemento da tupla
first_element((ID, _), ID).

% Caso base: se a lista de entrada estiver vazia, a lista de saída é a mesma.
add_to_list([], FullList, FullList).

% Caso recursivo: adiciona o elemento da cabeça da lista de entrada à lista de saída.
add_to_list([H|T], Acc, FullList) :-
    add_to_list(T, [H|Acc], FullList).

% Predicado principal que inicia a lista acumuladora como vazia.
inverter(InputList, FullList) :-
    add_to_list(InputList, [], FullList).

%O restante codigo é igual ao fornecido.
availability_operation(OpCode,Room,Day,LPossibilities,LDoctors):-surgery_id(OpCode,OpType),surgery(OpType,_,TSurgery,_),
    findall(Doctor,assignment_surgery(OpCode,Doctor),LDoctors),
    intersect_all_agendas(LDoctors,Day,LA),
    agenda_operation_room1(Room,Day,LAgenda),
    free_agenda0(LAgenda,LFAgRoom),
    intersect_2_agendas(LA,LFAgRoom,LIntAgDoctorsRoom),
    remove_unf_intervals(TSurgery,LIntAgDoctorsRoom,LPossibilities).


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

select_element([Element | _], Element).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_agenda_staff((TinS,TfinS,OpCode),Day,Staff):-
    retract(agenda_staff1(Staff,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Staff,Day,Agenda1)).

