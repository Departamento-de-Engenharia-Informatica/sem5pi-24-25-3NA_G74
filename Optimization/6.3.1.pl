:- dynamic availability/3.
%Estas declarações permitem que os fatos sejam dinamicamente modificados (inseridos, removidos ou atualizados) durante a execução do programa.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.

%O agenda_staff/3 é um fato que representa o horario ocupado do staff.
agenda_staff(102005,20241028,[(700,800,A),(1000,1300,B)]).
agenda_staff(102010,20241028,[(800,900,A),(950,1000,B),(600,700,C)]).
agenda_staff(102014,20241028,[(700,800,A),(900,950,B)]).
agenda_staff(102017,20241028,[]).
agenda_staff(102020,20241028,[]).
agenda_staff(102024,20241028,[]).
agenda_staff(102028,20241028,[]).
agenda_staff(102011,20241028,[]).
agenda_staff(102031,20241028,[]).
agenda_staff(102026,20241028,[]).

%A timetable/3 representa o horario de trabalho de um staff, aqui representado por número do staff, data e intervalo do dia em minutos.
timetable(102005,20241028,(600,1200)).
timetable(102010,20241028,(500,1440)).
timetable(102014,20241028,(200,800)).
timetable(102017,20241028,(300,900)).
timetable(102020,20241028,(400,700)).
timetable(102024,20241028,(200,800)).
timetable(102028,20241028,(300,900)).
timetable(102011,20241028,(400,1100)).
timetable(102031,20241028,(700,1440)).
timetable(102026,20241028,(300,1000)).

%No staff/4 é armazenado a informaçao sobre o staff incluindo o número do staff, a funçao, a especialidade e o tipo de cirurgia que pode executar.
staff(102005,doctor,cardiologist,[4,8]).
staff(102010,doctor,orthopaedist,[1,2,3,5,7,9]).
staff(102014,doctor,cardiologist,[4,8]).
staff(102017,doctor,orthopaedist,[1,2,3,5,7,9]).
staff(102020,doctor,cardiologist,[4,8]).
staff(102024,doctor,gastrologist,[6,10]).
staff(102028,doctor,orthopaedist,[1,2,3,5,7,9]).
staff(102011,doctor,cardiologist,[4,8]).
staff(102031,doctor,gastrologist,[6,10]).
staff(102026,doctor,gastrologist,[6,10]).

%No surgery/4 está contemplado o tempo para cada fase da cirurgia e o id da cirurgia.
surgery(1,15,40,30).
surgery(2,45,90,45).
surgery(3,30,200,45).
surgery(4,45,160,45).
surgery(5,45,190,45).
surgery(6,45,175,45).
surgery(7,45,260,45).
surgery(8,45,390,45).
surgery(9,45,275,45).
surgery(10,45,375,45).

%No surgery_id/2 corresponde à associaçao entre o id de cirurgia e os pedidos efetuados para agendamento de cirurgia.
surgery_id(1,2).
surgery_id(2,6).
surgery_id(3,6).
surgery_id(4,7).
surgery_id(5,8).
surgery_id(6,2).
surgery_id(7,4).
surgery_id(8,3).

%E o assignment_surgery/2 corresponde à necessidade de staff para a cirurgia correspondente.
assignment_surgery(1,102010).
assignment_surgery(1,102017).
assignment_surgery(1,102028).
assignment_surgery(2,102024).
assignment_surgery(2,102031).
assignment_surgery(2,102026).
assignment_surgery(3,102024).
assignment_surgery(3,102031).
assignment_surgery(4,102017).
assignment_surgery(5,102020).
assignment_surgery(6,102010).
assignment_surgery(7,102011).
assignment_surgery(8,102028).

%O agenda_operation_room/3 funciona identicamente ao agenda_staff mas, para salas de cirurgia, contempla as marcaçoes existentes nas salas.
agenda_operation_room(1,20241028,[(450,600,A),(900,1200,B)]).
agenda_operation_room(2,20241028,[]).
agenda_operation_room(3,20241028,[]).
agenda_operation_room(4,20241028,[]).
agenda_operation_room(5,20241028,[]).
agenda_operation_room(6,20241028,[]).

%Os predicados free_agenda0/2 e free_agenda1/2 permitem calcular o tempo disponivel de cada staff para o respectivo dia.
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

%O adapt_timetable/4 faz transformaçao à timetable existente para contemplar as alteraçoes de tempo livro tendo em conta o tempo disponivel.
adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).

%Os predicados de intersect servem para cruzar as disponibilidades de horario entre os medicos e verificar em que horarios sao compativeis.
%Neste caso temos um predicado para apenas intersectar duas agendas ou a globalidade das agendas para a cirurgia especificada.
intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
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



%O schedule_all_surgeries/2 faz a distribuiçao das cirurgias pela sala selecionada da data selecionada. Faz marcaçoes e calcula novas agendas para o staff e a sala.
schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),


%O availability_all_surgeries/3 verifica se é possivel atribuir todas as cirurgias de um tipo especifico a uma determinada sala e data.
    availability_all_surgeries(LOpCode,Room,Day),!.

availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),surgery(OpType,_,TSurgery,_),
    availability_operation(OpCode,Room,Day,LPossibilities,LDoctors),
    schedule_first_interval(TSurgery,LPossibilities,(TinS,TfinS)),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors),
    availability_all_surgeries(LOpCode,Room,Day).


% Este predicado permite avaliar e agender a lista de possibilidades para um determinado tipo de operacao num dia e sala especificada.
availability_operation(OpCode,Room,Day,LPossibilities,LDoctors):-surgery_id(OpCode,OpType),surgery(OpType,_,TSurgery,_),
    findall(Doctor,assignment_surgery(OpCode,Doctor),LDoctors),
    intersect_all_agendas(LDoctors,Day,LA),
    agenda_operation_room1(Room,Day,LAgenda),
    free_agenda0(LAgenda,LFAgRoom),
    intersect_2_agendas(LA,LFAgRoom,LIntAgDoctorsRoom),
    remove_unf_intervals(TSurgery,LIntAgDoctorsRoom,LPossibilities).

% Limpa os intervalos de tempo que sao demasiado pequenos para assegurar que apenas existem horarios adequados.
remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).

%Coloca o primeiro intervalo de cirurgia possivel em memoria.
schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

%Insere nova agenda com otimizaçao realizada.
insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).


%É o predicado geral basico para optimizar o planeamento de cirurgias. A partir da sala e do dia estabelecido. O predicado escreve num ficheiro os resultados.
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

%Faz varias permutaçoes em cadeia para tentar descobrir a soluçao optima.
obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
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

%Este predicado permite atualizar a soluçao otima se for encontrada um soluçao melhor que a atual.
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



