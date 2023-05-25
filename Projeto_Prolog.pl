% ist1107137 - Margarida Ascensao Lourenco
:- set_prolog_flag(answer_write_options,[max_depth(0)]).   % para listas completas     
:- ['dados.pl'], ['keywords.pl'].                          % ficheiros a importar.


% QUALIDADE DOS DADOS %


/* 
eventosSemSalas(-EventosSemSala:list)

Este predicado e satisfeito se EventosSemSala e uma lista, ordenada e sem elementos
repetidos, de ID's dos eventos sem sala.
*/
eventosSemSalas(EventosSemSala):- 
    findall(ID, evento(ID, _, _, _, semSala), EventosSemSala).                        


/* 
eventosSemSalasDiaSemana(+DiaDaSemana:atom, -EventosSemSala:list)

E verdade se EventosSemSala e uma lista, ordenada e sem elementos repetidos, de 
ID's dos eventos sem sala que ocorrem no dia da semana pretendido (DiaDaSemana).
*/
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-
    eventosSemSalas(TodosEventosSemSala),  
    % Da lista anterior seleciona os eventos que ocorrem em DiaDaSemana.                                                                  
    findall(ID, (member(ID, TodosEventosSemSala), 
    horario(ID, DiaDaSemana, _, _, _, _)), EventosSemSala).    


/* 
eventosPeriodoSemestral(+Periodo:atom, +PeriodoSemestral:atom, -EventosPeriodo:list)

O predicado e satisfeito se EventosPeriodo e uma lista com os ID's de todos os 
eventos que decorrem no periodo fornecido, incluindo as disciplinas semestrais.
*/
eventosPeriodoSemestral(Periodo, PeriodoSemestral, EventosPeriodo) :-
    % Lista com os eventos que decorrem no periodo pretendido.
    findall(ID, horario(ID, _, _, _, _, Periodo), EventosNessePeriodo),
    % Lista com os eventos semestrais que decorrem no periodo pretendido.
    findall(ID, horario(ID, _, _, _, _, PeriodoSemestral), EventosSemestrais),
    % Une as duas listas, eliminando os eventos repetidos.
    union(EventosNessePeriodo, EventosSemestrais, EventosPeriodo).

/* 
eventosPeriodo(+Periodo:atom, -EventosPeriodo:list)

Este predicado e utilizado para obter uma lista de ID's de eventos do periodo 
especificado. Ele utiliza o predicado eventosPeriodoSemestral, definindo as 
restricoes para garantir que o predicado utilizado funciona corretamente.
*/  

% consoante o periodo fornecido, atribui o identificador de PeriodoSemestral correto.  
eventosPeriodo(Periodo, EventosPeriodo) :-  
    member(Periodo, [p1, p2]), 
    PeriodoSemestral = p1_2,
    eventosPeriodoSemestral(Periodo, PeriodoSemestral, EventosPeriodo).
    
eventosPeriodo(Periodo, EventosPeriodo) :-
    member(Periodo, [p3, p4]), 
    PeriodoSemestral = p3_4,
    eventosPeriodoSemestral(Periodo, PeriodoSemestral, EventosPeriodo).
     

/* 
eventosSemSalasPeriodo(+ListaPeriodos:list, -EventosSemSala:list)

E verdade se ListaPeriodos e uma lista de periodos e EventosSemSala e uma lista, 
ordenada e sem elementos repetidos, de IDs de eventos sem sala nos periodos de 
ListaPeriodos.
*/
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala):- 
    eventosSemSalas(TodosEventosSemSalas),
    % ListaEventosPeriodo = [[EventosPeriodo1], [EventosPeriodo2], ...]
    findall(EventosPeriodo, (member(Periodo, ListaPeriodos), 
    eventosPeriodo(Periodo, EventosPeriodo)), ListaEventosPeriodo),
    % concatena todas as listas de eventos de cada periodo.      
    append(ListaEventosPeriodo, ListaEventos),                                                                                 
    sort(ListaEventos, ListaEventosSemRepetidos),   
    % Cria uma lista com os eventos comuns.                                                                                         
    intersection(TodosEventosSemSalas, ListaEventosSemRepetidos, EventosSemSala).                                                


% PESQUISA SIMPLES %


/* 
organizaEventos(+ListaEventos:list, Periodo:atom, -EventosNoPeriodo:list)

E verdade se EventosNoPeriodo e a lista, ordenada e sem elementos repetidos, de ID's 
dos eventos de ListaEventos que ocorrem no periodo Periodo.
*/
organizaEventos([],_,[]):- !. 
organizaEventos([ID|Resto], Periodo, EventosNoPeriodo):- 
    eventosPeriodo(Periodo, EventosPeriodo),
    not(member(ID, EventosPeriodo)),
    organizaEventos(Resto, Periodo, EventosNoPeriodo). 

organizaEventos([ID|Resto], Periodo, [ID|EventosNoPeriodo]):- 
    eventosPeriodo(Periodo, EventosPeriodo),
    member(ID, EventosPeriodo),
    organizaEventos(Resto, Periodo, EventosNoPeriodo). 


/* 
eventosMenoresQue(+Duracao:real,-Eventos:list)

E verdade se ListaEventosMenoresQue e a lista ordenada e sem elementos repetidos dos 
identificadores dos eventos que tem duracao menor ou igual a Duracao. 
*/
eventosMenoresQue(Duracao, Eventos):- 
    setof(ID, eventosMenoresQueBool(ID, Duracao), Eventos).                


/* 
eventosMenoresQueBool(+ID:inteiro, +Duracao:real)

E verdade se o evento identificado por ID tiver duracao igual ou menor a Duracao.
*/
eventosMenoresQueBool(ID, Duracao) :- 
    horario(ID, _, _, _, DuracaoEvento, _), DuracaoEvento =< Duracao.


/* 
procuraDisciplinas(+Curso:atom, -ListaDisciplinas:list)

O predicado e satisfeito quando ListaDisciplinas e uma lista ordenada por ordem
alfabetica com os nomes das disciplinas do curso Curso.
*/
procuraDisciplinas(Curso, ListaDisciplinas) :-
    findall(NomeDisciplina, (turno(ID, Curso, _, _), 
    evento(ID, NomeDisciplina, _, _, _)), Disciplinas),
    sort(Disciplinas, ListaDisciplinas).


/* 
organizaDisciplinas(+ListaDisciplinas:List, +Curso:atom, -Semestres:list)

E verdade se Semestres e uma lista com duas listas. A primeira sublista contem as 
disciplinas que ocorrem durante o primeiro semestre, e a segunda as disciplinas 
que decorrem durante o segundo semestre.
*/ 

organizaDisciplinas(ListaDisciplinas, Curso, Semestres):-
    organizaDisciplinas(ListaDisciplinas,Curso,[],[],Semestres).

% PSemestre corresponde a lista com as disciplinas do primeiro semestre.
% SSemestre corresponde a lista com as disciplinas do segundo semestre.

organizaDisciplinas([], _, PSemestre, SSemestre, Semestres):-
    sort(PSemestre, PSemestreordenado),
    sort(SSemestre, SSemestreordenado),
    append([PSemestreordenado], [SSemestreordenado], Semestres).

organizaDisciplinas([Disciplina|RestoDisciplinas],Curso,PSemestre,SSemestre,Semestres):-
    (disciplinasPrimeiroSemestre(Disciplina, Curso),
    organizaDisciplinas(RestoDisciplinas,Curso,[Disciplina|PSemestre],SSemestre,Semestres));
    (disciplinasSegundoSemestre(Disciplina, Curso),
    organizaDisciplinas(RestoDisciplinas,Curso,PSemestre,[Disciplina|SSemestre],Semestres)).


/*
disciplinasPrimeiroSemestre(+Disciplina:atom, +Curso:atom)
disciplinasSegundoSemestre(+Disciplina:atom, +Curso:atom)

Estes predicados auxiliares sao satisfeitos se a disciplina do curso Curso pertence ao 
primeiro ou segundo semestre, respetivamente.
*/
disciplinasPrimeiroSemestre(Disciplina, Curso):-
    evento(ID, Disciplina, _, _, _), turno(ID, Curso, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p1, p2, p1_2]).
  
disciplinasSegundoSemestre(Disciplina, Curso):-
    evento(ID, Disciplina, _, _, _), turno(ID, Curso, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p3, p4, p3_4]).

/* 
horasCurso(+Periodo:atom, +Curso:atom, +Ano:integer, -TotalHoras:real)

E verdade se TotalHoras e o numero de horas de um curso Curso num dado Ano e Periodo.
*/ 
horasCurso(Periodo, Curso, Ano, TotalHoras):-
    eventosPeriodo(Periodo, EventosPeriodo),
    findall(ID, (member(ID, EventosPeriodo), 
            turno(ID, Curso, Ano, _)), EventosCurso),
    % se varios turnos de um curso partilharem o mesmo evento, este so e contado uma vez.
    sort(EventosCurso, EventosCursoSemRepetidos),
    findall(Duracao, (member(ID, EventosCursoSemRepetidos), 
            horario(ID, _, _, _, Duracao, _)), ListaHoras),
    sum_list(ListaHoras, TotalHoras).

/* 
evolucaoHorasCurso(+Curso:atom,-Evolucao:list)

E verdade se Evolucao for uma lista de tuplos na forma (Ano, Periodo, NumHoras), em 
que NumHoras e o total de horas associadas ao curso Curso, no ano Ano e periodo 
Periodo.
*/
evolucaoHorasCurso(Curso, Evolucao) :-
    ListaAnos = [1,2,3],
    ListaPeriodos = [p1, p2, p3, p4],
    findall((Ano, Periodo, TotalHoras), 
        (member(Ano, ListaAnos), member(Periodo, ListaPeriodos), 
        horasCurso(Periodo, Curso, Ano, TotalHoras)), Evolucao).  


% OCUPACAO CRITICA DAS SALAS %


/* 
ocupaSlot(+HoraInicioDada:(integer|real), +HoraFimDada:(integer|real), 
          +HoraInicioEvento:(integer|real), +HoraFimEvento:(integer|real), -Horas:real)

E verdade se Horas for numero de horas sobrepostas entre o evento que tem inicio em 
HoraInicioEvento e fim em HoraFimEvento, e o slot que tem inicio em HoraInicioDada
e fim em HoraFimDada. Se nao existirem sobreposicoes o predicado deve falhar.
*/ 
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada < HoraFimEvento,
    HoraInicioEvento < HoraFimDada, 
    % verifica qual o menor valor entre as horas de fim.
    (HoraFimDada < HoraFimEvento, HoraFim = HoraFimDada; 
        HoraFim = HoraFimEvento),
    % verifica qual o maior valor entre as horas de inicio. 
    (HoraInicioDada > HoraInicioEvento, HoraInicio = HoraInicioDada;
        HoraInicio = HoraInicioEvento),
    Horas is HoraFim - HoraInicio,!.


/* 
numHorasOcupadas(+Periodo:atom, +TipoSala:atom, +DiaSemana:atom, +HoraInicio:real,
                 +HoraFim:real, -SomaHoras:real)

E verdade se SomaHoras for o numero de horas ocupadas nas salas do tipo TipoSala, no
intervalo de tempo definido entre HoraInicio e HoraFim, no dia da semana DiaSemana, e
no periodo Periodo.
*/ 
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    eventosPeriodo(Periodo, EventosPeriodo),
    salas(TipoSala, ListasSalas),
    % filtra os eventos que ocorrem nas salas do tipo TipoSala no periodo Periodo.
    findall(ID, (member(ID, EventosPeriodo), evento(ID, _, _, _, Sala), 
            member(Sala, ListasSalas)), EventosSala),
    % filtra os eventos do DiaSemana, com inicio e fim no intervalo de tempo.
    findall(ID, (member(ID, EventosSala), horario(ID, DiaSemana, _, _, _, _)), 
            EventosDiaSemana),
    findall(ID, (member(ID, EventosDiaSemana), horario(ID, _, HoraInicioEvento, 
            HoraFimEvento, _, _), 
            ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, _)), 
            EventosSlot), 
    % calcula o numero de horas ocupadas por cada evento.  
    findall(Horas, (member(ID, EventosSlot), horario(ID, _, HoraInicioEvento, 
            HoraFimEvento, _, _), 
            ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), 
            ListaHoras),
    sum_list(ListaHoras, SomaHoras).  


/* 
ocupacaoMax(+TipoSala:atom, +HoraInicio:(integer|real), +HoraFim:(integer|real), -Max:real) 

E verdade se Max for o numero de horas possiveis de ser ocupadas por salas do tipo 
TipoSala, no intervalo de tempo definido entre HoraInicio e HoraFim. Max e o intervalo
tempo dado por HoraFim - HoraInicio, multiplicado pelo numero de salas em jogo do tipo 
TipoSala.
*/ 
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    salas(TipoSala, ListaSalas),
    length(ListaSalas, NumSalas),
    Max is NumSalas*(HoraFim-HoraInicio).     


/* 
percentagem(+SomaHoras:integer, +Max:integer, -Percentagem:real) 

E verdade se Percentagem for a divisao de SomaHoras por Max, multiplicada por 100.
*/ 
percentagem(SomaHoras,Max, Percentagem):-
    Percentagem is SomaHoras/Max*100.

/* 
ocupacaoCritica(+HoraInicio:(integer|real), +HoraFim: (integer|real), +ThresHold:integer, 
                -ResultadosFinais:list)

E verdade se Resultados for uma lista ordenada de tuplos do tipo 
casosCriticos(DiaSemana, TipoSala, Percentagem), Em que DiaSemana, TipoSala e Percentagem
sao, respectivamente, um dia da semana, um tipo de sala e a sua percentagem de ocupacao, 
no intervalo de tempo entre HoraInicio e HoraFim, e supondo que a percentagem de ocupacao
relativa a esses elementos esta acima de um dado valor critico (Threshold).

*/
ocupacaoCritica(HoraInicio, HoraFim, ThresHold, Resultados):- 
    findall(casosCriticos(DiaSemana, TipoSala, Percentagem), 
        (evento(ID, _, _, _, Sala), salaTipo(Sala, TipoSala), 
        horario(ID, DiaSemana, _, _, _, Periodo),
        % nao considera os Periodos indeterminados ou semestrais.
        member(Periodo, [p1, p2, p3, p4]),  
        numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio,HoraFim, 
                        SomaHoras),
        ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
        percentagem(SomaHoras, Max, Percentag), 
        Percentagem is ceiling(Percentag),
        Percentagem > ThresHold), Resultados1),
    sort(Resultados1, Resultados).
     

% predicado auxiliar que recebe uma sala e devolve o tipo de sala a que pertence.
salaTipo(Sala, TipoSala):-
    salas(TipoSala, ListaSalas),
    member(Sala, ListaSalas).


/* 
ocupacaoMesa(+ListaPessoas:list, +ListaRestricoes:list, -OcupacaoMesa:list)

O predicado e satisfeito se ListaPessoas for a lista com o nome das pessoas a sentar
a mesa, ListaRestricoes for a lista de restricoes a verificar e OcupacaoMesa for uma 
lista com tres listas, em que a primeira contem as pessoas de um lado da mesa (X1, X2 e X3),
a segunda as pessoas da cabeceira (X4 e X5) e a terceira as pessoas do outro lado da 
mesa (X6, X7 e X8), de modo a que essas pessoas sejam exactamente as da ListaPessoas e 
verifiquem todas as restricoes de ListaRestricoes.
*/
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    % gera todas as possiveis permutacoes da lista de pessoas.
    permutation(ListaPessoas, OcupacaoMesaAux),
    formatar(OcupacaoMesaAux, OcupacaoMesa),
    verificarestricoes(ListaRestricoes, OcupacaoMesa).


% predicado auxiliar que verifica se umaa mesa satisfaz as restricoes.
verificarestricoes([],_).
verificarestricoes([Restricao1|Resto], Mesa):-
    % separa o nome da restricao e os argumentos.
    Restricao1 =.. [Restri|Pessoas_Arg],  
    % adiciona a mesa como ultimo argumento.
    append(Pessoas_Arg, [Mesa], Argumentos),  
    % junta o nome da restricao com os argumentos.
    Verificarestricoes =.. [Restri|Argumentos],
    call(Verificarestricoes),  
    verificarestricoes(Resto, Mesa).  

% predicado auxiliar que formata a lista de pessoas para a estrutura pretendida.      
formatar([X1, X2, X3, X4, X5, X6, X7, X8],
         [[X1, X2, X3], [X4, X5], [X6, X7, X8]]).


% Restricoes necessarias para sentar as pessoas na mesa.
cab1(NomePessoa, Mesa) :- 
    Mesa = [[_, _, _],[NomePessoa, _],[_, _, _]]. 
cab2(NomePessoa, Mesa) :- 
    Mesa = [[_, _, _],[_, NomePessoa],[_, _, _]].
    
honra(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, _], [NomePessoa1, _], [NomePessoa2, _, _]].
honra(NomePessoa1, NomePessoa2, Mesa):- 
    Mesa = [[_, _, NomePessoa2],[ _, NomePessoa1], [ _, _, _]].
    
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[NomePessoa1, NomePessoa2, _], [_, _ ], [_, _, _]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, NomePessoa1, NomePessoa2], [_, _ ], [_, _, _]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[NomePessoa2, NomePessoa1, _], [_, _ ], [_, _, _]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, NomePessoa2, NomePessoa1], [_, _ ], [_, _, _]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, _], [_, _ ], [NomePessoa1, NomePessoa2, _]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, _], [_, _ ], [_, NomePessoa1, NomePessoa2]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, _], [_, _ ], [_, NomePessoa2, NomePessoa1]].
lado(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, _], [_, _ ], [NomePessoa2, NomePessoa1, _]].
    
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[NomePessoa1, _, _], [_, _ ], [NomePessoa2, _, _]].
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, NomePessoa1, _], [_, _ ], [_, NomePessoa2, _]].
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, NomePessoa1], [_, _ ], [_, _, NomePessoa2]].
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[NomePessoa2, _, _], [_, _ ], [NomePessoa1, _, _]].
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, NomePessoa2, _], [_, _ ], [_, NomePessoa1, _]].
frente(NomePessoa1, NomePessoa2, Mesa) :- 
    Mesa = [[_, _, NomePessoa2], [_, _ ], [_, _, NomePessoa1]].
    
naoLado(NomePessoa1, NomePessoa2, Mesa) :- 
    not(lado(NomePessoa1, NomePessoa2, Mesa)).
    
naoFrente(NomePessoa1, NomePessoa2, Mesa) :-
    not(frente(NomePessoa1, NomePessoa2, Mesa)). 
    

        














