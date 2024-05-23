% Req. 2 TDA station - constructor
% Meta Primaria: station/5
% Meta Secundaria: integer(Id),string(Name),string(Type),(Type == "r"; Type == "m"; Type == "c"; Type == "t"),integer(StopTime),StopTime >= 0,!.
station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime]) :-
    integer(Id),
    string(Name),
    string(Type),
    (Type == "r"; Type == "m"; Type == "c"; Type == "t"),
    (integer(StopTime); float(StopTime)),
    StopTime >= 0,
    !.

% Req. 3 TDA section - constructor
% Meta Primaria: section/5
% Meta Secundaria: (float(Distance); integer(Distance)),Distance > 0,(float(Cost); integer(Cost)),Cost >= 0,!
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]) :-
    (float(Distance); integer(Distance)),
    Distance > 0,
    (float(Cost); integer(Cost)),
    Cost >= 0,
    !.

% Req. 4 TDA line - constructor
% Meta Primaria: line/5
% Meta Secundaria: integer(Id),string(Name),string(RailType),is_list(Sections)
line(Id, Name, RailType, Sections, [Id, Name, RailType, Sections]) :-
    integer(Id),
    string(Name),
    string(RailType),
    is_list(Sections).
% Req. 5 TDA line - Otros predicados
% Meta Primaria: lineLength/4
% Meta Secundaria: line/5, lineLengthRec/4
lineLength(Line, Length, Distance, Cost) :-
    line(_, _, _, Sections, Line),
    lineLengthRec(Sections, Length, Distance, Cost).
% TDA line- Otros predicados: caso base para función recursiva
% Meta Primaria: lineLength/4
% Meta Secundaria: N/A
lineLengthRec([], 0, 0, 0).
% TDA line- Otros predicados: caso recursivo para función recursiva
% Meta Primaria: lineLength/4
% Meta Secundaria: section/5, lingeLengthRec/4, Distance is GetDistanceAux + GetDistance,Cost is GetCostAux + GetCost,Length is GetLengthAux + 1
lineLengthRec([Section | Sections], Length, Distance, Cost) :-
    section(_, _, GetDistance, GetCost, Section),
    lineLengthRec(Sections, GetLengthAux, GetDistanceAux, GetCostAux),
    Distance is GetDistanceAux + GetDistance,
    Cost is GetCostAux + GetCost,
    Length is GetLengthAux + 1.

% Req. 6 TDA line - otras funciones
% Meta Primaria: lineSectionLength/6
% Meta Secundaria: line/5, lineSectionLengthRec/7
lineSectionLength(Line, Station1Name, Station2Name, Path, Distance, Cost) :-
    line(_, _, _, Sections, Line),
    lineSectionLengthRec(Sections, Station1Name, Station2Name, Path, Distance, Cost, 0).

% TDA line - Otras funciones: caso base para función recursiva
% Meta Primaria: lineSectionLengthRec/7
% Meta Secundaria: N/A
lineSectionLengthRec([], _, _, [], 0, 0, _).

% TDA line - Otras funciones: caso recursivo para función recursiva
% Meta Primaria: lineSectionLengthRec/7
/* Meta Secundaria: section/5, station/5, station/5,(GetStation1Name = Station1Name, GetStation2Name = Station2Name) ->
					(Distance is GetDistance, Cost is GetCost, Path = [Section]);(GetStation1Name = Station1Name) ->lineSectionLengthRec/7,
					Distance is DistanceAux + GetDistance,Cost is CostAux + GetCost,Path = [Section | PathAux];(GetStation2Name = Station2Name) ->
					lineSectionLengthRec(Sections, GetStation1Name, Station2Name, PathAux, DistanceAux, CostAux, 0),Distance is DistanceAux + GetDistance,
					Cost is CostAux + GetCost,Path = [Section | PathAux];Flag = 1 ->lineSectionLengthRec/7,Distance is DistanceAux + GetDistance,
					Cost is CostAux + GetCost,Path = [Section | PathAux]);lineSectionLengthRec/7)*/
lineSectionLengthRec([Section | Sections], Station1Name, Station2Name, Path, Distance, Cost, Flag) :-
    section(GetStation1, GetStation2, GetDistance, GetCost, Section),
    station(_, GetStation1Name, _, _, GetStation1),
    station(_, GetStation2Name, _, _, GetStation2),
    (   (GetStation1Name = Station1Name, GetStation2Name = Station2Name) ->
            (Distance is GetDistance, Cost is GetCost, Path = [Section])
    ;   (GetStation1Name = Station1Name) ->
            lineSectionLengthRec(Sections, GetStation2Name, Station2Name, PathAux, DistanceAux, CostAux, 1),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost,
        	Path = [Section | PathAux]
    ;   (GetStation2Name = Station2Name) ->
            lineSectionLengthRec(Sections, GetStation1Name, Station2Name, PathAux, DistanceAux, CostAux, 0),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost,
        	Path = [Section | PathAux]
    ;   (Flag = 1 ->
            lineSectionLengthRec(Sections, Station1Name, Station2Name, PathAux, DistanceAux, CostAux, Flag),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost,
            Path = [Section | PathAux])
    ;   lineSectionLengthRec(Sections, Station1Name, Station2Name, Path, Distance, Cost, Flag)
    ).

% Req. 7 TDA line - modificador: Predicado que permite añadir tramos a una línea
% Meta Primaria: lineAddSection/3
% Meta Secundaria: line/5, line/5
lineAddSection(Line, Section, LineOut) :-
    line(GetLineId, GetLineName, GetLineRailType, GetLineSections, Line),
    not(member(Section, GetLineSections)),
    append(GetLineSections, [Section], LineSectionsOut),
    line(GetLineId, GetLineName, GetLineRailType, LineSectionsOut, LineOut).

% Req. 8 TDA Línea - pertenencia. Predicado que permite determinar si un elemento cumple con las 
% restricciones señaladas en apartados anteriores en relación a las estaciones y tramos para poder conformar una línea.
% Meta Primaria:
% Meta Secundaria:
isLine(Line) :-
    line(_, _, _, GetLineSections, Line),
    (GetLineSections = [] ->  false;  
    (verifyIdName(GetLineSections, [], []), verifySections(GetLineSections))),
    !.

% Caso base: no hay más secciones que verificar
verifyIdName([], _, _).

% Caso recursivo: verifica cada sección en la lista de secciones
verifyIdName([FirstSection | RestSections], IdList, NameList) :-
    section(GetPoint1, _, _, _, FirstSection),
    station(GetIdPoint1, GetNamePoint1, _, _, GetPoint1),
    checkStationIdName(GetIdPoint1, GetNamePoint1, IdList, NameList, UpdatedIdList, UpdatedNameList),
    verifyIdName(RestSections, UpdatedIdList, UpdatedNameList).

% TDA line: Verifica y actualiza la lista de IDs y nombres
checkStationIdName(Id, Name, IdList, NameList, UpdatedIdList, UpdatedNameList) :-
    % Verifica si el ID y el nombre ya estan en las listas
    (	not(member(Id, IdList)) ->
    		UpdatedIdList = [Id | IdList]
    	;	fail)
    ;	(not(member(Name, NameList)) ->
        	UpdatedNameList = [Name | NameList]
     	;   fail). 
% TDA line: Verifica si de una estación se pueda ir a todas las demás estaciones
verifySections([]).
verifySections([_]). % Una sola sección es válida por defecto
verifySections([FirstSection, SecondSection | RestSections]) :-
    section(_, Station, _, _, FirstSection),
    section(Station, _, _, _, SecondSection),
    verifySections([SecondSection | RestSections]).

% Req. 9 TDA pcar - Constructor. Permite crear los carros de pasajeros que conforman un convoy. Los carros pueden ser de tipo terminal (tr) o central (ct).
% Meta Primaria: pcar/5
% Meta Secundaria: integer(Id),integer(Capacity),Capacity >= 0,string(Model),string(Type).
pcar(Id, Capacity, Model, Type, [Id, Capacity, Model, Type]) :-
    integer(Id),
    integer(Capacity),
    Capacity >= 0,
    string(Model),
    string(Type).

% Req. 10 TDA train - Constructor. Predicado que permite crear un tren o convoy.
% Meta Primaria: train/6
% Meta Secundaria:  integer(Id),string(Maker),string(RailType),integer(Speed),Speed >= 0,is_list(Pcars),(Pcars = [] ->  true; checkTrainStructure(Pcars)).
train(Id, Maker, RailType, Speed, Pcars, [Id, Maker, RailType, Speed, Pcars]) :-
    integer(Id),
    string(Maker),
    string(RailType),
    integer(Speed),
    Speed >= 0,
    is_list(Pcars),
    ((Pcars = []; Pcars = [_]) ->  true; checkTrainStructure(Pcars)).

% TDA train: Verifica si el primer y ultimo carro son terminales, y si tienen modelos compatibles
checkTrainStructure([FirstPcar | RestPcars]) :-
    pcar(_, _, ModelPcar, TypePcar, FirstPcar),
    checkMiddlePcars(RestPcars, LastPcar, ModelPcar),
    pcar(_, _, ModelPcar, TypePcar, LastPcar),
    TypePcar = "tr".
% TDA train: Verifica si los demas carros son centrales, y si tienen modelos compatibles
checkMiddlePcars([LastPcar], LastPcar, _).
checkMiddlePcars([FirstPcar | RestPcars], LastPcar, ModelPcar) :-
    pcar(_, _, ModelPcar, Type, FirstPcar),
    Type = "ct",
    checkMiddlePcars(RestPcars, LastPcar, ModelPcar).

% Req. 11 TDA train - Modificador. Función que permite añadir carros a un tren en una posición dada.
% Meta Primaria:
% Meta Secundaria:
trainAddCar(Train, Pcar, Position, TrainOut) :-
    train(Id, Maker, RailType, Speed, Pcars, Train),
    addCarInPosition(Pcars, Pcar, Position, PcarsOut),
    train(Id, Maker, RailType, Speed, PcarsOut, TrainOut).

% TDA train: Añade un carro a una lista de carros en una posición dada
addCarInPosition(Pcars, Pcar, 0, [Pcar | Pcars]) :- !.
addCarInPosition([FirstPcar | RestPcars], Pcar, Position, [FirstPcar | NewRestPcars]) :-
    Position > 0,
    NewPosition is Position - 1,
    addCarInPosition(RestPcars, Pcar, NewPosition, NewRestPcars).

% Req. 12 TDA train - Modificador. Predicado que permite eliminar un carro desde el convoy.
% Meta Primaria:
% Meta Secundaria:
trainRemoveCar(Train, Position, TrainOut) :-
    train(Id, Maker, RailType, Speed, Pcars, Train),
    removeCarInPosition(Pcars, Position, PcarsOut),
    train(Id, Maker, RailType, Speed, PcarsOut, TrainOut).

% TDA train: Elimina un carro de una lista de carros en una posición dada
removeCarInPosition([_ | Rest], 0, Rest) :- !.
removeCarInPosition([FirstPcar | RestPcars], Position, [FirstPcar | NewRestPcars]) :-
    Position > 0,
    NewPosition is Position -1,
    removeCarInPosition(RestPcars, NewPosition, NewRestPcars).
    
% Req. 13 TDA train - Pertenencia. Predicado que permite determinar si un elemento es un tren válido
% Meta Primaria
% Meta Secundaria
isTrain(Train) :-
    train(Id, Maker, RailType, Speed, Pcars, Train),
    integer(Id),
    string(Maker),
    string(RailType),
    integer(Speed),
    Speed >= 0,
    is_list(Pcars),
    checkTrainStructure(Pcars).

% Req. 14 TDA train - Otros predicados. Predicado que permite determinar la capacidad máxima de pasajeros del tren.
% Meta Primaria:
% Meta Secundaria:
trainCapacity(Train, Capacity) :-
    train(_, _, _, _, Pcars, Train),
    trainCapacityRec(Pcars, Capacity).

trainCapacityRec([], 0).
trainCapacityRec([FirstPcar | RestPcars], Capacity) :-
    pcar(_, GetCapacity, _, _, FirstPcar),
    trainCapacityRec(RestPcars, CapacityAux),
    Capacity is GetCapacity + CapacityAux.
