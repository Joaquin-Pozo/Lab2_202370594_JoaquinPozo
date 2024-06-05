% Req. 2 TDA station - constructor
% Meta Primaria: station/5
% Meta Secundaria: integer(Id),string(Name),string(Type),(Type == "r"; Type == "m"; Type == "c"; Type == "t"),integer(StopTime),StopTime >= 0.
station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime]) :-
    integer(Id),
    string(Name),
    string(Type),
    (Type == "r"; Type == "m"; Type == "c"; Type == "t"),
    (integer(StopTime); float(StopTime)),
    StopTime >= 0.
% Req. 3 TDA section - constructor
% Meta Primaria: section/5
% Meta Secundaria: (float(Distance); integer(Distance)),Distance > 0,(float(Cost); integer(Cost)),Cost >= 0.
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]) :-
    (float(Distance); integer(Distance)),
    Distance > 0,
    (float(Cost); integer(Cost)),
    Cost >= 0.
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
% Meta Secundaria: section/5, lingeLengthRec/4, TotalDistance is DistanceAcc + Distance, TotalCost is CostAcc + Cost, TotalLength is LengthAcc + 1.
lineLengthRec([Section | Sections], TotalLength, TotalDistance, TotalCost) :-
    section(_, _, Distance, Cost, Section),
    lineLengthRec(Sections, LengthAcc, DistanceAcc, CostAcc),
    TotalDistance is DistanceAcc + Distance,
    TotalCost is CostAcc + Cost,
    TotalLength is LengthAcc + 1.
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
    line(_, _, _, Sections, Line),
    Sections \= [],
    checkIdAndName(Sections), 
    checkSectionsConsistency(Sections).
% TDA Line: Funcion que verifica autenticidad de nombres y ids en una linea de metro
checkIdAndName(Sections) :-
    flattenStationNamesAndIds(Sections, NameList, IdList),
    notDuplicateds(IdList),
    notDuplicateds(NameList).
% TDA Line: Caso Base - Funcion que obtiene una lista con los nombres e ids de las estaciones (omite repetidos)
flattenStationNamesAndIds([LastSection], [Name], [Id]) :-
    section(_, Station, _, _, LastSection),
    station(Id, Name, _, _, Station).
% TDA Line: Caso Recursivo - Funcion que obtiene una lista con los nombres e ids de las estaciones (omite repetidos)
flattenStationNamesAndIds([Section | RestSections], [Name | RestNameList], [Id | RestIdList]) :-
    section(Station, _, _, _, Section),
    station(Id, Name, _, _, Station),
    flattenStationNamesAndIds(RestSections, RestNameList, RestIdList).
% TDA Line: Funcion que verifica que no existan duplicados en una lista
notDuplicateds([]).
notDuplicateds([First | Rest]) :-
    not(member(First, Rest)),
    notDuplicateds(Rest).
% TDA line: Verifica si de una estación se pueda ir a todas las demás estaciones
checkSectionsConsistency([_]).
checkSectionsConsistency([FirstSection, SecondSection | RestSections]) :-
    section(_, Station, _, _, FirstSection),
    section(Station, _, _, _, SecondSection),
    checkSectionsConsistency([SecondSection | RestSections]).
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
% Req. 15 TDA driver - Constructor. Predicado que permite crear un conductor cuya habilitación de conducción depende del fabricante de tren (train-maker)
% Meta Primaria: driver/4
% Meta Secundaria: integer(Id), string(Name), string(TrainMaker).
driver(Id, Name, TrainMaker, [Id, Name, TrainMaker]) :-
    integer(Id),
    string(Name),
    string(TrainMaker).
% Req. 16 TDA subway - Constructor. Predicado que permite crear una red de metro.
% Meta Primaria: subway/3
% Meta Secundaria: integer(Id), string(Nombre).
subway(Id, Name, [Id, Name]) :-
    integer(Id),
    string(Name).
% TDA subway: Constructor para agregar trenes a subway
subway(Id, Name, Trains, [Id, Name, Trains]) :-
    integer(Id),
    string(Name),
    is_list(Trains).

% Req. 17 TDA subway - Modificador. Predicado que permite añadir trenes a una red de metro.
% Meta Primaria:
% Meta Secundaria:
subwayAddTrain(Subway, Trains, SubwayOut) :-
    (	subway(Id, Name, Subway) ->
    		checkTrainsStructureAndId(Trains, []),
    		subway(Id, Name, Trains, SubwayOut)
    ;   subway(Id, Name, ExistingTrains, Subway) ->  
    		append(ExistingTrains, Trains, NewTrains),
    		checkTrainsStructureAndId(NewTrains, []),
        	subway(Id, Name, NewTrains, SubwayOut)
    ).
% TDA subway: Funcion que verifica si se repiten IDs en una lista de trains y si los carros que conforman cada train son compatibles
checkTrainsStructureAndId([], _).
checkTrainsStructureAndId([FirstTrain | RestTrains], IdList) :-
    train(Id, _, _, _, Pcars, FirstTrain),
    not(member(Id, IdList)),
    NewIdList = [Id | IdList],
    checkTrainStructure(Pcars),
    checkTrainsStructureAndId(RestTrains, NewIdList).
% TDA subway: Constructor para agregar lineas a subway
subway(Id, Name, Trains, Lines, [Id, Name, Trains, Lines]) :-
    integer(Id),
    string(Name),
    is_list(Trains),
    is_list(Lines).
% Req. 18 TDA subway - Modificador. Predicado que permite añadir líneas a una red de metro.
% Meta Primaria:
% Meta Secundaria:
subwayAddLine(Subway, Lines, SubwayOut) :-
    (	subway(Id, Name, Trains, Subway) ->
    		checkLines(Lines),
    		subway(Id, Name, Trains, Lines, SubwayOut)
    ;   subway(Id, Name, Trains, ExistingLines, Subway) ->  
    		append(ExistingLines, Lines, NewLines),
        	checkLines(NewLines),
        	subway(Id, Name, Trains, NewLines, SubwayOut)
    ).
% TDA subway: Funcion que verifica si las lineas a agregar cumplen con la estructura, sin que se se repitan IDs entre las lineas.

checkLines(Lines) :-
    getLineIdsStationNamesAndId(Lines, LineIds, NameList, IdList),
    flattenList(NameList, NewNameList),
    flattenList(IdList, NewIdList),
    notDuplicateds(LineIds),
    notDuplicateds(NewNameList),
    notDuplicateds(NewIdList).
% TDA subway: Funcion que obtiene todos los ID de las lineas de subway
getLineIdsStationNamesAndId([], [], [], []).
getLineIdsStationNamesAndId([FirstLine | RestLines], [Id | RestIds], [NameList | RestNameList], [IdList | RestIdList]) :-
    isLine(FirstLine),
    line(Id, _, _, Sections, FirstLine),
    flattenStationNamesAndIds(Sections, NameList, IdList),
    getLineIdsStationNamesAndId(RestLines, RestIds, RestNameList, RestIdList).
% TDA subway: Constructor para agregar drivers a subway
subway(Id, Name, Trains, Lines, Drivers, [Id, Name, Trains, Lines, Drivers]) :-
    integer(Id),
    string(Name),
    is_list(Trains),
    is_list(Lines),
    is_list(Drivers).
% Req. 19 TDA subway - Modificador. Predicado que permite añadir conductores a una red de metro.
% Meta Primaria:
% Meta Secundaria:
subwayAddDriver(Subway, Drivers, SubwayOut) :-
    (	subway(Id, Name, Trains, Lines, Subway) ->  
    		subway(Id, Name, Trains, Lines, Drivers, SubwayOut)
    ;   subway(Id, Name, Trains, Lines, ExistingDrivers, Subway) ->  
        	append(ExistingDrivers, Drivers, NewDrivers),
        	subway(Id, Name, Trains, Lines, NewDrivers, SubwayOut)
    ).
% Req. 20 TDA subway - Otras funciones . Función que permite expresar una red de metro en un formato String.
% Meta Primaria:
% Meta Secundaria:
subwayToString(Subway, StringOut) :-
    flattenList(Subway, FlatSubway),
    with_output_to(atom(Atom), format('~w', [FlatSubway])),
    atom_string(Atom, String),
    sub_string(String, 1, _, 1, StringOut).

% TDA subway: Funcion que permite aplanar la lista de listas subway en una sola lista.
flattenList([], []).
flattenList([First | Rest], FlatList) :-
    (   not(is_list(First))) ->
            FlatList = [First | RestFlat],
            flattenList(Rest, RestFlat)
    ;   flattenList(First, FirstFlat),
    	flattenList(Rest, RestFlat),
    	append(FirstFlat, RestFlat, FlatList).

% Req. 21 TDA subway - Modificador. Predicado que permite modificar el tiempo de parada de una estación.
% Meta Primaria: 
% Meta Secundaria:
subwaySetStationStoptime(Subway, StationName, Time, SubwayOut) :-
    subway(Id, Name, Trains, Lines, Drivers, Subway),
    searchForLines(Lines, StationName, Time, NewLines),
    subway(Id, Name, Trains, NewLines, Drivers, SubwayOut).

% TDA subway: Funcion que permite recorrer las lineas del metro y obtener sus secciones
searchForLines([], _, _, []).
searchForLines([FirstLine | RestLines], StationName, Time, [NewFirstLine | NewRestLines]) :-
    line(Id, Name, RailType, Sections, FirstLine),
    searchForSections(Sections, StationName, Time, NewSections),
    line(Id, Name, RailType, NewSections, NewFirstLine),
    searchForLines(RestLines, StationName, Time, NewRestLines).

% TDA subway: Funcion que permite recorrer las secciones de cada linea y obtener sus estaciones
searchForSections([], _, _, []).
searchForSections([FirstSection | RestSections], StationName, Time, [NewFirstSection | NewRestSections]) :-
    section(Station1, Station2, Distance, Cost, FirstSection),
    (   updateStationStopTime(Station1, StationName, Time, NewStation1) ->
        	section(NewStation1, Station2, Distance, Cost, NewFirstSection),
        	searchForSections(RestSections, StationName, Time, NewRestSections)
    ;   updateStationStopTime(Station2, StationName, Time, NewStation2) ->
        	section(Station1, NewStation2, Distance, Cost, NewFirstSection),
    		searchForSections(RestSections, StationName, Time, NewRestSections)
    ;   NewFirstSection = FirstSection, 
    	searchForSections(RestSections, StationName, Time, NewRestSections)
    ).

% TDA subway: Funcion que permite verificar si alguna estacion coincide con StationName, si es asi, actualiza el tiempo de parada
updateStationStopTime(Station, StationName, Time, NewStation) :-
    station(Id, StationName, Type, _, Station),
    station(Id, StationName, Type, Time, NewStation).

% TDA subway: Constructor para agregar recorridos a subway
subway(Id, Name, Trains, Lines, Drivers, Routes, [Id, Name, Trains, Lines, Drivers, Routes]) :-
    integer(Id),
    string(Name),
    is_list(Trains),
    is_list(Lines),
    is_list(Drivers),
    is_list(Routes).
% TDA subway: Constructor para agregar recorridos a subway
route(TrainId, LineId, [TrainId, LineId]).
% TDA subway: Constructor para agregar recorridos a subway
route(TrainId, LineId, DriverId, DepartureTime, DepartureStation, ArrivalStation, [TrainId, LineId, DriverId, DepartureTime, DepartureStation, ArrivalStation]).
% Req. 22 TDA subway - Modificador. Predicado que permite asignar un tren a una línea.
% Meta Primaria:
% Meta Secundaria:
subwayAssignTrainToLine(Subway, TrainId, LineId, SubwayOut) :-
    subway(Id, Name, Trains, Lines, Drivers, Subway),
    checkTrainId(Trains, TrainId),
    checkLineId(Lines, LineId),
    route(TrainId, LineId, Route),
    subway(Id, Name, Trains, Lines, Drivers, Route, SubwayOut).

% TDA subway: Funcion que verifica si el Id de un tren se encuentra en Subway
checkTrainId(Trains, TrainId) :-
    getTrainsId(Trains, IdList),
    member(TrainId, IdList).
% TDA subway: Funcion que aplana todos los Id de una lista de trains
getTrainsId([], []).
getTrainsId([FirstTrain | RestTrains], [Id | IdList]) :-
    train(Id, _, _, _, _, FirstTrain),
    getTrainsId(RestTrains, IdList).
% TDA subway: Funcion que verifica si el Id de una linea se encuentra en Subway
checkLineId(Lines, LineId) :-
    getLinesId(Lines, IdList),
    member(LineId, IdList).
% TDA subway: Funcion que aplana todos los Id de una lista de lines
getLinesId([], []).
getLinesId([FirstLine | RestLines], [Id | IdList]) :-
    line(Id, _, _, _, FirstLine),
    getLinesId(RestLines, IdList).

% Req. 23 TDA subway - Modificador. Predicado que permite asignar un conductor a un tren en un horario de salida determinado considerando estación de partida y de llegada.
% Meta Primaria:
% Meta Secundaria:
subwayAssignDriverToTrain(Subway, DriverId, TrainId, DepartureTime, DepartureStation, ArrivalStation, SubwayOut) :-
    subway(Id, Name, Trains, Lines, Drivers, Route, Subway),
    assignDriverToTrain(Route, DriverId, TrainId, DepartureTime, DepartureStation, ArrivalStation, NewRoute),
    subway(Id, Name, Trains, Lines, Drivers, NewRoute, SubwayOut).

assignDriverToTrain(Route, DriverId, TrainId, DepartureTime, DepartureStation, ArrivalStation, NewRoute) :-
    route(TrainId, LineId, Route),
    route(TrainId, LineId, DriverId, DepartureTime, DepartureStation, ArrivalStation, NewRoute).

% Req. 24 TDA subway - Otros predicados. Predicado que permite determinar dónde está un tren a partir de una hora indicada del día.
% Meta Primaria:
% Meta Secundaria:
whereIsTrain(Subway, TrainId, Time, Station) :-
    subway(_, _, Trains, Lines, _, Route, Subway),
    route(TrainId, LineId, _, DepartureTime, DepartureStation, ArrivalStation, Route),
    calculateTimeDiff(DepartureTime, Time, Minutes),
    getTrainSpeed(Trains, TrainId, Speed),
    getLineSections(Lines, LineId, Sections),
    calculateClosestStation(Sections, Minutes, Speed, Station, DepartureStation, ArrivalStation, 0).

% Calcula la diferencia de tiempo en minutos
calculateTimeDiff(DepartureTime, Time, Minutes) :-
    transformToMinutes(DepartureTime, NewDepartureTime),
    transformToMinutes(Time, NewTime),
    Minutes is NewTime - NewDepartureTime.

% Convierte una cadena de tiempo en minutos
transformToMinutes(Time, TimeOut) :-
    split_string(Time, ":", "", TimeList),
    TimeList = [HourStr, MinuteStr, SecondStr],
    number_string(Hours, HourStr),
    number_string(Minutes, MinuteStr),
    number_string(Seconds, SecondStr),
    TotalMinutes is Hours * 60 + Minutes + Seconds // 60,
    TimeOut = TotalMinutes.

% Obtiene la velocidad de un tren dado su ID
getTrainSpeed([], _, _) :- fail.
getTrainSpeed([FirstTrain | _], TrainId, Speed) :-
    train(TrainId, _, _, Speed, _, FirstTrain).
getTrainSpeed([_ | RestTrains], TrainId, Speed) :-
    getTrainSpeed(RestTrains, TrainId, Speed).

% Obtiene las secciones de una línea dado su ID
getLineSections([], _, _) :- fail.
getLineSections([FirstLine | _], LineId, Sections) :-
    line(LineId, _, _, Sections, FirstLine).
getLineSections([_ | RestLines], LineId, Sections) :-
    getLineSections(RestLines, LineId, Sections).

% Calcula la estación más cercana al tren según el tiempo transcurrido
calculateClosestStation([], _, _, _, _, _, _) :- fail.
calculateClosestStation([FirstSection | RestSections], Minutes, Speed, Station, DepartureStation, ArrivalStation, Flag) :-
    section(Station1, _, _, _, FirstSection),
    station(_, Station1Name, _, _, Station1),
    (   Station1Name = DepartureStation ->
        	startCalculating([FirstSection | RestSections], Minutes, 0, Speed, Station, ArrivalStation, Flag)
    ;   calculateClosestStation(RestSections, Minutes, Speed, Station, DepartureStation, ArrivalStation, Flag)
    ).

% Calcula la estación actual del tren basándose en el tiempo transcurrido
startCalculating([], _, _, _, _, _, _) :- fail.
startCalculating([FirstSection | RestSections], Minutes, MinutesAcc, Speed, Station, ArrivalStation, Flag) :-
    section(Station1, Station2, Distance, _, FirstSection),
    station(_, Station1Name, _, StopTime1, Station1),
    station(_, Station2Name, _, _, Station2),
    NewMinutesAcc is MinutesAcc + Distance / Speed + StopTime1,
    (   NewMinutesAcc >= Minutes, Flag = 0 ->
        Station = Station1Name
    ;   Station1Name = ArrivalStation, Flag = 0 ->
        Station = Station1Name
    ;   Station2Name = ArrivalStation, Flag = 0 ->
        Station = Station2Name
    ;   startCalculating(RestSections, Minutes, NewMinutesAcc, Speed, Station, ArrivalStation, Flag)
    ).
 