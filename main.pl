% Req. 2: TDA station - constructor
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

% Req. 3: TDA section - constructor
% Meta Primaria: section/5
% Meta Secundaria: (float(Distance); integer(Distance)),Distance > 0,(float(Cost); integer(Cost)),Cost >= 0,!
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]) :-
    (float(Distance); integer(Distance)),
    Distance > 0,
    (float(Cost); integer(Cost)),
    Cost >= 0,
    !.

% Req. 4: TDA line - constructor
% Meta Primaria: line/5
% Meta Secundaria: integer(Id),string(Name),string(RailType),is_list(Sections)
line(Id, Name, RailType, Sections, [Id, Name, RailType, Sections]) :-
    integer(Id),
    string(Name),
    string(RailType),
    is_list(Sections).
% Req. 5: TDA line - Otros predicados
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

% Req. 6: TDA line - otras funciones
% Meta Primaria: lineSectionLength/5
% Meta Secundaria: line/5, lineSectionLengthRec/6
lineSectionLength(Line, Station1Name, Station2Name, Distance, Cost) :-
    line(_, _, _, Sections, Line),
    lineSectionLengthRec(Sections, Station1Name, Station2Name, Distance, Cost, 0).

lineSectionLengthRec([], _, _, 0, 0, _).
lineSectionLengthRec([Section | Sections], Station1Name, Station2Name, Distance, Cost, Flag) :-
    section(GetStation1, GetStation2, GetDistance, GetCost, Section),
    station(_, GetStation1Name, _, _, GetStation1),
    station(_, GetStation2Name, _, _, GetStation2),
    (   (GetStation1Name = Station1Name, GetStation2Name = Station2Name) ->
            (Distance is GetDistance, Cost is GetCost)
    ;   (GetStation1Name = Station1Name) ->
            lineSectionLengthRec(Sections, GetStation2Name, Station2Name, DistanceAux, CostAux, 1),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost
    ;   (GetStation2Name = Station2Name) ->
            lineSectionLengthRec(Sections, GetStation1Name, Station2Name, DistanceAux, CostAux, 0),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost
    ;   (Flag = 1 ->
            lineSectionLengthRec(Sections, Station1Name, Station2Name, DistanceAux, CostAux, Flag),
            Distance is DistanceAux + GetDistance,
            Cost is CostAux + GetCost)
    ;   lineSectionLengthRec(Sections, Station1Name, Station2Name, Distance, Cost, Flag)
    ).
