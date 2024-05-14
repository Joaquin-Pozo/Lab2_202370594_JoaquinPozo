% Req. 1: TDA station - constructor
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

% Req. 2: TDA section - constructor
% Meta Primaria: section/5
% Meta Secundaria: (float(Distance); integer(Distance)),Distance > 0,(float(Cost); integer(Cost)),Cost >= 0,!.
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]) :-
    (float(Distance); integer(Distance)),
    Distance > 0,
    (float(Cost); integer(Cost)),
    Cost >= 0,
    !.

% Req. 3: TDA line - constructor
% Meta Primaria: line/5
% Meta Secundaria: integer(Id),string(Name),string(RailType),is_list(Sections).
line(Id, Name, RailType, Sections, [Id, Name, RailType, Sections]) :-
    integer(Id),
    string(Name),
    string(RailType),
    is_list(Sections).
% Req. 4: TDA line - Otros predicados
% Meta Primaria:
% Meta Secundaria:
lineLength(Line, Length, Distance, Cost).