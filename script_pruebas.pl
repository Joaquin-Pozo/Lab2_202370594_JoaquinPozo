% Estaciones Linea 1 simplificada
station(0, "Baquedano", "c", 30, ST0),
station(1, "USACH", "c", 30, ST1),
station(2, "Estación Central", "c", 45, ST2),
station(3, "ULA", "r", 45, ST3),
station(4, "República", "r", 45, ST4),
station(5, "Los Héroes", "c", 60, ST5),
station(6, "Toesca", "r", 40, ST6),
station(7, "La Moneda", "r", 40, ST7),
station(8, "Cochera", "m", 3600, ST8),
station(9, "Parque OHiggins", "r", 30, ST9),
station(10, "San Pablo", "r", 40, ST10),
station(11, "Los Dominicos", "c", 60, ST11),
station(12, "Tobalaba", "t", 40, ST12),
% Estaciones Linea 6 simplificada
station(13, "Cerrillos", "t", 31, ST13),
station(14, "Lo Valledor", "r", 89, ST14),
station(15, "Pdte. Pedro Aguirre Cerda", "c", 56, ST15),
station(16, "Franklin", "r", 19, ST16),
station(17, "Biobío", "r", 57, ST17),
station(18, "Ñuble", "m", 32, ST18),
station(19, "Estadio Nacional", "r", 26, ST19),
station(20, "Ñuñoa", "r", 57, ST20),
station(21, "Inés de Suárez", "m", 62, ST21),
station(22, "Los Leones", "t", 15, ST22),
% Creando secciones de L1
section(ST0, ST1, 2, 50, S0),
section(ST1, ST2, 2.5, 55, S1),
section(ST2, ST3, 1.5, 30, S2),
section(ST3, ST4, 3, 45, S3),
section(ST4, ST5, 3, 45, S4),
section(ST5, ST6, 1.4, 50, S5),
section(ST6, ST7, 2, 40, S6),
section(ST7, ST8, 3, 200, S7),
section(ST8, ST9, 7, 200, S8),
section(ST9, ST10, 7, 200, S9),
section(ST10, ST11, 7, 200, S10),
section(ST11, ST12, 7, 200, S11),
% Creando secciones de L6
section(ST13, ST14, 6, 70, S12),
section(ST14, ST15, 4.6, 40, S13),
section(ST15, ST16, 5.9, 92, S14),
section(ST16, ST17, 9.5, 65, S15),
section(ST17, ST18, 2, 84, S16),
section(ST18, ST19, 10.2, 100, S17),
section(ST19, ST20, 4, 65, S18),
section(ST20, ST21, 7, 48, S19),
section(ST21, ST22, 1.5, 54, S20),
% Creando lineas
line(0, "Línea 0", "UIC 60 ASCE", [ ], L0),
line(1, "Línea 1", "100 R.E.", [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10], L1),
line(6, "Línea 6", "200 R.E.", [S12, S13, S14, S15, S16, S17, S18, S19, S20], L6),
lineLength(L0, LargoLinea0, DistanciaLinea0, CostoLinea0),
lineLength(L1, LargoLinea1, DistanciaLinea1, CostoLinea1),
lineLength(L6, LargoLinea6, DistanciaLinea6, CostoLinea6),
lineSectionLength(L1, "Cochera", "Los Dominicos", SECCIONES, DISTANCIA, COSTO),
lineAddSection(L0, S0, L0_1),
lineAddSection(L0_1, S1, L0_2),
lineAddSection(L0_2, S2, L0_3),
lineAddSection(L1, S11, L1_1),
isLine(L1),
isLine(L6),
isLine(L0_3),
pcar(0, 90, "NS-74", "ct", PC0),
pcar(1, 100, "NS-74", "tr", PC1),
pcar(2, 150, "NS-74", "tr", PC2),
pcar(3, 90, "NS-74", "ct", PC3),
pcar(4, 100, "AS-2014", "ct", PC4),
pcar(5, 100, "AS-2014", "ct", PC5),
pcar(6, 100, "AS-2016", "ct", PC6),
pcar(7, 50, "AS-2016", "tr", PC7),
pcar(8, 30, "AS-2016", "tr", PC8),
pcar(9, 120, "AS-2016", "ct", PC9),
pcar(10, 90, "AS-2016", "ct", PC10),
train(0, "CAF", "UIC 60 ASCE", 60, [ ], T0),
train(1, "CAF", "UIC 60 ASCE", 70, [PC1, PC0, PC3, PC2], T1),
train(2, "CAF", "UIC 60 ASCE", 40, [PC7, PC9, PC10, PC8], T6),
train(3, "CAF", "UIC 60 ASCE", 80, [PC8, PC6, PC10, PC9, PC7], T7),
%el tren resultante T5 debería ser equivalente a T1
trainAddCar(T0, PC1, 0, T2),
trainAddCar(T2, PC2, 1, T3),
trainAddCar(T3, PC0, 1, T4),
trainAddCar(T4, PC3, 2, T5),
trainRemoveCar(T5, 2, T52),
trainRemoveCar(T52, 1, T53),
isTrain(T4),
isTrain(T53),
trainCapacity(T1, CapacityT1),
trainCapacity(T2, CapacityT2),
trainCapacity(T53, CapacityT53),
driver(0, "Jorge Gónzalez", "CAF", D0),
driver(1, "Gabriel Boric", "ALSTOM", D1),
driver(2, "Violeta Parra", "CAF", D2),
driver(3, "Carlos Rodriguez", "ALSTOM", D3),
subway(0, "Metro Santiago", SW0),
subwayAddTrain(SW0, [T1], SW1),
subwayAddTrain(SW1, [T6, T7], SW2),
subwayAddLine(SW2, [L1_1], SW3),
subwayAddLine(SW3, [L6], SW4),
subwayAddDriver(SW4, [D0] , SW5),
subwayAddDriver(SW5, [D1, D2, D3], SW6),
subwayToString(SW6, STR1),
subwayToString(SW5, STR2),
subwayToString(SW4, STR3),
subwayToString(SW3, STR4),
subwaySetStationStoptime(SW6, "Los Héroes", 90, SW7),
subwaySetStationStoptime(SW7, "USACH", 120, SW8),
subwaySetStationStoptime(SW8, "Estación Central", 70, SW9),
subwayAssignTrainToLine(SW9, 1, 6, SW9_1),
subwayAssignDriverToTrain(SW9_1, 2, 1, "10:30:00", "Cerrillos", "Los Leones", SW9_2),
whereIsTrain(SW9_2, 1, "15:30:00", Station).