% creando una nueva estación
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
station(10, "San Pablo", "t", 40, ST10),
station(11, "Los Dominicos", "t", 60, ST11),
station(12, "Tobalaba", "t", 40, ST12),
% creando una nueva sección
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
% Creando lineas
line(0, "Línea 0", "UIC 60 ASCE", [ ], L0),
line(1, "Línea 1", "100 R.E.", [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10], L1),
lineLength(L0, LargoLinea0, DistanciaLinea0, CostoLinea0),
lineLength(L1, LargoLinea1, DistanciaLinea1, CostoLinea1),
lineSectionLength(L1, "Cochera", "Los Dominicos", SECCIONES, DISTANCIA, COSTO),
lineAddSection(L0, S0, L0_1),
lineAddSection(L0_1, S1, L0_2),
lineAddSection(L0_2, S2, L0_3),
lineAddSection(L1, S11, L1_1),
isLine(L1),
isLine(L0_3),
% creando una nueva estación
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
station(10, "San Pablo", "t", 40, ST10),
station(11, "Los Dominicos", "t", 60, ST11),
station(12, "Tobalaba", "t", 40, ST12),
% creando una nueva sección
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
% Creando lineas
line(0, "Línea 0", "UIC 60 ASCE", [ ], L0),
line(1, "Línea 1", "100 R.E.", [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10], L1),
lineLength(L0, LargoLinea0, DistanciaLinea0, CostoLinea0),
lineLength(L1, LargoLinea1, DistanciaLinea1, CostoLinea1),
lineSectionLength(L1, "Cochera", "Los Dominicos", SECCIONES, DISTANCIA, COSTO),
lineAddSection(L0, S0, L0_1),
lineAddSection(L0_1, S1, L0_2),
lineAddSection(L0_2, S2, L0_3),
lineAddSection(L1, S11, L1_1),
isLine(L1),
isLine(L0_3),

pcar(0, 90, "NS-74", "ct", PC0),
pcar(1, 100, "NS-74", "tr", PC1),
pcar(2, 150, "NS-74", "tr", PC2),
pcar(3, 90, "NS-74", "ct", PC3),
pcar(4, 100, "AS-2014", "ct", PC4),
pcar(5, 100, "AS-2014", "ct", PC5),
pcar(6, 100, "AS-2016", "ct", PC6),
train(0, "CAF", "UIC 60 ASCE", 60, [ ], T0),
train(1, "CAF", "UIC 60 ASCE", 70, [PC1, PC0, PC3, PC2], T1).
