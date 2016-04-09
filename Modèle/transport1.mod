/*
	Probl�me de transport :
	- Les quantit�s transport�es doivent �tres conserv�es, produites ou consomm�es en chaque point
	- Les capacit�s transport�s doivent �tre inf�rieurs � la capacit� maximale de l'arc
*/

/* Param�tres */
param n;
set U:=1..n;

param b{i in U}; /* si >0 demande, si < 0 dispo du noeud */
param d{i in U, j in U}; /* Capa de l'arc */
param c{i in U, j in U}; /* Co�t de l'arc */

/* Variables */
var x{i in U, j in U}, integer, >=0;

/* Fonction objectif */
minimize z: sum{i in U, j in U} x[i, j]*c[i, j];

/* Contraintes */
s.t. C1{i in U}: (sum{j in U} x[i, j]) - (sum{j in U} x[j, i] ) = -b[i]; /* pas de pertes */
s.t. C2{i in U, j in U}: x[i, j] <= d[i, j]; /* utilisation <= capacit� max */

solve;

display z, x;

end;
