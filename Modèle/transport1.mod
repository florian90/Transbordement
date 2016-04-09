/*
	Problème de transport :
	- Les quantités transportées doivent êtres conservées, produites ou consommées en chaque point
	- Les capacités transportés doivent être inférieurs à la capacité maximale de l'arc
*/

/* Paramètres */
param n;
set U:=1..n;

param b{i in U}; /* si >0 demande, si < 0 dispo du noeud */
param d{i in U, j in U}; /* Capa de l'arc */
param c{i in U, j in U}; /* Coût de l'arc */

/* Variables */
var x{i in U, j in U}, integer, >=0;

/* Fonction objectif */
minimize z: sum{i in U, j in U} x[i, j]*c[i, j];

/* Contraintes */
s.t. C1{i in U}: (sum{j in U} x[i, j]) - (sum{j in U} x[j, i] ) = -b[i]; /* pas de pertes */
s.t. C2{i in U, j in U}: x[i, j] <= d[i, j]; /* utilisation <= capacité max */

solve;

display z, x;

end;
