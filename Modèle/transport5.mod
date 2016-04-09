 /*
	Problème de transport :
	- Les quantités transportées doivent êtres conservées, produites ou consommées en chaque point
	- Les capacités transportés doivent être inférieurs à la capacité maximale de l'arc
	- Le temps de transport de chaque produit doit être inférieur au temps maximal T
	-> Il faut minimiser la somme des couts unitaires + les couts fixes pour les arcs utilisés / transbordement
*/

/* Paramètres */
param nf;			/* Nombre de fournisseurs 				*/
param np;						/* Nombre de plates formes 				*/
param nc;						/* Nombre de clients 					*/

set F := 1..nf;						/* Ensemble de fournisseurs 				*/
set P := nf+1..nf+np;				/* Ensemble de plates formes 				*/
set C := nf+np+1..nf+np+nc;			/* Ensemble de clients 					*/
set U := F union P union C;			/* Total des noeuds 		*/
set DEP := F union P;
set ARR := P union C;

param b{i in U} default 0; 			/* si >0 demande, si < 0 dispo du noeud 		*/

param u{i in U, j in U}, default 0; 		/* Capa de l'arc 		*/
param c{i in U, j in U}, default 0; 		/* Coût fixe de l'arc 		*/
param h{i in U, j in U}, default 0; 		/* Coût variable de l'arc 					*/
param t{i in U, j in U}, default 0; 		/* Temps de transport 					*/

param g{i in P} default 0;			/* Coût unitaire de transbordement 			*/
param s{p in P} default 0;			/* Temps de transbordement				*/

param T;			/* Temps maximal du transport de chaque produit				*/

param inf:=10000;				/* Param plus grand que tous les x pour des tests binaires			*/

/* Variables */
var x2{i in U, j in U}, integer, >=0;
var y2{i in U, j in U}, binary;

var x3{i in U, j in U, k in U}, integer, >= 0;
var y3{i in U, j in U, k in U}, binary;

/* Fonction objectif */
minimize z: sum{i in F, j in P} (x2[i, j] * h[i, j] + y2[i, j] * c[i, j] + x2[i, j] * g[j]) + sum{j in P, k in C} (x2[j, k] * h[j, k] + y2[j, k] * c[j, k] );

/* Contraintes */
s.t. C1{i in U, j in U}: 	x2[i, j] <= u[i, j]; 				/* utilisation <= capacité max 				*/
s.t. C2{i in U}: 		sum{j in U} (x2[i, j] - x2[j, i]) == -b[i]; 				/* pas de pertes 		*/
s.t. C3{i in U, j in U}: 	inf * y2[i, j] >= x2[i, j];
s.t. C4{i in F, j in P, k in C}: 		inf * y3[i, j, k] >= x3[i, j, k];
s.t. C5{i in U, j in U}: 	x2[i, j] = sum{k in C} x3[i, j, k] + sum{k in F} x3[k, j, i];
s.t. C7{i in F, j in P, k in C}: 		y3[i, j, k] * (t[i, j] + t[j, k]) + s[j] <= T;	/* TEMPS */

solve;

display z, x3;

end;
