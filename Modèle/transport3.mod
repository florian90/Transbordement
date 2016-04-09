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
/*var x{i in F, j in P, k in C}, integer, >=0;
var y{i in F, j in P, k in C}, binary;*/

var x{i in U, j in U, k in U}, integer, >= 0;
var y{i in U, j in U, k in U}, binary;

var x2{i in DEP, j in ARR}, integer, >=0;

/* Fonction objectif */
minimize z: sum{i in F, j in P, k in C} (x[i, j, k] * (h[i, j] + h[j, k]) + x[i, j, k] * g[j]); /*y[i, j, k] * (c[i, j] + c[j, k]) + */
/* Somme pour tout arc de : 
	    nombre de produit transporté * coût unitaire 
	+ coût fixe si arc utilisé 
	+ nombre de produit transbordés * coût de transbordement */

/* Contraintes */

/*s.t. C1{i in F, j in P, k in C}: 		x[i, j, k] <= u[i, j];
s.t. C2{i in F, j in P, k in C}: 		x[i, j, k] <= u[j, k];*/

s.t. C3{i in F}: sum{j in P, k in C} (x[i, j, k]) == -b[i];
s.t. C4{j in P}: sum{i in F, k in C} (x[i, j, k]) == -b[j];
s.t. C5{k in C}: sum{i in F, j in P} (x[i, j, k]) == -b[k];

/*s.t. C6{i in F, j in P, k in C}: 		inf * y[i, j, k] >= x[i, j, k];*/

/*s.t. C7{i in F, j in P, k in C}: 		y[i, j, k] * (t[i, j] + t[j, k]) + s[j] <= T;*/

s.t. C8{i in DEP, j in ARR}: x2[i, j] = sum{k in C} x[i, j, k];
s.t. C9{i in DEP, j in ARR}: x2[i, j] <= u[i, j];

solve;

display z, x;

end;
