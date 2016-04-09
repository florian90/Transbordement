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

param b{i in U} default 0; 			/* si >0 demande, si < 0 dispo du noeud 		*/

param u{i in U, j in U}, default 0; 		/* Capa de l'arc 		*/
param c{i in U, j in U}, default 0; 		/* Coût fixe de l'arc 		*/
param h{i in U, j in U}, default 0; 		/* Coût variable de l'arc 					*/
param t{i in U, j in U}, default 0; 		/* Temps de transport 					*/

param g{i in U} default 0;			/* Coût unitaire de transbordement 			*/
param s{p in P} default 0;			/* Temps de transbordement				*/

param T;			/* Temps maximal du transport de chaque produit				*/

param inf:=1000000;				/* Param plus grand que tous les x pour des tests binaires			*/

/* Variables */
var x{i in U, j in U}, integer, >=0;
var y{i in U, j in U}, binary;			/* Booléen définissant si les arc sont utilisés ou pas				*/
var test{p in P, i in U};

/* Fonction objectif */
minimize z: sum{i in U, j in U} (x[i, j]*h[i, j] + y[i, j] * c[i, j] + x[i, j] * g[i]);
/* Somme pour tout arc de : 
	    nombre de produit transporté * coût unitaire 
	+ coût fixe si arc utilisé 
	+ nombre de produit transbordés * coût de transbordement */

/* Contraintes */
s.t. C1{i in U, j in U}: 	x[i, j] <= u[i, j]; 				/* utilisation <= capacité max 				*/
s.t. C2{i in U}: 		sum{j in U} (x[i, j] - x[j, i]) == -b[i]; 				/* pas de pertes 		*/
s.t. C3{i in U, j in U}: 	inf * y[i, j] >= x[i, j];				/* y[i, j] représente si l'arc (i, j) est utilisée		*/
s.t. C4{p in P, i in U, j in U}:	t[i, p] * y[i, p] + s[p] + t[p, j] * y[p, j] <= T;		/* Le temps de chaque trajet utilisé est <= au temps max 		*/
				/* /!\ Le temps de transport est surrestimé dans certains cas /!\ 	*/

solve;

display z, x, test;

end;
