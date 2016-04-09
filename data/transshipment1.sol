- Best evaluation = 183.0

- Best solution=GTransshipmentSolution [evaluation=183.0, tabAssignment=[0, 
    0, 0, 4, 
	0, 0, 8, 
	0, 0, 6, 
	
	0, 0, 0, 0, 
	0, 0, 0, 0, 
	4, 5, 7, 2] 
]

- Solving time = 19m0.925s 

===============================================================================

java -ea -Xms512m -Xmx2048m Main 0

Loading challenge file: data/transshipment1.txt
GTransshipmentProblem.load(data/transshipment1.txt)
problem=GTransshipmentProblem [instanceName=transshipment1, tabNodes=[GNode 1 [depot, b=-4], GNode 2 [depot, b=-8], GNode 3 [depot, b=-6], GNode 4 [platform, g=10.0, s=5.0], GNode 5 [platform, g=15.0, s=3.0], GNode 6 [platform, g=5.0, s=10.0], GNode 7 [client, b=4], GNode 8 [client, b=5], GNode 9 [client, b=7], GNode 10 [client, b=2]], tabEdges=[GEdge (1,4) [u=5, c=3.0, h=2.0, t=3.0], GEdge (1,5) [u=5, c=3.0, h=2.0, t=3.0], GEdge (1,6) [u=5, c=3.0, h=2.0, t=3.0], GEdge (2,4) [u=5, c=3.0, h=2.0, t=3.0], GEdge (2,5) [u=5, c=3.0, h=2.0, t=3.0], GEdge (2,6) [u=5, c=3.0, h=2.0, t=3.0], GEdge (3,4) [u=5, c=3.0, h=2.0, t=3.0], GEdge (3,5) [u=5, c=3.0, h=2.0, t=3.0], GEdge (3,6) [u=5, c=3.0, h=2.0, t=3.0], GEdge (4,7) [u=5, c=3.0, h=2.0, t=3.0], GEdge (4,8) [u=5, c=3.0, h=2.0, t=3.0], GEdge (4,9) [u=5, c=3.0, h=2.0, t=3.0], GEdge (4,10) [u=5, c=3.0, h=2.0, t=3.0], GEdge (5,7) [u=5, c=3.0, h=2.0, t=3.0], GEdge (5,8) [u=5, c=3.0, h=2.0, t=3.0], GEdge (5,9) [u=5, c=3.0, h=2.0, t=3.0], GEdge (5,10) [u=5, c=3.0, h=2.0, t=3.0], GEdge (6,7) [u=5, c=3.0, h=2.0, t=3.0], GEdge (6,8) [u=5, c=3.0, h=2.0, t=3.0], GEdge (6,9) [u=5, c=3.0, h=2.0, t=3.0], GEdge (6,10) [u=5, c=3.0, h=2.0, t=3.0]], T=1000.0]

First assignment=GTransshipmentSolution [evaluation=183.0, tabAssignment=[0, 0, 0, 4, 0, 0, 8, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 4, 5, 7, 2] ]

Best solution=GTransshipmentSolution [evaluation=183.0, tabAssignment=[0, 0, 0, 4, 0, 0, 8, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 4, 5, 7, 2] ]

real	19m0.925s
user	19m1.112s
sys	0m0.088s

