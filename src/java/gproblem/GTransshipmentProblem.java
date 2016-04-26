/**
 *
 *     This file is part of ag41-2016P-challenge-transshipment
 *
 *     ag41-2016P-challenge-transshipment is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     ag41-2016P-challenge-transshipment is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with ag41-2016P-challenge-transshipment.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package gproblem;

import java.sql.Time;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Vector;

import gio.FichierEntree;
import gio.FichierSortie;

/**
 *
 * Transshipment problem for the challenge AG41 of spring 2016
 *
 * @author Olivier Grunder
 * @version 0.03
 * @date 7 march 2016
 *
 */
public class GTransshipmentProblem {

	public static final String TOKEN_NAME="NAME" ;
	public static final String TOKEN_NBR_NODES="NBR_NODES" ;
	public static final String TOKEN_NBR_EDGES="NBR_EDGES" ;
	public static final String TOKEN_NODE="NODE" ;
	public static final String TOKEN_EDGE="EDGE" ;
	public static final String TOKEN_T="T" ;

	public static final double RATIO_DEMAND_NBR_NODES = 5 ; // Total demand/nbr nodes

	public static final double MIN_NODES_DEVIATION = 0.7 ; // 70%
	public static final double MAX_NODES_DEVIATION = 1.3 ; // 130%

	public static final double MIN_T = 100.0 ;
	public static final double MAX_T = 200.0 ;

	public static final double MIN_NODE_DEMAND_PERCENTAGE_DEVIATION = 0.5 ; // 50%
	public static final double MAX_NODE_DEMAND_PERCENTAGE_DEVIATION = 1.5 ; // 150%
	public static final double MIN_NODE_FIXED_COST = 10.0 ;
	public static final double MAX_NODE_FIXED_COST = 100.0 ;
	public static final double MIN_NODE_SETUP = 1.0 ;
	public static final double MAX_NODE_SETUP = 1.0 ;

//	public static final double MIN_RATIO_CAPACITY = 0.8 ;
//	public static final double MAX_RATIO_CAPACITY = 2.0 ;

	public static final double MIN_RATIO_CAPACITY = 1.0 ;
	public static final double MAX_RATIO_CAPACITY = 10.0 ;

	public static final double MIN_EDGE_FIXED_COST = 10;
	public static final double MAX_EDGE_FIXED_COST = 100;
	public static final double MIN_EDGE_UNIT_COST = 1.0;
	public static final double MAX_EDGE_UNIT_COST = 20.0;
	public static final double MIN_EDGE_TRANSP_TIME = 1.0;
	public static final double MAX_EDGE_TRANSP_TIME = 1.0;


	/**
	 * Name of the instance
	 */
	protected String instanceName ;

	/**
	 * Vector of nodes
	 */
	protected GNode tabNodes[] ;

	/**
	 * Vector of edges
	 */
	protected GEdge tabEdges[] ;


	/**
	 * Maximum time to finish the deliveries of all products to the clients
	 */
	protected double t ;


	/**
	 *
	 */
	public GTransshipmentProblem() {
		tabNodes = null ;
		tabEdges = null ;
		t=-1.0 ;
	}


	/**
	 *
	 * @param problemFilename
	 */
	public GTransshipmentProblem(String problemFilename) {
		tabNodes = null ;
		tabEdges = null ;
		t=-1.0 ;

		load (problemFilename) ;

		System.out.println("problem="+toString()) ;
	}


	/**
	 *
	 * @param n
	 * @return
	 */
	static public GTransshipmentProblem getRandomInstance(int n) {
		Calendar cal = Calendar.getInstance();
		Random rand = new Random(cal.getTimeInMillis()) ;

		int nresid = n ;
		int nmin = Math.max( (int)(Math.round(nresid/3.0 * 0.8)), 1) ;
		int nmax = Math.max( (int)(Math.round(nresid/3.0 * 1.2)), nmin)  ;
		int nbrDepot = rand.nextInt(nmax-nmin+1)+nmin ;
		if (nbrDepot<2) nbrDepot = 2 ;

		nresid -= nbrDepot;
		nmin = Math.min( (int) (nresid/2.0 * 0.8), 1) ;
		nmax = Math.max( (int) (nresid/2.0 * 1.2), nmin)  ;
		int nbrPlatform = rand.nextInt(nmax-nmin+1)+nmin ;
		if (nbrPlatform<2) nbrPlatform = 2 ;

		int nbrClient = nresid - nbrPlatform ;

		System.out.println("nbrDepot="+nbrDepot+" ; nbrPlatform="+nbrPlatform+" ; nbrClient="+nbrClient) ;

		int totalDemand = (int) (n*RATIO_DEMAND_NBR_NODES) ;

		GTransshipmentProblem pb = new GTransshipmentProblem() ;
		pb.tabNodes = new GNode[n] ;
		pb.tabEdges = new GEdge[(nbrDepot+nbrClient)*nbrPlatform] ;

		// Tmax
		pb.t = rand.nextDouble()*(MAX_T-MIN_T)+MIN_T ;

		int realQtyDepot = 0 ; // total real qty in all depots
		int realQtyDemand = 0 ; // total real qty for clients
		for (int i=0;i<pb.getNbrNodes();i++) {
			/**
			 * @param nodeind
			 * @param nodex : int x
			 * @param nodey : int y
			 * @param nodeb : int demand
			 * @param nodeg : double cost
			 * @param nodes : double time
			 */
			int nodex = 0 ;
			int nodey = 0 ;
			int demand= 0 ;
			double unitcost = 0 ;
			double setup = 0 ;

			if (i<nbrDepot) {
				nodex = 10 ;
				nodey = 10+i*10 ;
				double factor = (rand.nextDouble()*(MAX_NODE_DEMAND_PERCENTAGE_DEVIATION-MIN_NODE_DEMAND_PERCENTAGE_DEVIATION)+MIN_NODE_DEMAND_PERCENTAGE_DEVIATION) ;
				demand = -(int) (totalDemand/nbrDepot*factor) ;
				realQtyDepot += -demand ;
			}
			else {
				if (i<nbrDepot+nbrPlatform) { // Platform
					nodex = 50 ;
					nodey = 10+(i-nbrDepot)*10 ;
					unitcost = rand.nextDouble()*(MAX_NODE_FIXED_COST-MIN_NODE_FIXED_COST)+MIN_NODE_FIXED_COST ;
					setup = rand.nextDouble()*(MAX_NODE_SETUP-MIN_NODE_SETUP)+MIN_NODE_SETUP ;
				}
				else { // Client
					nodex = 100 ;
					nodey = 10+(i-nbrDepot-nbrPlatform)*10 ;
					double factor = (rand.nextDouble()*(MAX_NODE_DEMAND_PERCENTAGE_DEVIATION-MIN_NODE_DEMAND_PERCENTAGE_DEVIATION)+MIN_NODE_DEMAND_PERCENTAGE_DEVIATION) ;
					demand = (int) (totalDemand/nbrClient*factor) ;
					realQtyDemand += demand ;
				}
			}

			pb.tabNodes[i] = new GNode(i+1, nodex, nodey, demand, unitcost, setup) ;
		}

		// Make sum of the depot = sum of the demand
		if (realQtyDemand>realQtyDepot) { // increase depot
			int diff = realQtyDemand-realQtyDepot ;
			int lastdepotdemand = pb.tabNodes[nbrDepot-1].getDemand() ;
			lastdepotdemand -= diff ;
			pb.tabNodes[nbrDepot-1].setDemand(lastdepotdemand) ;
			totalDemand = realQtyDemand ;
		}
		else { // increase demand
			int diff = realQtyDepot-realQtyDemand ;
			int lastclientdemand = pb.tabNodes[pb.getNbrNodes()-1].getDemand() ;
			lastclientdemand += diff ;
			pb.tabNodes[pb.getNbrNodes()-1].setDemand(lastclientdemand) ;
			totalDemand = realQtyDepot ;
		}

		int iedge=0 ;
		for (int i=0;i<nbrDepot;i++) {
			float avg_capacity = -pb.getNode(i).getDemand()/nbrPlatform ;
			int max_capacity = (int) (avg_capacity*MAX_RATIO_CAPACITY) ;
			int min_capacity = (int) (avg_capacity*MIN_RATIO_CAPACITY) ;
			int total_capacity = 0 ;
			for (int j=0;j<nbrPlatform;j++) {
				// capacity
				int capacity = (int) (rand.nextDouble()*(max_capacity-min_capacity)+min_capacity) ;
				total_capacity += capacity ;
				if (j==nbrPlatform-1 && total_capacity<-pb.getNode(i).getDemand()) {
					int diff = -pb.getNode(i).getDemand()-total_capacity ;
					capacity += 2*diff ;
					total_capacity += 2*diff ;
				}

				double fixedcost = rand.nextDouble()*(MAX_EDGE_FIXED_COST-MIN_EDGE_FIXED_COST)+MIN_EDGE_FIXED_COST ;
				double unitcost = rand.nextDouble()*(MAX_EDGE_UNIT_COST-MIN_EDGE_UNIT_COST)+MIN_EDGE_UNIT_COST ;
				double transptime = rand.nextDouble()*(MAX_EDGE_TRANSP_TIME-MIN_EDGE_TRANSP_TIME)+MIN_EDGE_TRANSP_TIME ;

				/**
				 * @param edgeind
				 * @param nodei : startingNode
				 * @param nodej : endingNode
				 * @param edgeu : u_capacity
				 * @param edgec : c_fixedCost
				 * @param edgeh : h_unitCost
				 * @param edget : t_time
				 */
				//System.out.println ("creating edge: "+pb.tabNodes[i].getIndice()+" -> "+pb.tabNodes[j+nbrDepot].getIndice());
				capacity*=100 ;
				pb.tabEdges[iedge] = new GEdge(iedge+1, pb.tabNodes[i], pb.tabNodes[j+nbrDepot], capacity, fixedcost, unitcost, transptime) ;
				iedge++ ;

			}

			if (total_capacity<-pb.getNode(i).getDemand()) {
				System.out.println("ERROR : depot demand>capacity ; demand="+(-pb.getNode(i).getDemand())+" ; total_capacity="+total_capacity) ;
				System.exit(0) ;
			}

		}

		int tabCapacity[] = new int[nbrClient];
		for (int j=0;j<nbrClient;j++)
			tabCapacity[j] = 0 ;

		for (int i=0;i<nbrPlatform;i++) {
			float avg_product = totalDemand/nbrPlatform ;
			float avg_capacity = avg_product/nbrClient ;
			int max_capacity = (int) (avg_capacity*MAX_RATIO_CAPACITY) ;
			int min_capacity = (int) (avg_capacity*MIN_RATIO_CAPACITY) ;
			for (int j=0;j<nbrClient;j++) {
				// capacity
				int capacity = (int) (rand.nextDouble()*(max_capacity-min_capacity)+min_capacity) ;
				tabCapacity[j] += capacity ;
				if (i==nbrPlatform-1) {
					if (tabCapacity[j]<pb.getNode(j).getDemand()) {
						int diff = pb.getNode(i).getDemand()-tabCapacity[j] ;
						capacity += 2*diff ;
						tabCapacity[j] += 2*diff ;
					}
				}

				if (tabCapacity[j]<pb.getNode(i).getDemand()) {
					System.out.println("ERROR : clicent demand>capacity ; demand="+(pb.getNode(i).getDemand())+" ; tabCapacity[j]="+tabCapacity[j]) ;
					System.exit(0) ;
				}

				double fixedcost = rand.nextDouble()*(MAX_EDGE_FIXED_COST-MIN_EDGE_FIXED_COST)+MIN_EDGE_FIXED_COST ;
				double unitcost = rand.nextDouble()*(MAX_EDGE_UNIT_COST-MIN_EDGE_UNIT_COST)+MIN_EDGE_UNIT_COST ;
				double transptime = rand.nextDouble()*(MAX_EDGE_TRANSP_TIME-MIN_EDGE_TRANSP_TIME)+MIN_EDGE_TRANSP_TIME ;

				/**
				 * @param edgeind
				 * @param nodei : startingNode
				 * @param nodej : endingNode
				 * @param edgeu : u_capacity
				 * @param edgec : c_fixedCost
				 * @param edgeh : h_unitCost
				 * @param edget : t_time
				 */
				capacity*=100 ;
				pb.tabEdges[iedge] = new GEdge(iedge+1, pb.tabNodes[i+nbrDepot], pb.tabNodes[j+nbrDepot+nbrPlatform], capacity, fixedcost, unitcost, transptime) ;
				iedge++ ;

			}
		}

		return pb;
	}




	/**
	 * Save a problem description file
	 *
	 * @param problemFilename name of the problem description file
	 *
	 */
	public void save(String problemFilename) {
		System.out.println("GTransshipmentProblem.save("+problemFilename+")") ;
		FichierSortie fs = new FichierSortie (problemFilename) ;

//		NAME : transshipment1
//		NBR_NODES: 10
//		NBR_EDGES: 21
//		T: 1000
		fs.ecrire(TOKEN_NAME+" : "+this.instanceName+"\n") ;
		fs.ecrire(TOKEN_NBR_NODES+" : "+this.getNbrNodes()+"\n") ;
		fs.ecrire(TOKEN_NBR_EDGES+" : "+this.getNbrEdges()+"\n") ;
		fs.ecrire(TOKEN_T+" : "+this.getT()+"\n") ;

		fs.ecrire("# NODE: i x_i y_i b_i g_i s_i\n") ;
		fs.ecrire("# x_i, y_i : x, y of node i\n") ;
		fs.ecrire("# b_i : demand of node_i, <0 for depot, >0 for client, =0 for platforms\n") ;
		fs.ecrire("# g_i : cost for using node i, >0 only for platforms\n") ;
		fs.ecrire("# s_i : time for crossing node _i, >0 only for platforms\n") ;

		for (int inode=1;inode<=getNbrNodes();inode++) {
			GNode node = getNodeWithIndiceAttribute(inode) ;
			fs.ecrire(TOKEN_NODE+": "+inode+" "+node.getX()+" "+node.getY()+" "+node.getDemand()+" "+node.getCost()+" "+node.getTime()+"\n") ;
		}

		fs.ecrire("# EDGE: k i j u_ij c_ij h_ij t_ij\n") ;
		fs.ecrire("# i : starting node of edge k\n") ;
		fs.ecrire("# j : ending node of edge k\n") ;
		fs.ecrire("# u_ij : capacity of edge (i,j)\n") ;
		fs.ecrire("# c_ij : fixed cost for the use of (i,j)\n") ;
		fs.ecrire("# h_ij : unit cost for (i,j)\n") ;
		fs.ecrire("# t_ij : delivery time for (i,j)\n") ;

		for (int iedge=0;iedge<getNbrEdges();iedge++) {
			GEdge edge = getEdge(iedge) ;
			fs.ecrire(TOKEN_EDGE+": "+edge.getIndice()+" "+edge.getStartingNode().getIndice()+" "+edge.getEndingNode().getIndice()+" "+edge.getCapacity()+" "+edge.getFixedCost()+" "+edge.getUnitCost()+" "+edge.getTime()+"\n") ;
		}

		// Bug v0.03
		fs.ecrire("EOF\n") ;
		fs.fermer() ;

	}



	/**
	 *
	 * @param filename
	 */
	public void saveGmpl(String filename) {
		System.out.println("GTransshipmentProblem.saveGmpl("+filename+".dat)") ;

		FichierSortie fs = new FichierSortie (filename+".dat") ;

//		set DEPOTS := D1 D2 ;
//		set PLATFORMS := PF1  ;
//		set CLIENTS := C1 C2 ;

		fs.ecrire("set DEPOTS :=") ;
		int nbrDepot = getNbrDepot() ;
		for (int i=1;i<=nbrDepot;i++)
			fs.ecrire("D"+i+" ") ;
		fs.ecrire(";\n") ;

		fs.ecrire("set PLATFORMS :=") ;
		int nbrPlatform = getNbrPlatform() ;
		for (int i=1;i<=nbrPlatform;i++)
			fs.ecrire("PF"+i+" ") ;
		fs.ecrire(";\n") ;

		fs.ecrire("set CLIENTS :=") ;
		int nbrClients = getNbrClient() ;
		for (int i=1;i<=nbrClients;i++)
			fs.ecrire("C"+i+" ") ;
		fs.ecrire(";\n") ;

//		param T := 100 ;
		fs.ecrire("param T := "+this.getT()+";\n") ;


//		# demand for nodes
//		param b :=
//			D1  -3
//			D2  -5
//			C1   4
//			C2   4 ;

		fs.ecrire("# demand for nodes\n") ;
		fs.ecrire("param b := \n") ;
		for (int i=0;i<getNbrNodes();i++) {
			if (i<nbrDepot)
				fs.ecrire("  D"+(i+1)+"  "+getNode(i).getDemand()+"\n") ;
			else if (i>=nbrDepot+nbrPlatform)
				fs.ecrire("  C"+(i+1-nbrDepot-nbrPlatform)+"  "+getNode(i).getDemand()+"\n") ;
		}
		fs.ecrire(";\n") ;

//		# Arcs parameters : u=capacity; c=fixed cost; h=unit cost; t=time
//		param : A : u c h t :=
//				D1  PF1 5 3 2 4
//				D2  PF1 5 3 2 4
//				PF1 C1  5 3 2 4
//				PF1 C2  5 3 2 4 ;

		fs.ecrire("# Arcs parameters : u=capacity; c=fixed cost; h=unit cost; t=time\n") ;
		fs.ecrire("param : A : u c h t :=\n") ;
		for (int i=0;i<getNbrEdges();i++) {
			GEdge edge = getEdge(i) ;
			// start
			GNode s = edge.getStartingNode() ;
			if (s.isDepot())
				fs.ecrire("D"+s.getIndice()+"  ") ;
			else
				fs.ecrire("PF"+(s.getIndice() - nbrDepot)+"  ") ;

			// end
			GNode e = edge.getEndingNode() ;
			if (e.isPlatform())
				fs.ecrire("PF"+(e.getIndice() - nbrDepot)+"  ") ;
			else
				fs.ecrire("C"+(e.getIndice() - nbrDepot-nbrPlatform)+"  ") ;

			fs.ecrire(edge.getCapacity()+" "+edge.getFixedCost()+" "+edge.getUnitCost()+" "+edge.getTime()+"\n") ;

		}
		fs.ecrire(";\n") ;

//		# platforms cost parameters
//		param g :=
//			PF1 3 ;

		fs.ecrire("# platforms cost parameters\n") ;
		fs.ecrire("param g := \n") ;
		for (int i=0;i<getNbrNodes();i++) {
			if (i>=nbrDepot && i<nbrDepot+nbrPlatform)
				fs.ecrire("  PF"+(i+1-nbrDepot)+"  "+getNode(i).getCost()+"\n") ;
		}
		fs.ecrire(";\n") ;

//		# platforms time parameters
//		param s :=
//			PF1 5 ;

		fs.ecrire("# platforms time parameters\n") ;
		fs.ecrire("param s := \n") ;
		for (int i=0;i<getNbrNodes();i++) {
			if (i>=nbrDepot && i<nbrDepot+nbrPlatform)
				fs.ecrire("  PF"+(i+1-nbrDepot)+"  "+getNode(i).getTime()+"\n") ;
		}
		fs.ecrire(";\n end;\n ") ;

		fs.fermer() ;

	}



	/**
	 *
	 * @return
	 */
	private int getNbrDepot() {
		int nbrdepot = 0 ;

		for (int i=0;i<getNbrNodes();i++)
			if (getNode(i).getDemand()<0)
				nbrdepot++ ;

		return nbrdepot;
	}

	/**
	 *
	 * @return
	 */
	private int getNbrPlatform() {
		int n = 0 ;

		for (int i=0;i<getNbrNodes();i++)
			if (getNode(i).getDemand()==0)
				n++ ;

		return n;
	}

	/**
	 *
	 * @return
	 */
	private int getNbrClient() {
		int n = 0 ;

		for (int i=0;i<getNbrNodes();i++)
			if (getNode(i).getDemand()>0)
				n++ ;

		return n;
	}


	/**
	 * Open a problem description file
	 *
	 * @param problemFilename name of the problem description file
	 *
	 */
	public int load(String problemFilename) {
		System.out.println("GTransshipmentProblem.load("+problemFilename+")") ;

		if (problemFilename==null) return 1 ;

		int nbrNodes = 0 ;
		int nbrEdges = 0 ;
		tabNodes = null ;
		tabEdges = null ;
		this.t=-1.0 ;

		FichierEntree fe = new FichierEntree (problemFilename) ;

		int itabnodes = 0 ;
		int itabedges = 0 ;
		int encore = 1 ;
		int lineNumber = 0 ;
		while (encore == 1) {
			lineNumber++ ;
			String s = fe.lireLigne() ;
			if (s==null) {
				break ;
			}

			if (s.charAt(0)!='#') {
				StringTokenizer st = new StringTokenizer(s, new String(":")) ;
				String sg = st.nextToken().trim() ;

				if (sg.compareToIgnoreCase(TOKEN_NAME)==0 ) {
					instanceName = st.nextToken().trim()  ;
				}

				if (sg.compareToIgnoreCase(TOKEN_NBR_NODES)==0 ) {
					nbrNodes = Integer.parseInt(st.nextToken().trim()) ;
					tabNodes = new GNode[nbrNodes] ;
				}

				if (sg.compareToIgnoreCase(TOKEN_NBR_EDGES)==0 ) {
					nbrEdges = Integer.parseInt(st.nextToken().trim()) ;
					tabEdges = new GEdge[nbrEdges] ;
				}

				if (sg.compareToIgnoreCase(TOKEN_T)==0 ) {
					this.t = Double.parseDouble(st.nextToken().trim()) ;
				}

				if (sg.compareToIgnoreCase(TOKEN_NODE)==0 ) {
					String params = st.nextToken().trim() ;
					StringTokenizer st2 =  new StringTokenizer(params, new String(" ")) ;
					int nodeind = Integer.parseInt(st2.nextToken().trim()) ;
					int nodex = Integer.parseInt(st2.nextToken().trim()) ;
					int nodey = Integer.parseInt(st2.nextToken().trim()) ;
					int nodeb = Integer.parseInt(st2.nextToken().trim()) ;
					// Bug v0.03
					double nodeg = Double.parseDouble(st2.nextToken().trim()) ;
					double nodes = Double.parseDouble(st2.nextToken().trim()) ;

					tabNodes[itabnodes++] = new GNode(nodeind, nodex, nodey, nodeb, nodeg, nodes) ;
				}

				if (sg.compareToIgnoreCase(TOKEN_EDGE)==0 ) {
					String params = st.nextToken().trim() ;
					StringTokenizer st2 =  new StringTokenizer(params, new String(" ")) ;

					int edgeind = Integer.parseInt(st2.nextToken().trim()) ;

					int edgei = Integer.parseInt(st2.nextToken().trim()) ;
					GNode nodei = getNodeWithIndiceAttribute(edgei) ;
					if (nodei==null)
						printParsingError ("node with indice "+edgei+" does not exist in "+problemFilename+", line "+lineNumber) ;

					int edgej = Integer.parseInt(st2.nextToken().trim()) ;
					GNode nodej = getNodeWithIndiceAttribute(edgej) ;
					if (nodej==null)
						printParsingError ("node with indice "+edgej+" does not exist in "+problemFilename+", line "+lineNumber) ;

					int edgeu = Integer.parseInt(st2.nextToken().trim()) ;
					// Bug v0.03
					double edgec = Double.parseDouble(st2.nextToken().trim()) ;
					double edgeh = Double.parseDouble(st2.nextToken().trim()) ;
					double edget = Double.parseDouble(st2.nextToken().trim()) ;

					tabEdges[itabedges++] = new GEdge(edgeind, nodei, nodej, edgeu, edgec, edgeh, edget) ;
				}

				if (sg.compareToIgnoreCase("EOF")==0 ) {
					encore=0 ;
				}
			}
		}

		fe.fermer() ;

		buildEdgesForNodes() ;

		return 0 ;

	}



	/**
	 * Define starting edges for every node
	 */
	private void buildEdgesForNodes() {
		for (GNode node: tabNodes) {
			int n = 0 ;
			for (GEdge edge: tabEdges) {
				if (edge.getStartingNode()==node)
					n++ ;
			}
			node.setNbrEdges(n) ;

			int i = 0 ;
			for (GEdge edge: tabEdges) {
				if (edge.getStartingNode()==node)
					node.setEdge(i++, edge) ;
			}


		}

	}


	/**
	 * Search for a node which indice attribute is "nodeindice"
	 * returns null if node does not exist
	 *
	 * @param nodeindice : indice attribute of the node :
	 * @return
	 */
	private GNode getNodeWithIndiceAttribute(int nodeindice) {
		for (GNode node: tabNodes) {
			if (node.getIndice()==nodeindice)
				return node ;
		}
		return null;
	}


	/**
	 * Error parsing instance file
	 * @param msg
	 */
	private void printParsingError(String msg) {
		System.out.println("ERROR parsing instance file: "+msg) ;
		System.exit(0) ;

	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "GTransshipmentProblem [instanceName=" + instanceName
				+ ", tabNodes=" + Arrays.toString(tabNodes) + ", tabEdges="
				+ Arrays.toString(tabEdges) + ", T="+t + "]";
	}


	/**
	 *
	 * @return
	 */
	public GNode[] getTabNodes() {
		return tabNodes ;
	}


	/**
	 *
	 * @return
	 */
	public GEdge[] getTabEdges() {
		return tabEdges ;
	}

	/**
	 *
	 * @return
	 */
	public int getNbrNodes() {
		return tabNodes.length ;
	}


	/**
	 *
	 * @return
	 */
	public int getNbrEdges() {
		return tabEdges.length ;
	}


	public GEdge getEdge(int i) {
		return tabEdges[i];
	}

	public GNode getNode(int i) {
		return tabNodes[i];
	}


	/**
	 * @return the t
	 */
	public double getT() {
		return t;
	}


	/**
	 * @param t the t to set
	 */
	public void setT(double t) {
		this.t = t;
	}


	/**
	 * Return an edge from its data indice
	 * the data indice is the indice of the edge in the data file
	 *
	 * @param edgeIndice
	 * @return
	 */
	public GEdge getEdgeFromIndice(int edgeIndice) {
		for (int i=0;i<tabEdges.length;i++) {
			if (tabEdges[i].getIndice()==edgeIndice)
				return tabEdges[i] ;
		}
		return null;
	}

	/*
	 * Parse a node to a string
	 * in order to write it in a text file to save a problem instance
	 * @param n the node to parse to a string
	 * @return a string representing the node
	 */
	private String toStringForFile(Node n)
	{
		str = new String("Node: ");
		str += n.getIndice() + " ";
		str += n.getX() + " " + n.getY() + " ";
		str += n.getDemand() + " ";
		str += n.getCost() + " ";
		str += n.getTime();
		return str;
	}

	/*
	 * Parse a n edge to a string
	 * in order to write it in a text file to save a problem instance
	 * @param e the edge to parse to a string
	 * @return a string representing the edge
	 */
	private String toStringForFile(Edge e)
	{
		str = new String("Edge: ");
		str += e.getIndice() + " ";
		str += e.getCapacity() + " ";
		str += e.getFixedCost() + " ";
		str += e.getUnitCost() + " ";
		str += e.getTime;
		return str;
	}

	/*
	 * Save the problem in a text file named fileName+".rand.txt"
	 * @param fileName the prefix of the file name
	 */
	public void saveTxt(String fileName) {
		System.out.println("GTransshipmentProblem.saveTxt("+fileName+".rand.txt)") ;
		FichierSortie fs = new FichierSortie (fileName+".rand.txt") ;
		fs.ecrire("fileName" + "\n");
		fs.ecrire("NBR_NODES: " + tabNodes.length + "\n");
		fs.ecrire("NBR_EDGES: " + tabEdges.length + "\n");
		fs.ecrire("T: " + getT() + "\n");

		fs.ecrire("# NODE: i x_i y_i b_i g_i s_i\n");
		fs.ecrire("# x_i, y_i : x, y of node i\n");
		fs.ecrire("# b_i : demand of node_i, <0 for depot, >0 for client, =0 for platforms\n");
		fs.ecrire("# g_i : cost for using node i, >0 only for platforms\n");
		fs.ecrire("# s_i : time for crossing node _i, >0 only for platforms\n");
		for (Node n : tabNodes)
			fs.ecrire(toStringForFile(n) + "\n");

		fs.ecrire("# EDGE: k i j u_ij c_ij h_ij t_ij\n");
		fs.ecrire("# i : starting node of edge k\n");
		fs.ecrire("# j : ending node of edge k\n");
		fs.ecrire("# u_ij : capacity of edge (i,j)\n");
		fs.ecrire("# c_ij : fixed cost for the use of (i,j)\n");
		fs.ecrire("# h_ij : unit cost for (i,j)\n");
		fs.ecrire("# t_ij : delivery time for (i,j)\n");

		for (Edge e : tabEdges)
			fs.ecrire(toStringForFile(e) + "\n");

		fs.ecrire("EOF\n") ;

		fs.fermer() ;

	}

}
