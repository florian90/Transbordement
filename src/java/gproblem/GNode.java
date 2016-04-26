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

import java.util.Arrays;
import java.util.Vector;

/**
 * 
 * @author Olivier Grunder
 * @version 0.01
 * @date 7 march 2016
 *
 */
public class GNode {
	/**
	 * Node indice
	 */
	int indice ;
	
	/**
	 * Coordinates of node
	 */
	private int x, y ;
	
	/**
	 * Demand of the node : >0 for customer (final nodes), <0 for depot (initial nodes), =0 for platforms (intermediary nodes)
	 */
	private int b_demand ;
	
	/**
	 * Cost/time for using this node => only for platforms
	 */
	private double g_cost, s_time ;
	
	/**
	 * edges starting from node
	 */
	private GEdge tabEdges[] ;
	
	
	
	/**
	 * 
	 * @param nodeind
	 * @param nodex : int x
	 * @param nodey : int y 
	 * @param nodeb : int demand
	 * @param unitcost : double cost
	 * @param setup : double time
	 */
	public GNode(int nodeind, int nodex, int nodey, int nodeb, double unitcost, double setup) {
		indice = nodeind ;
		x = nodex ;
		y = nodey ;
		b_demand = nodeb ;
		g_cost = unitcost ;
		s_time = setup ;
		tabEdges = null ;
	}

	/**
	 * @return the indice
	 */
	public int getIndice() {
		return indice;
	}

	/**
	 * @param indice the indice to set
	 */
	public void setIndice(int indice) {
		this.indice = indice;
	}

	/**
	 * @return the x
	 */
	public int getX() {
		return x;
	}

	/**
	 * @param x the x to set
	 */
	public void setX(int x) {
		this.x = x;
	}

	/**
	 * @return the y
	 */
	public int getY() {
		return y;
	}

	/**
	 * @param y the y to set
	 */
	public void setY(int y) {
		this.y = y;
	}

	/**
	 * @return the b_demand
	 */
	public int getDemand() {
		return b_demand;
	}

	/**
	 * @param b_demand the b_demand to set
	 */
	public void setDemand(int b_demand) {
		this.b_demand = b_demand;
	}

	/**
	 * @return the g_cost
	 */
	public double getCost() {
		return g_cost;
	}

	/**
	 * @param g_cost the g_cost to set
	 */
	public void setCost(double g_cost) {
		this.g_cost = g_cost;
	}

	/**
	 * @return the s_time
	 */
	public double getTime() {
		return s_time;
	}

	/**
	 * @param s_time the s_time to set
	 */
	public void setTime(double s_time) {
		this.s_time = s_time;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (b_demand==0) { // platform
			return "GNode "+indice+" [platform, g=" + g_cost + ", s=" + s_time + "]";
		}
		else if (b_demand<0) { // depot
			return "GNode "+indice+" [depot, b=" + b_demand + "]";
		}
		// client
		return "GNode "+indice+" [client, b=" + b_demand + "]";
	}

	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toStringDebug() {
		String tabEdgesSt="" ;
		for (int i=0;i<tabEdges.length;i++)
			tabEdgesSt = tabEdgesSt+"edge "+tabEdges[i].getIndice()+"," ;
		
		if (b_demand==0) { // platform
			return "GNode "+indice+" [platform, g=" + g_cost + ", s=" + s_time + ", tabEdges="+tabEdgesSt+ "]";
		}
		else if (b_demand<0) { // depot
			return "GNode "+indice+" [depot, b=" + b_demand + ", tabEdges="+tabEdgesSt+ "]";
		}
		// client
		return "GNode "+indice+" [client, b=" + b_demand + ", tabEdges="+tabEdgesSt+ "]";
	}

	/**
	 * 
	 * @param n
	 */
	public void setNbrEdges(int n) {
		tabEdges = new GEdge[n] ;		
	}

	/**
	 * 
	 * @param n
	 */
	public int getNbrEdges() {
		return tabEdges.length ;		
	}

	/**
	 * 
	 * @param i
	 * @param edge
	 */
	public void setEdge(int i, GEdge edge) {
		tabEdges[i] = edge ;
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public GEdge getEdge(int i) {
		return tabEdges[i];
	}
	
	/**
	 * 
	 * @param i
	 * @return
	 */
	public int getEdgeIndice(int i) {
		assert tabEdges!=null : "getEdgeIndice(): tabEdges="+tabEdges ;
		return tabEdges[i].getIndice();
	}

	public GEdge[] getTabEdges() {
		return tabEdges;
	}

	public boolean isDepot() {
		return b_demand<0;
	}
	public boolean isPlatform() {
		return b_demand==0;
	}
	public boolean isClient() {
		return b_demand>0;
	}
	
	

}
