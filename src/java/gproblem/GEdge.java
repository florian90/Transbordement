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

/**
 * 
 * @author Olivier Grunder
 * @version 0.01
 * @date 7 march 2016
 *
 */
public class GEdge {
	/**
	 * Node indice
	 */
	int indice ;
	
	/**
	 * capacity, fixed_cost, unit_cost, time
	 */
	private int u_capacity ;
	private double c_fixedCost, h_unitCost, t_time ;
	
	/**
	 * nodes
	 */
	private GNode startingNode, endingNode ;

	/**
	 * 
	 * @param edgeind
	 * @param nodei : startingNode
	 * @param nodej : endingNode
	 * @param edgeu : u_capacity
	 * @param fixedcost : c_fixedCost
	 * @param unitcost : h_unitCost
	 * @param transptime : t_time
	 */
	public GEdge(int edgeind, GNode nodei, GNode nodej, int edgeu, double fixedcost,
			double unitcost, double transptime) {
		indice = edgeind ;
		startingNode = nodei ;
		endingNode = nodej ;
		u_capacity = edgeu ;
		c_fixedCost = fixedcost ;
		h_unitCost = unitcost ;
		t_time = transptime ;
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
	 * @return the u_capacity
	 */
	public int getCapacity() {
		return u_capacity;
	}

	/**
	 * @param u_capacity the u_capacity to set
	 */
	public void setCapacity(int u_capacity) {
		this.u_capacity = u_capacity;
	}

	/**
	 * @return the c_fixedCost
	 */
	public double getFixedCost() {
		return c_fixedCost;
	}

	/**
	 * @param c_fixedCost the c_fixedCost to set
	 */
	public void setFixedCost(double c_fixedCost) {
		this.c_fixedCost = c_fixedCost;
	}

	/**
	 * @return the h_unitCost
	 */
	public double getUnitCost() {
		return h_unitCost;
	}

	/**
	 * @param h_unitCost the h_unitCost to set
	 */
	public void setUnitCost(double h_unitCost) {
		this.h_unitCost = h_unitCost;
	}

	/**
	 * @return the t_time
	 */
	public double getTime() {
		return t_time;
	}

	/**
	 * @param t_time the t_time to set
	 */
	public void setTime(double t_time) {
		this.t_time = t_time;
	}

	/**
	 * @return the startingNode
	 */
	public GNode getStartingNode() {
		return startingNode;
	}

	/**
	 * @param startingNode the startingNode to set
	 */
	public void setStartingNode(GNode startingNode) {
		this.startingNode = startingNode;
	}

	/**
	 * @return the endingNode
	 */
	public GNode getEndingNode() {
		return endingNode;
	}

	/**
	 * @param endingNode the endingNode to set
	 */
	public void setEndingNode(GNode endingNode) {
		this.endingNode = endingNode;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "GEdge ("+startingNode.getIndice()+","+endingNode.getIndice()+") [u=" + u_capacity
				+ ", c=" + c_fixedCost + ", h=" + h_unitCost
				+ ", t=" + t_time + "]";
	}
	
	

}
