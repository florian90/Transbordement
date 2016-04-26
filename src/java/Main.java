
import gproblem.GTransshipmentProblem;

public class Main {
	public static void main(String args[]) {
		String filename;
		int n;
		if(args.length < 2)
			System.out.println("Need two arguments : the filename and the size of the problem (0<x<500)") ;
		else
		{
			filename = args[0];
			n = new Integer(args[1]);
			GTransshipmentProblem.getRandomInstance(n).save(filename);
		}
	}

}
