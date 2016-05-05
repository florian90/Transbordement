
import gproblem.GTransshipmentProblem;

import java.util.Scanner;

public class Main {
	public static void main(String args[]) {
		String filename;
		int n;
		if(args.length < 2)
		{
			System.out.println("Need two arguments : the filename and the size of the problem (0<x<500)");
			Scanner scanner = new Scanner(System.in);
			System.out.println("Enter the name of the problem :");
			filename = scanner.nextLine();
			System.out.println("Enter the size of the problem :");
			n = scanner.nextInt();
		}
		else
		{
			filename = args[0];
			n = new Integer(args[1]);
		}
		GTransshipmentProblem.getRandomInstance(n).save(filename);
	}

}
