package misc;

import java.util.Arrays;

public class AdvOfCode2020Day23 {

	public static void main(String... args) {
		CupsProblem ex1 = new CupsProblem(9, 10, new int[] { 3, 8, 9, 1, 2, 5, 4, 6, 7 });
		ex1.simulate();
		readPart1(ex1);
		
		CupsProblem part1 = new CupsProblem(9, 100, new int[] { 2, 4, 7, 8, 1, 9, 3, 5, 6 });
		part1.simulate();
		readPart1(part1);
		
		CupsProblem ex2 = new CupsProblem(1_000_000, 10_000_000, new int[] { 3, 8, 9, 1, 2, 5, 4, 6, 7 });
		ex2.simulate();
		readPart2(ex2);
		
		CupsProblem part2 = new CupsProblem(1_000_000, 10_000_000, new int[] { 2, 4, 7, 8, 1, 9, 3, 5, 6 });
		part2.simulate();
		readPart2(part2);
		
	}

	private static void readPart1(CupsProblem p) {
		System.out.println("Read ex/p1 : ");
		int[] cups = p.cups;

		int cup = cups[1 - 1]; // The cups that follows one
		do {
			System.out.print(cup + " ");

			cup = cups[cup - 1];
		} while(cup != 1); // Stop when looping back to cup 1

		System.out.println();
	}

	private static void readPart2(CupsProblem p) {
		System.out.println("Part2 : ");
		int[] cups = p.cups;
		int a = cups[1 - 1];
		int b = cups[a - 1];
		System.out.println(a);
		System.out.println(b);
		System.out.println(((long) a) * ((long) b));
	}

}

class CupsProblem {
	private static boolean trace = false;

	private final int size;
	private final int moves;

	private int head;
	private int move = 1;

	// This array represents a map of CupID to which cup follows it.
	// For a given cup C, then the value of cups[C-1] is the following cup.
	// So for example if the circle is 3,1,2
	// cups[3-1] = 1, cups[1-1] = 2, cups[2-1] = 0 (it loops)
	// So the array would be [2,0,1]
	final int[] cups;

	CupsProblem(int size, int moves, int[] seed) {
		this.size = size;
		this.moves = moves;

		cups = new int[size];

		head = seed[0];

		// Read seed into array
		int prevCup = -1;
		for(int cup : seed) {

			if(prevCup != -1) {
				cups[prevCup - 1] = cup;
			}

			prevCup = cup;
		}

		// Complete the rest of the array
		for(int cup = seed.length + 1; cup <= size; cup++) {
			cups[prevCup - 1] = cup;

			prevCup = cup;
		}

		// Close the circle
		cups[prevCup - 1] = head;

		if(trace)
			System.out.println(Arrays.toString(cups));
	}

	public void simulate() {
		for(int i = 0; i < moves; i++) {
			// if(moves >= 100000000 && i % 1000000 == 0) {
			// System.out.println(i / 1000000 + "%");
			// }

			shuffle();
		}

		trace();
	}

	private void shuffle() {
		if(trace)
			System.out.println("-- move " + move + " --");
		trace();

		// The 3 items to remove
		int a = cups[head - 1];
		int b = cups[a - 1];
		int c = cups[b - 1];

		int dest = findDest(a, b, c);

		if(trace)
			System.out.println("pick up: " + a + " " + b + " " + c);

		if(trace)
			System.out.println("dest: " + dest);

		// Remove the items, the cup after head is now the one that was after c
		cups[head - 1] = cups[c - 1];

		// Insert the items after dest
		int afterDest = cups[dest - 1];
		cups[dest - 1] = a;
		cups[c - 1] = afterDest;

		// Pick a new head
		head = cups[head - 1];

		if(trace)
			System.out.println();
		move++;
	}

	private int findDest(int a, int b, int c) {
		int candidate = head;

		while(true) {
			candidate--;

			if(candidate == 0) {
				candidate = size + 1;
				continue;
			}

			if(candidate == a || candidate == b || candidate == c) {
				continue;
			}

			break;
		}

		return candidate;
	}

	private void trace() {
		if(!trace)
			return;

		for(int c : cups) {
			System.out.print(c);
		}

		System.out.println(" // " + head);
	}

	static int indexOf(int[] array, int val) {
		for(int i = 0; i < array.length; i++) {
			if(array[i] == val)
				return i;
		}

		throw new IllegalStateException("indexOf couldn't find value");
	}

}
