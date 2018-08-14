/* A second C program -- reads and adds a set of values.
*/
#include <stdio.h>

int
main(int argc, char *argv[]) {
	int sum;  /* the running sum */
	int next; /* the next value to be added */

	sum = 0;

	/* get the numbers one by one */
	while (scanf("%d", &next) == 1) {
		sum = sum+next;
	}

	/* and print their sum */
	printf("The sum of the numbers is %d\n", sum);
	return 0;
}

/* =====================================================================
   Program written by Alistair Moffat, as an example for the book
   "Programming, Problem Solving, and Abstraction with C", Pearson
   Custom Books, Sydney, Australia, 2002; revised edition 2012,
   ISBN 9781486010974.

   See http://people.eng.unimelb.edu.au/ammoffat/ppsaa/ for further
   information.

   Prepared December 2012 for the Revised Edition.
   ================================================================== */
