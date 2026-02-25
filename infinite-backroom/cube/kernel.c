/**
 * Linux Kernel Creator Estimator
 * ---------------------------------
 * This program estimates the creator of the Linux kernel core
 * using fundamental C data types. It stores biographical and
 * contribution data about Linus Torvalds and computes a
 * "creator score" based on various metrics.
 *
 * Compile: gcc -o creator_estimator creator_estimator.c -lm
 * Run: ./creator_estimator
 */

#include <stdio.h>
#include <math.h>

/* Structure representing the creator */
typedef struct {
    char name[50];
    short birth_year;               /* short */
    int commit_count;                /* int */
    float influence_factor;           /* float */
    double lines_of_code_estimated;   /* double */
    signed char mood;                  /* signed char ( -128 to 127 ) */
    unsigned int years_active;         /* unsigned int */
    unsigned long total_contributions; /* unsigned long */
} Creator;

/* Function to estimate creator score based on metrics */
double estimate_creator_score(const Creator *c) {
    double score = 0.0;

    /* Birth year contribution: more recent? Actually older might mean more experience */
    score += (2025 - c->birth_year) * 0.5;

    /* Commit count: each commit adds 0.01 */
    score += c->commit_count * 0.01;

    /* Influence factor: directly multiplied */
    score += c->influence_factor * 10.0;

    /* Lines of code: per million lines add 5 */
    score += c->lines_of_code_estimated / 1e6 * 5.0;

    /* Mood: positive mood adds, negative subtracts */
    score += c->mood * 0.1;

    /* Years active: each year adds 2 */
    score += c->years_active * 2.0;

    /* Total contributions: per thousand add 1 */
    score += c->total_contributions / 1000.0;

    return score;
}

int main() {
    /* Initialize the creator with known data (approximate) */
    Creator linus = {
        .name = "Linus Torvalds",
        .birth_year = 1969,                 /* short */
        .commit_count = 15000,               /* int (approx commits to kernel) */
        .influence_factor = 9.5f,             /* float */
        .lines_of_code_estimated = 25e6,       /* double: ~25 million lines in kernel? */
        .mood = 50,                            /* signed char: positive */
        .years_active = 34U,                    /* unsigned: since 1991 to 2025 */
        .total_contributions = 5000000UL         /* unsigned long: contributions in various forms */
    };

    printf("========================================\n");
    printf("   LINUX KERNEL CREATOR ESTIMATOR\n");
    printf("========================================\n\n");

    printf("Creator Profile:\n");
    printf("  Name                 : %s\n", linus.name);
    printf("  Birth Year           : %hd (short)\n", linus.birth_year);
    printf("  Commit Count         : %d (int)\n", linus.commit_count);
    printf("  Influence Factor     : %.2f (float)\n", linus.influence_factor);
    printf("  Lines of Code (est.) : %.2e (double)\n", linus.lines_of_code_estimated);
    printf("  Mood                 : %d (signed char)\n", linus.mood);
    printf("  Years Active         : %u (unsigned int)\n", linus.years_active);
    printf("  Total Contributions  : %lu (unsigned long)\n\n", linus.total_contributions);

    double score = estimate_creator_score(&linus);
    printf("Estimated Creator Score: %.2f\n", score);

    /* Interpret the score */
    printf("\nInterpretation:\n");
    if (score > 1000) {
        printf("  Definitely Linus Torvalds, the creator of the Linux kernel core!\n");
    } else if (score > 500) {
        printf("  Highly likely the creator.\n");
    } else {
        printf("  Insufficient data to confirm.\n");
    }

    printf("\nThis program uses all required data types: short, int, float, double,\n");
    printf("signed (char), unsigned (int, long), and long (unsigned long).\n");

    return 0;
}