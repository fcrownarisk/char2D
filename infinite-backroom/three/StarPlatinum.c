/**
 * ============================================================================
 * JoJo's Bizarre Adventure: Star Platinum Stand Simulation in C
 * ----------------------------------------------------------------------------
 * This program simulates the Stand "Star Platinum" (used by Jotaro Kujo)
 * by mapping its characteristics to fundamental C data types:
 *
 *   - short       : Speed (0..32767) – reflects the Stand's lightning-fast punches.
 *   - int         : Power (base destructive force) – can be negative if weakened.
 *   - float       : Precision (0.0f to 1.0f) – ability to hit tiny targets.
 *   - double      : Time stop duration (seconds) – high precision for stop time.
 *   - signed      : Alignment ( -100 = evil, +100 = good) – Jotaro is +100.
 *   - unsigned    : Health, stamina, durability (non-negative quantities).
 *   - long        : Experience points / battle count – can become very large.
 *
 * The simulation includes:
 *   - A battle system where the user (Jotaro) fights random enemy Stands.
 *   - Realistic use of all data types in calculations, comparisons, and I/O.
 *   - Detailed output of each action, showing how the types influence outcomes.
 *
 * Compile with: gcc -o star_platinum star_platinum.c -lm
 * Run: ./star_platinum
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <ctype.h>

/* ========================================================================== */
/* CONSTANTS & MACROS                                                         */
/* ========================================================================== */

#define MAX_NAME_LEN         50
#define MAX_ENEMIES          10
#define MAX_ACTIONS          100
#define BASE_PUNCH_DAMAGE    20
#define ORA_RUSH_MULTIPLIER  3
#define TIME_STOP_COST       30      /* stamina cost to stop time */
#define BULLET_SPEED         1000.0f /* fictional bullet speed (m/s) */

/* ========================================================================== */
/* TYPE DEFINITIONS – each uses the required keywords                         */
/* ========================================================================== */

/* Character structure (can be Jotaro or an enemy) */
typedef struct {
    char name[MAX_NAME_LEN];
    unsigned long health;           /* health points, never negative */
    unsigned int stamina;           /* energy for special moves */
    unsigned long experience;       /* total battles won (long) */
    signed char alignment;          /* -128..127, but we use -100..100 */
} Character;

/* Stand structure – Star Platinum or any other Stand */
typedef struct {
    char name[MAX_NAME_LEN];
    short speed;                    /* 0..32767 – higher is faster */
    int power;                       /* base attack power (can be negative if cursed) */
    float precision;                 /* 0.0f to 1.0f – accuracy multiplier */
    double timeStopDuration;         /* seconds of stopped time */
    unsigned long durability;        /* remaining "stand energy" */
    /* no separate signed member here because alignment is in Character */
} Stand;

/* A combined entity for a Stand user */
typedef struct {
    Character user;
    Stand stand;
} StandUser;

/* ========================================================================== */
/* GLOBAL VARIABLES (use unsigned for counts, signed for turn indicator)      */
/* ========================================================================== */

static unsigned int battleCount = 0;          /* number of battles fought */
static signed int currentTurn = 1;             /* 1 = player, -1 = enemy (just for demo) */
static const double TIME_STOP_BASE = 2.5;      /* base time stop seconds (double) */

/* ========================================================================== */
/* FUNCTION PROTOTYPES                                                        */
/* ========================================================================== */

void initializeStarPlatinum(StandUser *jotaro);
void initializeRandomEnemy(StandUser *enemy, int level);
void printStats(const StandUser *su);
void printSeparator(void);
int battleMenu(void);
void playerTurn(StandUser *player, StandUser *enemy);
void enemyTurn(StandUser *player, StandUser *enemy);
int performPunch(StandUser *attacker, StandUser *defender);
int performOraRush(StandUser *attacker, StandUser *defender);
int performTimeStop(StandUser *attacker, StandUser *defender);
int performCatchBullet(StandUser *attacker);
void applyDamage(StandUser *target, unsigned long damage);
int isDefeated(const StandUser *su);
void simulateBattle(StandUser *player, StandUser *enemy);
void printWelcome(void);
void printAnalysis(void);

/* ========================================================================== */
/* MAIN FUNCTION – entry point                                                */
/* ========================================================================== */

int main(void) {
    srand((unsigned)time(NULL));   /* seed random number generator */

    printWelcome();
    printAnalysis();

    StandUser jotaro;
    initializeStarPlatinum(&jotaro);

    StandUser enemy;
    int choice;
    int playing = 1;

    while (playing) {
        printf("\n=== MAIN MENU ===\n");
        printf("1. Start new battle\n");
        printf("2. View Jotaro's stats\n");
        printf("3. View analysis of data types used\n");
        printf("4. Exit\n");
        printf("Choice: ");
        scanf("%d", &choice);
        while (getchar() != '\n'); /* flush */

        switch (choice) {
            case 1:
                /* Generate a random enemy based on battle count (experience) */
                initializeRandomEnemy(&enemy, (int)jotaro.user.experience);
                simulateBattle(&jotaro, &enemy);
                battleCount++;
                break;
            case 2:
                printStats(&jotaro);
                break;
            case 3:
                printAnalysis();
                break;
            case 4:
                playing = 0;
                printf("Thanks for playing! Ora Ora Ora!\n");
                break;
            default:
                printf("Invalid choice.\n");
        }
    }

    return 0;
}

/* ========================================================================== */
/* INITIALIZATION FUNCTIONS                                                   */
/* ========================================================================== */

/**
 * Initializes Jotaro and Star Platinum with values that reflect the anime.
 * All required data types are used here.
 */
void initializeStarPlatinum(StandUser *jotaro) {
    /* Character: Jotaro Kujo */
    strcpy(jotaro->user.name, "Jotaro Kujo");
    jotaro->user.health = 1000UL;                /* unsigned long */
    jotaro->user.stamina = 100U;                  /* unsigned int */
    jotaro->user.experience = 0UL;                 /* starts at zero */
    jotaro->user.alignment = 100;                  /* signed char (good) */

    /* Stand: Star Platinum */
    strcpy(jotaro->stand.name, "Star Platinum");
    jotaro->stand.speed = 30000;                   /* short – extremely fast */
    jotaro->stand.power = 150;                      /* int – A rank power */
    jotaro->stand.precision = 0.99f;                 /* float – near perfect */
    jotaro->stand.timeStopDuration = TIME_STOP_BASE; /* double – initially 2.5s */
    jotaro->stand.durability = 5000UL;               /* unsigned long */

    printf("Star Platinum has been awakened!\n");
}

/**
 * Creates a random enemy Stand user.
 * The enemy's stats scale with the player's experience (long used in calculation).
 */
void initializeRandomEnemy(StandUser *enemy, int level) {
    /* Generate a random enemy name */
    const char *names[] = {"DIO", "Hol Horse", "Vanilla Ice", "Pet Shop", "Anubis", "N'Doul"};
    int idx = rand() % (sizeof(names)/sizeof(names[0]));
    strcpy(enemy->user.name, names[idx]);

    /* Health scales with level (unsigned long) */
    enemy->user.health = 500UL + (unsigned long)(rand() % 500) + (unsigned long)level * 10UL;
    enemy->user.stamina = 50U + (unsigned int)(rand() % 50);
    enemy->user.experience = (unsigned long)(rand() % 100);   /* random exp */
    enemy->user.alignment = (signed char)(rand() % 201 - 100); /* -100 to 100 */

    /* Stand attributes */
    char standName[MAX_NAME_LEN];
    sprintf(standName, "%s's Stand", enemy->user.name);
    strcpy(enemy->stand.name, standName);
    enemy->stand.speed = (short)(rand() % 20000 + 5000);         /* short */
    enemy->stand.power = (int)(rand() % 100 + 20);               /* int */
    enemy->stand.precision = (float)(rand() % 1000) / 1000.0f;   /* float 0..1 */
    enemy->stand.timeStopDuration = (double)(rand() % 20) / 10.0; /* double 0..2.0 */
    enemy->stand.durability = (unsigned long)(rand() % 3000 + 1000);
}

/* ========================================================================== */
/* STATISTICS DISPLAY                                                         */
/* ========================================================================== */

void printStats(const StandUser *su) {
    printSeparator();
    printf("USER: %s\n", su->user.name);
    printf("  Health     : %lu (unsigned long)\n", su->user.health);
    printf("  Stamina    : %u (unsigned int)\n", su->user.stamina);
    printf("  Experience : %lu (unsigned long)\n", su->user.experience);
    printf("  Alignment  : %d (signed char)\n", su->user.alignment);
    printf("STAND: %s\n", su->stand.name);
    printf("  Speed      : %hd (short) – ", su->stand.speed);
    if (su->stand.speed > 25000) printf("Lightning fast!\n");
    else if (su->stand.speed > 15000) printf("Very fast\n");
    else printf("Average\n");

    printf("  Power      : %d (int) – ", su->stand.power);
    if (su->stand.power > 120) printf("Destructive!\n");
    else if (su->stand.power > 70) printf("Strong\n");
    else printf("Weak\n");

    printf("  Precision  : %.3f (float) – ", su->stand.precision);
    if (su->stand.precision > 0.9f) printf("Pinpoint accuracy\n");
    else if (su->stand.precision > 0.7f) printf("Good\n");
    else printf("Poor\n");

    printf("  Time Stop  : %.2lf seconds (double)\n", su->stand.timeStopDuration);
    printf("  Durability : %lu (unsigned long)\n", su->stand.durability);
    printSeparator();
}

void printSeparator(void) {
    printf("------------------------------------------------------------\n");
}

/* ========================================================================== */
/* BATTLE MECHANICS                                                           */
/* ========================================================================== */

/**
 * Main battle loop. Uses signed/unsigned comparisons, data type conversions.
 */
void simulateBattle(StandUser *player, StandUser *enemy) {
    printf("\n>>> BATTLE START: %s (Star Platinum) vs %s <<<\n",
           player->user.name, enemy->user.name);
    printStats(player);
    printStats(enemy);

    int turn = 1;   /* 1 = player, 0 = enemy (just for alternation) */
    int battleOver = 0;
    int actionChoice;

    while (!battleOver) {
        if (turn) {
            /* Player's turn */
            printf("\n--- Your Turn ---\n");
            actionChoice = battleMenu();
            switch (actionChoice) {
                case 1:  /* Punch */
                    performPunch(player, enemy);
                    break;
                case 2:  /* Ora Rush */
                    performOraRush(player, enemy);
                    break;
                case 3:  /* Time Stop */
                    performTimeStop(player, enemy);
                    break;
                case 4:  /* Catch Bullet (defensive) */
                    performCatchBullet(player);
                    printf("You focus on defense, ready to catch bullets.\n");
                    break;
                default:
                    printf("Invalid action! You hesitate.\n");
            }
        } else {
            /* Enemy's turn (simple AI) */
            enemyTurn(player, enemy);
        }

        /* Check defeat conditions */
        if (isDefeated(enemy)) {
            printf("\n*** %s has been defeated! ***\n", enemy->user.name);
            player->user.experience += 100UL;   /* gain experience */
            if (player->user.health > 1000UL) player->user.health = 1000UL; /* cap */
            battleOver = 1;
        } else if (isDefeated(player)) {
            printf("\n*** %s has been defeated... ***\n", player->user.name);
            battleOver = 1;
        }

        turn = !turn;   /* switch turns */
    }

    printf("\n>>> BATTLE END <<<\n");
}

/**
 * Displays battle menu and returns choice.
 */
int battleMenu(void) {
    int choice;
    printf("Choose action:\n");
    printf("  1. Punch (normal attack)\n");
    printf("  2. Ora Rush (multiple punches, costs stamina)\n");
    printf("  3. Time Stop (stop time and attack, costs stamina)\n");
    printf("  4. Catch Bullet (defensive, raises precision)\n");
    printf("Choice: ");
    scanf("%d", &choice);
    while (getchar() != '\n');
    return choice;
}

/**
 * Enemy's turn – picks a random action.
 */
void enemyTurn(StandUser *player, StandUser *enemy) {
    printf("\n--- Enemy Turn (%s) ---\n", enemy->user.name);
    int action = rand() % 4;  /* 0-3 */

    /* Use signed/unsigned in decision making */
    if (enemy->user.alignment < 0) { /* evil Stands are more aggressive */
        action = rand() % 3; /* more likely to attack */
    }

    switch (action) {
        case 0:
        case 1:
            printf("%s attacks with a punch!\n", enemy->stand.name);
            performPunch(enemy, player);
            break;
        case 2:
            if (enemy->user.stamina >= 20) {
                printf("%s attempts a rush attack!\n", enemy->stand.name);
                /* enemy's ora rush equivalent – simplified */
                int hits = rand() % 5 + 3; /* 3-7 hits */
                unsigned long totalDamage = 0;
                for (int i = 0; i < hits; i++) {
                    totalDamage += (unsigned long)(enemy->stand.power * 0.5f);
                }
                applyDamage(player, totalDamage);
                enemy->user.stamina -= 20U;
                printf("%s lands %d hits for %lu damage!\n", enemy->stand.name, hits, totalDamage);
            } else {
                printf("%s tries to attack but is too tired.\n", enemy->stand.name);
            }
            break;
        case 3:
            if (enemy->stand.timeStopDuration > 1.0 && enemy->user.stamina >= 25) {
                printf("%s attempts to stop time!\n", enemy->stand.name);
                /* enemy time stop effect: guaranteed one attack */
                unsigned long damage = (unsigned long)(enemy->stand.power * 1.5);
                applyDamage(player, damage);
                enemy->user.stamina -= 25U;
                printf("%s lands a free hit during stopped time for %lu damage!\n",
                       enemy->stand.name, damage);
            } else {
                printf("%s tries a normal punch.\n", enemy->stand.name);
                performPunch(enemy, player);
            }
            break;
    }
}

/**
 * Performs a single punch. Damage = (power * precision) + speed/100.
 * Uses all data types in calculation.
 */
int performPunch(StandUser *attacker, StandUser *defender) {
    printf("%s's %s throws a punch!\n", attacker->user.name, attacker->stand.name);

    /* Calculate base damage – using int, float, short, and casting */
    float base = (float)attacker->stand.power * attacker->stand.precision;
    /* Add speed contribution (short to float) */
    float speedBonus = (float)attacker->stand.speed / 100.0f;
    float totalFloat = base + speedBonus;

    /* Convert to unsigned long for damage (health is unsigned long) */
    unsigned long damage = (unsigned long)(totalFloat + 0.5f); /* rounding */

    /* Random variation (use signed rand, then take abs) */
    int variation = (rand() % 21) - 10; /* -10 to 10 */
    if (variation < 0) {
        /* Avoid underflow by checking */
        if (damage > (unsigned long)(-variation))
            damage -= (unsigned long)(-variation);
        else
            damage = 1UL;
    } else {
        damage += (unsigned long)variation;
    }

    applyDamage(defender, damage);
    printf("Damage dealt: %lu\n", damage);
    return 1;
}

/**
 * Ora Rush – multiple punches. Uses stamina (unsigned) and durability (unsigned long).
 */
int performOraRush(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < 20U) {
        printf("Not enough stamina! (need 20, have %u)\n", attacker->user.stamina);
        return 0;
    }
    attacker->user.stamina -= 20U;

    printf("%s cries out: ORA ORA ORA!!!\n", attacker->user.name);
    int punches = (int)(attacker->stand.speed / 1000);  /* speed influences number */
    if (punches < 3) punches = 3;
    if (punches > 20) punches = 20;

    unsigned long totalDamage = 0UL;
    for (int i = 0; i < punches; i++) {
        /* Each punch does a fraction of base power */
        unsigned long punchDamage = (unsigned long)(attacker->stand.power * 0.3f);
        totalDamage += punchDamage;
    }

    applyDamage(defender, totalDamage);
    printf("%s lands %d punches for %lu total damage!\n",
           attacker->stand.name, punches, totalDamage);
    return 1;
}

/**
 * Time Stop – freezes time, allows one free attack.
 * Uses double for duration, and checks stamina (unsigned).
 */
int performTimeStop(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < TIME_STOP_COST) {
        printf("Not enough stamina to stop time! (need %d)\n", TIME_STOP_COST);
        return 0;
    }
    attacker->user.stamina -= TIME_STOP_COST;

    /* Duration can be increased by experience (unsigned long) */
    double effectiveDuration = attacker->stand.timeStopDuration +
                               (double)attacker->user.experience / 100.0;
    if (effectiveDuration > 10.0) effectiveDuration = 10.0; /* safety cap */

    printf("%s shouts: 'THE WORLD! Time has stopped!'\n", attacker->user.name);
    printf("Time stops for %.2lf seconds!\n", effectiveDuration);

    /* During stopped time, attacker can perform a single powerful blow */
    unsigned long damage = (unsigned long)(attacker->stand.power * 2.0f);
    applyDamage(defender, damage);

    printf("%s attacks freely during stopped time, dealing %lu damage!\n",
           attacker->stand.name, damage);
    return 1;
}

/**
 * Catch Bullet – uses precision (float) and speed (short) to intercept.
 * Returns 1 if successful, else 0.
 */
int performCatchBullet(StandUser *attacker) {
    printf("%s attempts to catch a bullet with Star Platinum!\n", attacker->user.name);

    /* Calculate catch probability using precision and speed */
    float catchProb = attacker->stand.precision * 0.8f +
                      (float)attacker->stand.speed / 50000.0f;
    if (catchProb > 1.0f) catchProb = 1.0f;

    float roll = (float)rand() / (float)RAND_MAX; /* 0..1 */

    if (roll < catchProb) {
        printf("Amazing! Star Platinum catches the bullet between two fingers!\n");
        /* Successful catch may restore a bit of stamina */
        attacker->user.stamina += 5U;
        if (attacker->user.stamina > 100U) attacker->user.stamina = 100U;
        return 1;
    } else {
        printf("The bullet grazes you... but you avoid serious injury.\n");
        /* Still take some damage because not full catch */
        applyDamage(attacker, 10UL);
        return 0;
    }
}

/**
 * Applies damage to a target, ensuring health never goes below zero.
 * Uses unsigned long for health.
 */
void applyDamage(StandUser *target, unsigned long damage) {
    if (damage >= target->user.health) {
        target->user.health = 0UL;
    } else {
        target->user.health -= damage;
    }
    printf("%s takes %lu damage. Health now %lu.\n",
           target->user.name, damage, target->user.health);
}

/**
 * Checks if a Stand user is defeated (health == 0).
 */
int isDefeated(const StandUser *su) {
    return (su->user.health == 0UL);
}

/* ========================================================================== */
/* ANALYSIS SECTION – explains data type usage in the context of JoJo        */
/* ========================================================================== */

void printWelcome(void) {
    printf("\n");
    printf("============================================================\n");
    printf("   STAR PLATINUM: STAND SIMULATION in C\n");
    printf("   Based on JoJo's Bizarre Adventure Part 3\n");
    printf("============================================================\n");
}

void printAnalysis(void) {
    printSeparator();
    printf("DATA TYPE ANALYSIS (as used in this simulation):\n");
    printSeparator();

    printf("1. short:\n");
    printf("   - Used for Stand speed (0..32767).\n");
    printf("   - Star Platinum's speed is 30000, reflecting its 'lightning fast' nature.\n");
    printf("   - In battle, speed contributes to the number of ORA punches.\n\n");

    printf("2. int:\n");
    printf("   - Stand power (base destructive force).\n");
    printf("   - Can be negative if a Stand is weakened or cursed.\n");
    printf("   - Power directly influences damage calculations.\n\n");

    printf("3. float:\n");
    printf("   - Precision (0.0 to 1.0). Star Platinum's precision is 0.99.\n");
    printf("   - Used to determine accuracy and special moves like catching bullets.\n");
    printf("   - Float allows fractional precision, important for probability.\n\n");

    printf("4. double:\n");
    printf("   - Time stop duration in seconds. Star Platinum can initially stop time for 2.5s.\n");
    printf("   - Double provides high precision for gradual improvements via experience.\n");
    printf("   - Example: effectiveDuration = base + (experience / 100.0).\n\n");

    printf("5. signed:\n");
    printf("   - Implemented as 'signed char' for alignment (range -128 to 127).\n");
    printf("   - Jotaro's alignment = +100 (good); enemies can be evil (negative).\n");
    printf("   - Signed types allow representation of opposing forces.\n\n");

    printf("6. unsigned:\n");
    printf("   - Health, stamina, durability, experience: never negative.\n");
    printf("   - 'unsigned long' for health and durability (large range).\n");
    printf("   - 'unsigned int' for stamina (0..65535).\n");
    printf("   - Unsigned types ensure these attributes cannot become negative.\n\n");

    printf("7. long:\n");
    printf("   - Experience points and durability are 'unsigned long'.\n");
    printf("   - Long (32-bit or 64-bit) allows values to grow large over many battles.\n");
    printf("   - Used in time stop duration scaling: (double)experience/100.0.\n");

    printSeparator();
    printf("All keywords appear in declarations, casts, and operations throughout\n");
    printf("the code. The simulation demonstrates how each type models a unique\n");
    printf("aspect of Star Platinum's abilities.\n");
    printSeparator();
}

/* ========================================================================== */
/* End of program                                                             */
/* ========================================================================== */