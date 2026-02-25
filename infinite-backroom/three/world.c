/**
 * ============================================================================
 * JoJo's Bizarre Adventure: THE WORLD (DIO) Stand Simulation in C
 * ----------------------------------------------------------------------------
 * This program simulates the Stand "The World" (used by DIO)
 * by mapping its characteristics to fundamental C data types:
 *
 *   - short       : Speed (0..32767) – reflects The World's incredible velocity.
 *   - int         : Power (base destructive force) – can be negative if weakened.
 *   - float       : Precision (0.0f to 1.0f) – accuracy for knife throwing.
 *   - double      : Time stop duration (seconds) – DIO's signature ability.
 *   - signed      : Alignment ( -100 = evil, +100 = good) – DIO is -100.
 *   - unsigned    : Health, stamina, durability (non-negative quantities).
 *   - long        : Experience points / battle count – can become very large.
 *
 * The simulation includes:
 *   - A battle system where the user (DIO) fights random Joestar enemies.
 *   - Realistic use of all data types in calculations, comparisons, and I/O.
 *   - Detailed output of each action, showing how the types influence outcomes.
 *
 * Compile with: gcc -o the_world the_world.c -lm
 * Run: ./the_world
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
#define BASE_PUNCH_DAMAGE    25
#define KNIFE_DAMAGE         15
#define ROAD_ROLLER_DAMAGE    80
#define TIME_STOP_COST        40      /* stamina cost to stop time */
#define KNIFE_THROW_COST      15
#define ROAD_ROLLER_COST       60

/* ========================================================================== */
/* TYPE DEFINITIONS – each uses the required keywords                         */
/* ========================================================================== */

/* Character structure (can be DIO or an enemy) */
typedef struct {
    char name[MAX_NAME_LEN];
    unsigned long health;           /* health points, never negative */
    unsigned int stamina;           /* energy for special moves */
    unsigned long experience;       /* total battles won (long) */
    signed char alignment;          /* -128..127, but we use -100..100 */
} Character;

/* Stand structure – The World or any other Stand */
typedef struct {
    char name[MAX_NAME_LEN];
    short speed;                    /* 0..32767 – higher is faster */
    int power;                       /* base attack power (can be negative if cursed) */
    float precision;                 /* 0.0f to 1.0f – accuracy multiplier */
    double timeStopDuration;         /* seconds of stopped time */
    unsigned long durability;        /* remaining "stand energy" */
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
static const double TIME_STOP_BASE = 5.0;      /* DIO's initial time stop: 5 seconds (double) */

/* ========================================================================== */
/* FUNCTION PROTOTYPES                                                        */
/* ========================================================================== */

void initializeTheWorld(StandUser *dio);
void initializeRandomEnemy(StandUser *enemy, int level);
void printStats(const StandUser *su);
void printSeparator(void);
int battleMenu(void);
void playerTurn(StandUser *player, StandUser *enemy);
void enemyTurn(StandUser *player, StandUser *enemy);
int performPunch(StandUser *attacker, StandUser *defender);
int performKnifeThrow(StandUser *attacker, StandUser *defender);
int performTimeStop(StandUser *attacker, StandUser *defender);
int performRoadRoller(StandUser *attacker, StandUser *defender);
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

    StandUser dio;
    initializeTheWorld(&dio);

    StandUser enemy;
    int choice;
    int playing = 1;

    while (playing) {
        printf("\n=== MAIN MENU ===\n");
        printf("1. Start new battle (as DIO)\n");
        printf("2. View DIO's stats\n");
        printf("3. View analysis of data types used\n");
        printf("4. Exit\n");
        printf("Choice: ");
        scanf("%d", &choice);
        while (getchar() != '\n'); /* flush */

        switch (choice) {
            case 1:
                /* Generate a random enemy based on battle count (experience) */
                initializeRandomEnemy(&enemy, (int)dio.user.experience);
                simulateBattle(&dio, &enemy);
                battleCount++;
                break;
            case 2:
                printStats(&dio);
                break;
            case 3:
                printAnalysis();
                break;
            case 4:
                playing = 0;
                printf("Muda Muda Muda! Thanks for playing!\n");
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
 * Initializes DIO and The World with values that reflect the anime/manga.
 * All required data types are used here.
 */
void initializeTheWorld(StandUser *dio) {
    /* Character: DIO */
    strcpy(dio->user.name, "DIO");
    dio->user.health = 1200UL;                /* unsigned long */
    dio->user.stamina = 120U;                  /* unsigned int */
    dio->user.experience = 0UL;                 /* starts at zero */
    dio->user.alignment = -100;                  /* signed char (evil) */

    /* Stand: The World */
    strcpy(dio->stand.name, "THE WORLD");
    dio->stand.speed = 32000;                   /* short – even faster than Star Platinum */
    dio->stand.power = 160;                      /* int – A rank power */
    dio->stand.precision = 0.95f;                 /* float – high but not perfect */
    dio->stand.timeStopDuration = TIME_STOP_BASE; /* double – initially 5.0s */
    dio->stand.durability = 6000UL;               /* unsigned long */

    printf("THE WORLD has awakened! Za Warudo!\n");
}

/**
 * Creates a random enemy Stand user (Joestar group).
 * The enemy's stats scale with the player's experience (long used in calculation).
 */
void initializeRandomEnemy(StandUser *enemy, int level) {
    /* Generate a random enemy name */
    const char *names[] = {"Jotaro", "Joseph", "Kakyoin", "Polnareff", "Avdol", "Iggy"};
    int idx = rand() % (sizeof(names)/sizeof(names[0]));
    strcpy(enemy->user.name, names[idx]);

    /* Health scales with level (unsigned long) */
    enemy->user.health = 800UL + (unsigned long)(rand() % 600) + (unsigned long)level * 10UL;
    enemy->user.stamina = 70U + (unsigned int)(rand() % 60);
    enemy->user.experience = (unsigned long)(rand() % 100);   /* random exp */
    enemy->user.alignment = (signed char)(rand() % 201 - 100); /* -100 to 100 */

    /* Stand attributes */
    char standName[MAX_NAME_LEN];
    if (strcmp(enemy->user.name, "Jotaro") == 0)
        strcpy(standName, "Star Platinum");
    else if (strcmp(enemy->user.name, "Joseph") == 0)
        strcpy(standName, "Hermit Purple");
    else if (strcmp(enemy->user.name, "Kakyoin") == 0)
        strcpy(standName, "Hierophant Green");
    else if (strcmp(enemy->user.name, "Polnareff") == 0)
        strcpy(standName, "Silver Chariot");
    else if (strcmp(enemy->user.name, "Avdol") == 0)
        strcpy(standName, "Magician's Red");
    else
        strcpy(standName, "The Fool");

    strcpy(enemy->stand.name, standName);
    enemy->stand.speed = (short)(rand() % 25000 + 5000);         /* short */
    enemy->stand.power = (int)(rand() % 120 + 30);               /* int */
    enemy->stand.precision = (float)(rand() % 1000) / 1000.0f;   /* float 0..1 */
    enemy->stand.timeStopDuration = (double)(rand() % 30) / 10.0; /* double 0..3.0 (Jotaro may have some) */
    enemy->stand.durability = (unsigned long)(rand() % 4000 + 1500);
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
    printf("  Alignment  : %d (signed char) – ", su->user.alignment);
    if (su->user.alignment < 0) printf("Evil\n");
    else if (su->user.alignment > 0) printf("Good\n");
    else printf("Neutral\n");

    printf("STAND: %s\n", su->stand.name);
    printf("  Speed      : %hd (short) – ", su->stand.speed);
    if (su->stand.speed > 30000) printf("Beyond lightning!\n");
    else if (su->stand.speed > 20000) printf("Incredibly fast\n");
    else printf("Average\n");

    printf("  Power      : %d (int) – ", su->stand.power);
    if (su->stand.power > 140) printf("Destructive!\n");
    else if (su->stand.power > 80) printf("Strong\n");
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
    printf("\n>>> BATTLE START: %s (THE WORLD) vs %s <<<\n",
           player->user.name, enemy->user.name);
    printStats(player);
    printStats(enemy);

    int turn = 1;   /* 1 = player, 0 = enemy (just for alternation) */
    int battleOver = 0;
    int actionChoice;

    while (!battleOver) {
        if (turn) {
            /* Player's turn */
            printf("\n--- Your Turn (DIO) ---\n");
            actionChoice = battleMenu();
            switch (actionChoice) {
                case 1:  /* Punch */
                    performPunch(player, enemy);
                    break;
                case 2:  /* Knife Throw */
                    performKnifeThrow(player, enemy);
                    break;
                case 3:  /* Time Stop */
                    performTimeStop(player, enemy);
                    break;
                case 4:  /* Road Roller */
                    performRoadRoller(player, enemy);
                    break;
                default:
                    printf("Invalid action! DIO laughs at your hesitation.\n");
            }
        } else {
            /* Enemy's turn (simple AI) */
            enemyTurn(player, enemy);
        }

        /* Check defeat conditions */
        if (isDefeated(enemy)) {
            printf("\n*** %s has been defeated! MUDA MUDA! ***\n", enemy->user.name);
            player->user.experience += 150UL;   /* gain experience */
            if (player->user.health > 1200UL) player->user.health = 1200UL; /* cap */
            battleOver = 1;
        } else if (isDefeated(player)) {
            printf("\n*** DIO has been defeated... You thought it was your victory, but it was I, DIO!? ***\n");
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
    printf("  2. Knife Throw (ranged, costs stamina)\n");
    printf("  3. Time Stop (stop time and attack, costs stamina)\n");
    printf("  4. Road Roller (devastating drop, costs much stamina)\n");
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
    if (enemy->user.alignment < 0) { /* evil enemies are more aggressive */
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
                printf("%s attempts a special attack!\n", enemy->stand.name);
                /* enemy special move – varies by name */
                if (strstr(enemy->stand.name, "Star Platinum") != NULL) {
                    /* Jotaro's ORA rush */
                    int hits = rand() % 6 + 4; /* 4-9 hits */
                    unsigned long totalDamage = 0;
                    for (int i = 0; i < hits; i++) {
                        totalDamage += (unsigned long)(enemy->stand.power * 0.4f);
                    }
                    applyDamage(player, totalDamage);
                    enemy->user.stamina -= 20U;
                    printf("%s uses ORA ORA for %lu damage!\n", enemy->stand.name, totalDamage);
                } else {
                    /* generic */
                    unsigned long damage = (unsigned long)(enemy->stand.power * 1.2f);
                    applyDamage(player, damage);
                    enemy->user.stamina -= 15U;
                }
            } else {
                printf("%s tries to attack but is too tired.\n", enemy->stand.name);
            }
            break;
        case 3:
            if (enemy->stand.timeStopDuration > 1.0 && enemy->user.stamina >= 30) {
                printf("%s attempts to stop time!\n", enemy->stand.name);
                /* enemy time stop effect: guaranteed two attacks */
                unsigned long damage = (unsigned long)(enemy->stand.power * 1.8);
                applyDamage(player, damage);
                applyDamage(player, damage / 2);
                enemy->user.stamina -= 30U;
                printf("%s lands two hits during stopped time for total %lu damage!\n",
                       enemy->stand.name, damage + damage/2);
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
    printf("%s's %s throws a punch! Muda!\n", attacker->user.name, attacker->stand.name);

    /* Calculate base damage – using int, float, short, and casting */
    float base = (float)attacker->stand.power * attacker->stand.precision;
    float speedBonus = (float)attacker->stand.speed / 100.0f;
    float totalFloat = base + speedBonus;

    /* Convert to unsigned long for damage (health is unsigned long) */
    unsigned long damage = (unsigned long)(totalFloat + 0.5f); /* rounding */

    /* Random variation (use signed rand, then take abs) */
    int variation = (rand() % 21) - 10; /* -10 to 10 */
    if (variation < 0) {
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
 * Knife Throw – ranged attack. Uses precision (float) and stamina (unsigned).
 */
int performKnifeThrow(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < KNIFE_THROW_COST) {
        printf("Not enough stamina! (need %d, have %u)\n", KNIFE_THROW_COST, attacker->user.stamina);
        return 0;
    }
    attacker->user.stamina -= KNIFE_THROW_COST;

    printf("%s throws a flurry of knives! Muda Muda!\n", attacker->user.name);

    /* Number of knives depends on speed (short) and precision */
    int knifeCount = (int)(attacker->stand.speed / 2000) +
                     (int)(attacker->stand.precision * 10);
    if (knifeCount < 3) knifeCount = 3;
    if (knifeCount > 20) knifeCount = 20;

    unsigned long totalDamage = 0UL;
    for (int i = 0; i < knifeCount; i++) {
        /* Each knife does base KNIFE_DAMAGE modified by power and precision */
        float knifeDamageFloat = (float)KNIFE_DAMAGE *
                                 ((float)attacker->stand.power / 100.0f) *
                                 attacker->stand.precision;
        unsigned long knifeDamage = (unsigned long)(knifeDamageFloat + 0.5f);
        if (knifeDamage < 1UL) knifeDamage = 1UL;
        totalDamage += knifeDamage;
    }

    applyDamage(defender, totalDamage);
    printf("%d knives hit for %lu total damage!\n", knifeCount, totalDamage);
    return 1;
}

/**
 * Time Stop – freezes time, allows multiple free attacks.
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
                               (double)attacker->user.experience / 200.0;
    if (effectiveDuration > 12.0) effectiveDuration = 12.0; /* safety cap */

    printf("DIO shouts: 'ZA WARUDO! TOKI YO TOMARE!'\n");
    printf("Time stops for %.2lf seconds!\n", effectiveDuration);

    /* During stopped time, DIO can perform multiple attacks */
    int attacks = (int)(effectiveDuration * 2.0); /* roughly 2 attacks per second */
    if (attacks < 1) attacks = 1;
    if (attacks > 10) attacks = 10;

    unsigned long totalDamage = 0UL;
    for (int i = 0; i < attacks; i++) {
        unsigned long damage = (unsigned long)(attacker->stand.power * 1.2f);
        totalDamage += damage;
    }

    applyDamage(defender, totalDamage);
    printf("DIO lands %d attacks during stopped time, dealing %lu total damage!\n",
           attacks, totalDamage);
    return 1;
}

/**
 * Road Roller – DIO's iconic move. Massive damage but high stamina cost.
 */
int performRoadRoller(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < ROAD_ROLLER_COST) {
        printf("Not enough stamina for Road Roller! (need %d, have %u)\n",
               ROAD_ROLLER_COST, attacker->user.stamina);
        return 0;
    }
    attacker->user.stamina -= ROAD_ROLLER_COST;

    printf("DIO cries: 'ROAD ROLLER DA!'\n");
    printf("A steamroller appears from nowhere and crashes down!\n");

    /* Damage based on power and durability (unsigned long) */
    float damageFloat = (float)ROAD_ROLLER_DAMAGE *
                        ((float)attacker->stand.power / 100.0f) *
                        (1.0f + (float)attacker->stand.durability / 10000.0f);
    unsigned long damage = (unsigned long)(damageFloat);

    applyDamage(defender, damage);
    printf("The Road Roller crushes the enemy for %lu damage!\n", damage);
    return 1;
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
    printf("   THE WORLD (DIO): STAND SIMULATION in C\n");
    printf("   Based on JoJo's Bizarre Adventure Part 3\n");
    printf("============================================================\n");
}

void printAnalysis(void) {
    printSeparator();
    printf("DATA TYPE ANALYSIS (as used in this simulation):\n");
    printSeparator();

    printf("1. short:\n");
    printf("   - Used for Stand speed (0..32767).\n");
    printf("   - THE WORLD's speed is 32000, reflecting its incredible velocity.\n");
    printf("   - In battle, speed contributes to the number of knife throws.\n\n");

    printf("2. int:\n");
    printf("   - Stand power (base destructive force).\n");
    printf("   - Can be negative if a Stand is weakened or cursed.\n");
    printf("   - Power directly influences damage calculations.\n\n");

    printf("3. float:\n");
    printf("   - Precision (0.0 to 1.0). THE WORLD's precision is 0.95.\n");
    printf("   - Used to determine accuracy of knife throws and damage modifiers.\n");
    printf("   - Float allows fractional precision, important for probability.\n\n");

    printf("4. double:\n");
    printf("   - Time stop duration in seconds. DIO can initially stop time for 5.0s.\n");
    printf("   - Double provides high precision for gradual improvements via experience.\n");
    printf("   - Example: effectiveDuration = base + (experience / 200.0).\n\n");

    printf("5. signed:\n");
    printf("   - Implemented as 'signed char' for alignment (range -128 to 127).\n");
    printf("   - DIO's alignment = -100 (evil); enemies can be good (positive).\n");
    printf("   - Signed types allow representation of opposing forces.\n\n");

    printf("6. unsigned:\n");
    printf("   - Health, stamina, durability, experience: never negative.\n");
    printf("   - 'unsigned long' for health and durability (large range).\n");
    printf("   - 'unsigned int' for stamina (0..65535).\n");
    printf("   - Unsigned types ensure these attributes cannot become negative.\n\n");

    printf("7. long:\n");
    printf("   - Experience points and durability are 'unsigned long'.\n");
    printf("   - Long (32-bit or 64-bit) allows values to grow large over many battles.\n");
    printf("   - Used in time stop duration scaling: (double)experience/200.0.\n");

    printSeparator();
    printf("All keywords appear in declarations, casts, and operations throughout\n");
    printf("the code. The simulation demonstrates how each type models a unique\n");
    printf("aspect of THE WORLD's abilities.\n");
    printSeparator();
}

/* ========================================================================== */
/* End of program                                                             */
/* ========================================================================== */