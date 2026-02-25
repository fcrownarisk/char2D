/**
 * ============================================================================
 * INTERACTIVE MIDDLEWARE: STAR PLATINUM vs THE WORLD
 * ----------------------------------------------------------------------------
 * This program acts as a middleware between two Stands from JoJo's Bizarre
 * Adventure: Star Platinum (Jotaro) and The World (DIO). It allows:
 *   - User control of either Stand in a turn-based battle.
 *   - AI vs. AI simulation.
 *   - Real-time stat tracking using fundamental C data types:
 *       short      : Stand speed
 *       int        : Stand power
 *       float      : Precision
 *       double     : Time stop duration
 *       signed     : Alignment (good/evil)
 *       unsigned   : Health, stamina, durability, experience
 *       long       : Experience points (unsigned long)
 *
 * The middleware demonstrates data exchange and interaction between the two
 * Stands, with all calculations respecting type ranges and conversions.
 *
 * Compile: gcc -o middleware middleware.c -lm
 * Run: ./middleware
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

/* ========================================================================== */
/* CONSTANTS & MACROS                                                         */
/* ========================================================================== */

#define MAX_NAME_LEN         50
#define BASE_PUNCH_DAMAGE    20
#define ORA_RUSH_MULTIPLIER   3
#define MUDA_RUSH_MULTIPLIER   3
#define KNIFE_DAMAGE          15
#define ROAD_ROLLER_DAMAGE    80
#define TIME_STOP_COST        40
#define KNIFE_THROW_COST      15
#define ROAD_ROLLER_COST      60

/* ========================================================================== */
/* TYPE DEFINITIONS                                                           */
/* ========================================================================== */

typedef struct {
    char name[MAX_NAME_LEN];
    unsigned long health;           /* unsigned long – health points */
    unsigned int stamina;           /* unsigned int – energy for special moves */
    unsigned long experience;       /* unsigned long – battles won */
    signed char alignment;          /* signed – good (+), evil (-) */
} Character;

typedef struct {
    char name[MAX_NAME_LEN];
    short speed;                    /* short – 0..32767 */
    int power;                       /* int – base attack power (can be negative) */
    float precision;                 /* float – 0.0f to 1.0f */
    double timeStopDuration;         /* double – seconds of stopped time */
    unsigned long durability;        /* unsigned long – remaining energy */
} Stand;

typedef struct {
    Character user;
    Stand stand;
} StandUser;

/* ========================================================================== */
/* GLOBAL VARIABLES                                                           */
/* ========================================================================== */

static unsigned int battleCount = 0;       /* unsigned – number of battles */
static const double TIME_STOP_BASE_SP = 2.5;  /* Star Platinum initial */
static const double TIME_STOP_BASE_TW = 5.0;  /* The World initial */

/* ========================================================================== */
/* FUNCTION PROTOTYPES                                                        */
/* ========================================================================== */

void initializeStarPlatinum(StandUser *jotaro);
void initializeTheWorld(StandUser *dio);
void printStats(const StandUser *su);
void printSeparator(void);
int battleMenu(int controller);  /* 0 = AI, 1 = control Jotaro, 2 = control DIO */
void playerTurn(StandUser *attacker, StandUser *defender, int control);
void enemyTurn(StandUser *ai, StandUser *opponent);
int performPunch(StandUser *attacker, StandUser *defender);
int performOraRush(StandUser *attacker, StandUser *defender);
int performMudaRush(StandUser *attacker, StandUser *defender);
int performKnifeThrow(StandUser *attacker, StandUser *defender);
int performRoadRoller(StandUser *attacker, StandUser *defender);
int performTimeStop(StandUser *attacker, StandUser *defender);
void applyDamage(StandUser *target, unsigned long damage);
int isDefeated(const StandUser *su);
void simulateBattle(StandUser *p1, StandUser *p2, int control);
void printWelcome(void);
void printMiddlewareExplanation(void);

/* ========================================================================== */
/* MAIN FUNCTION                                                              */
/* ========================================================================== */

int main(void) {
    srand((unsigned)time(NULL));

    printWelcome();
    printMiddlewareExplanation();

    StandUser jotaro;
    StandUser dio;
    initializeStarPlatinum(&jotaro);
    initializeTheWorld(&dio);

    int choice;
    int playing = 1;

    while (playing) {
        printf("\n=== MIDDLEWARE MENU ===\n");
        printf("1. Control Jotaro (Star Platinum) vs AI DIO\n");
        printf("2. Control DIO (The World) vs AI Jotaro\n");
        printf("3. Watch AI vs AI battle\n");
        printf("4. View both Stands' stats\n");
        printf("5. Exit\n");
        printf("Choice: ");
        scanf("%d", &choice);
        while (getchar() != '\n'); /* flush */

        switch (choice) {
            case 1:
                simulateBattle(&jotaro, &dio, 1);  /* control Jotaro */
                battleCount++;
                break;
            case 2:
                simulateBattle(&jotaro, &dio, 2);  /* control DIO */
                battleCount++;
                break;
            case 3:
                simulateBattle(&jotaro, &dio, 0);  /* AI vs AI */
                battleCount++;
                break;
            case 4:
                printf("\n--- JOTARO (Star Platinum) ---\n");
                printStats(&jotaro);
                printf("\n--- DIO (The World) ---\n");
                printStats(&dio);
                break;
            case 5:
                playing = 0;
                printf("Middleware terminated. Ora! Muda!\n");
                break;
            default:
                printf("Invalid choice.\n");
        }
    }

    return 0;
}

/* ========================================================================== */
/* INITIALIZATION                                                             */
/* ========================================================================== */

void initializeStarPlatinum(StandUser *jotaro) {
    strcpy(jotaro->user.name, "Jotaro Kujo");
    jotaro->user.health = 1000UL;
    jotaro->user.stamina = 100U;
    jotaro->user.experience = 0UL;
    jotaro->user.alignment = 100;                    /* signed char – good */

    strcpy(jotaro->stand.name, "Star Platinum");
    jotaro->stand.speed = 30000;                      /* short */
    jotaro->stand.power = 150;                         /* int */
    jotaro->stand.precision = 0.99f;                   /* float */
    jotaro->stand.timeStopDuration = TIME_STOP_BASE_SP; /* double */
    jotaro->stand.durability = 5000UL;                  /* unsigned long */
}

void initializeTheWorld(StandUser *dio) {
    strcpy(dio->user.name, "DIO");
    dio->user.health = 1200UL;
    dio->user.stamina = 120U;
    dio->user.experience = 0UL;
    dio->user.alignment = -100;                       /* signed char – evil */

    strcpy(dio->stand.name, "THE WORLD");
    dio->stand.speed = 32000;                          /* short */
    dio->stand.power = 160;                             /* int */
    dio->stand.precision = 0.95f;                       /* float */
    dio->stand.timeStopDuration = TIME_STOP_BASE_TW;    /* double */
    dio->stand.durability = 6000UL;                      /* unsigned long */
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
    printf("  Alignment  : %d (signed char) – %s\n",
           su->user.alignment,
           (su->user.alignment < 0) ? "Evil" : (su->user.alignment > 0) ? "Good" : "Neutral");
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
/* BATTLE SYSTEM                                                             */
/* ========================================================================== */

void simulateBattle(StandUser *p1, StandUser *p2, int control) {
    /* p1 = Jotaro, p2 = DIO (fixed order for simplicity) */
    StandUser *player = NULL;
    StandUser *ai = NULL;
    if (control == 1) {        /* control Jotaro */
        player = p1;
        ai = p2;
    } else if (control == 2) { /* control DIO */
        player = p2;
        ai = p1;
    } else {                   /* AI vs AI */
        player = NULL;          /* both AI */
    }

    printf("\n>>> BATTLE START: %s (Star Platinum) vs %s (The World) <<<\n",
           p1->user.name, p2->user.name);
    printStats(p1);
    printStats(p2);

    int turn = 1;   /* 1 = Jotaro's turn, 2 = DIO's turn */
    int battleOver = 0;

    while (!battleOver) {
        if (control == 0) { /* AI vs AI */
            if (turn == 1) {
                printf("\n--- AI Jotaro's Turn ---\n");
                enemyTurn(p1, p2);
            } else {
                printf("\n--- AI DIO's Turn ---\n");
                enemyTurn(p2, p1);
            }
        } else {
            /* Human vs AI */
            if ((control == 1 && turn == 1) || (control == 2 && turn == 2)) {
                /* Player's turn */
                printf("\n--- Your Turn (%s) ---\n", player->user.name);
                playerTurn(player, (player == p1) ? p2 : p1, control);
            } else {
                /* AI's turn */
                printf("\n--- AI %s's Turn ---\n",
                       (turn == 1) ? p1->user.name : p2->user.name);
                enemyTurn((turn == 1) ? p1 : p2, (turn == 1) ? p2 : p1);
            }
        }

        /* Check defeat */
        if (isDefeated(p1)) {
            printf("\n*** %s has been defeated! ***\n", p1->user.name);
            p2->user.experience += 150UL;
            battleOver = 1;
        } else if (isDefeated(p2)) {
            printf("\n*** %s has been defeated! ***\n", p2->user.name);
            p1->user.experience += 150UL;
            battleOver = 1;
        }

        turn = (turn == 1) ? 2 : 1; /* switch turns */
    }

    /* Reset health for next battle (optional) */
    p1->user.health = 1000UL;
    p2->user.health = 1200UL;
    p1->user.stamina = 100U;
    p2->user.stamina = 120U;
    printf("\n>>> BATTLE END <<<\n");
}

/**
 * Player's turn – presents appropriate moves based on who they control.
 */
void playerTurn(StandUser *attacker, StandUser *defender, int control) {
    int actionChoice;
    if (control == 1) { /* Jotaro's moves */
        printf("Choose Jotaro's action:\n");
        printf("  1. Punch\n");
        printf("  2. Ora Rush (costs stamina)\n");
        printf("  3. Time Stop (costs stamina)\n");
        printf("  4. Catch Bullet (defensive, raises precision)\n");
        printf("Choice: ");
        scanf("%d", &actionChoice);
        while (getchar() != '\n');

        switch (actionChoice) {
            case 1: performPunch(attacker, defender); break;
            case 2: performOraRush(attacker, defender); break;
            case 3: performTimeStop(attacker, defender); break;
            case 4: /* Catch bullet – simplified */
                printf("Star Platinum catches an imaginary bullet! Stamina +5\n");
                attacker->user.stamina += 5U;
                if (attacker->user.stamina > 100U) attacker->user.stamina = 100U;
                break;
            default: printf("Invalid! You hesitate.\n");
        }
    } else { /* DIO's moves */
        printf("Choose DIO's action:\n");
        printf("  1. Punch\n");
        printf("  2. Knife Throw (costs stamina)\n");
        printf("  3. Time Stop (costs stamina)\n");
        printf("  4. Road Roller (costs lots of stamina)\n");
        printf("Choice: ");
        scanf("%d", &actionChoice);
        while (getchar() != '\n');

        switch (actionChoice) {
            case 1: performPunch(attacker, defender); break;
            case 2: performKnifeThrow(attacker, defender); break;
            case 3: performTimeStop(attacker, defender); break;
            case 4: performRoadRoller(attacker, defender); break;
            default: printf("Invalid! DIO laughs.\n");
        }
    }
}

/**
 * AI turn – random actions for the given Stand.
 */
void enemyTurn(StandUser *ai, StandUser *opponent) {
    int action = rand() % 4;
    if (strcmp(ai->stand.name, "Star Platinum") == 0) {
        /* Jotaro AI */
        switch (action) {
            case 0: performPunch(ai, opponent); break;
            case 1: performOraRush(ai, opponent); break;
            case 2: performTimeStop(ai, opponent); break;
            case 3: /* defensive */
                printf("%s focuses.\n", ai->stand.name);
                ai->user.stamina += 3U;
                if (ai->user.stamina > 100U) ai->user.stamina = 100U;
                break;
        }
    } else {
        /* DIO AI */
        switch (action) {
            case 0: performPunch(ai, opponent); break;
            case 1: performKnifeThrow(ai, opponent); break;
            case 2: performTimeStop(ai, opponent); break;
            case 3: performRoadRoller(ai, opponent); break;
        }
    }
}

/**
 * Punch – basic attack.
 */
int performPunch(StandUser *attacker, StandUser *defender) {
    printf("%s's %s throws a punch! ", attacker->user.name, attacker->stand.name);
    float base = (float)attacker->stand.power * attacker->stand.precision;
    float speedBonus = (float)attacker->stand.speed / 100.0f;
    float totalFloat = base + speedBonus;
    unsigned long damage = (unsigned long)(totalFloat + 0.5f);
    int variation = (rand() % 21) - 10;
    if (variation < 0) {
        if (damage > (unsigned long)(-variation))
            damage -= (unsigned long)(-variation);
        else
            damage = 1UL;
    } else {
        damage += (unsigned long)variation;
    }
    applyDamage(defender, damage);
    return 1;
}

/**
 * Ora Rush – Star Platinum's signature.
 */
int performOraRush(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < 20U) {
        printf("Not enough stamina! (need 20, have %u)\n", attacker->user.stamina);
        return 0;
    }
    attacker->user.stamina -= 20U;
    printf("Jotaro: ORA ORA ORA!!!\n");
    int punches = (int)(attacker->stand.speed / 1000);
    if (punches < 3) punches = 3;
    if (punches > 20) punches = 20;
    unsigned long total = 0UL;
    for (int i = 0; i < punches; i++)
        total += (unsigned long)(attacker->stand.power * 0.3f);
    applyDamage(defender, total);
    printf("%d punches land for %lu damage!\n", punches, total);
    return 1;
}

/**
 * Muda Rush – The World's equivalent (used in AI, but we'll reuse for DIO if needed)
 * Actually DIO's AI may use knife throw and road roller instead.
 */

/**
 * Knife Throw – DIO's ranged attack.
 */
int performKnifeThrow(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < KNIFE_THROW_COST) {
        printf("Not enough stamina! (need %d)\n", KNIFE_THROW_COST);
        return 0;
    }
    attacker->user.stamina -= KNIFE_THROW_COST;
    printf("DIO throws knives! MUDA MUDA!\n");
    int knives = (int)(attacker->stand.speed / 2000) + (int)(attacker->stand.precision * 10);
    if (knives < 3) knives = 3;
    if (knives > 20) knives = 20;
    unsigned long total = 0UL;
    for (int i = 0; i < knives; i++) {
        float knifeDmg = (float)KNIFE_DAMAGE * ((float)attacker->stand.power / 100.0f) * attacker->stand.precision;
        total += (unsigned long)(knifeDmg + 0.5f);
    }
    applyDamage(defender, total);
    printf("%d knives hit for %lu damage!\n", knives, total);
    return 1;
}

/**
 * Road Roller – DIO's ultimate move.
 */
int performRoadRoller(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < ROAD_ROLLER_COST) {
        printf("Not enough stamina for Road Roller! (need %d)\n", ROAD_ROLLER_COST);
        return 0;
    }
    attacker->user.stamina -= ROAD_ROLLER_COST;
    printf("DIO: ROAD ROLLER DA!\n");
    float dmgFloat = (float)ROAD_ROLLER_DAMAGE * ((float)attacker->stand.power / 100.0f) *
                     (1.0f + (float)attacker->stand.durability / 10000.0f);
    unsigned long damage = (unsigned long)dmgFloat;
    applyDamage(defender, damage);
    printf("Road Roller crushes for %lu damage!\n", damage);
    return 1;
}

/**
 * Time Stop – both Stands can stop time.
 */
int performTimeStop(StandUser *attacker, StandUser *defender) {
    if (attacker->user.stamina < TIME_STOP_COST) {
        printf("Not enough stamina! (need %d)\n", TIME_STOP_COST);
        return 0;
    }
    attacker->user.stamina -= TIME_STOP_COST;
    double effective = attacker->stand.timeStopDuration + (double)attacker->user.experience / 200.0;
    if (effective > 12.0) effective = 12.0;
    printf("%s: Time has stopped for %.2lf seconds!\n", attacker->user.name, effective);
    int attacks = (int)(effective * 1.5); /* 1.5 attacks per second */
    if (attacks < 1) attacks = 1;
    unsigned long total = 0UL;
    for (int i = 0; i < attacks; i++)
        total += (unsigned long)(attacker->stand.power * 1.2f);
    applyDamage(defender, total);
    printf("%s lands %d attacks during stopped time for %lu damage!\n",
           attacker->stand.name, attacks, total);
    return 1;
}

/**
 * Apply damage – ensure health doesn't go negative.
 */
void applyDamage(StandUser *target, unsigned long damage) {
    if (damage >= target->user.health)
        target->user.health = 0UL;
    else
        target->user.health -= damage;
    printf("%s takes %lu damage. Health now %lu.\n",
           target->user.name, damage, target->user.health);
}

int isDefeated(const StandUser *su) {
    return (su->user.health == 0UL);
}

/* ========================================================================== */
/* EXPLANATION & WELCOME                                                      */
/* ========================================================================== */

void printWelcome(void) {
    printf("\n");
    printf("============================================================\n");
    printf("   INTERACTIVE MIDDLEWARE: STAR PLATINUM vs THE WORLD\n");
    printf("   JoJo's Bizarre Adventure – Stand Battle Simulator\n");
    printf("============================================================\n");
}

void printMiddlewareExplanation(void) {
    printSeparator();
    printf("MIDDLEWARE CONCEPT:\n");
    printf("This program acts as a bridge between two independent Stand\n");
    printf("simulations. It combines their data structures, allows them to\n");
    printf("interact in battle, and demonstrates the use of C data types:\n");
    printf("  - short (speed), int (power), float (precision), double (time stop)\n");
    printf("  - signed (alignment), unsigned (health, stamina, durability), long (experience)\n");
    printf("The middleware handles turn management, damage calculation, and\n");
    printf("AI decision-making, ensuring type safety and range checks.\n");
    printSeparator();
}

/* ========================================================================== */
/* END OF PROGRAM                                                             */
/* ========================================================================== */