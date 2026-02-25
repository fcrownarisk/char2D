/**
 * Spacetime Curvature Engine – Infinite Backroom Edition
 * ----------------------------------------------------------------------------
 * Simulates the curvature of spacetime around a massive object using the
 * Schwarzschild metric. A particle (or light ray) is fired past the object,
 * and its trajectory is computed via numerical integration of the geodesic
 * equations. The result is printed as a series of (x, y) coordinates,
 * showing how the path bends – a direct visualization of curved spacetime.
 *
 * The "infinite backroom" theme: the space is an infinite 2D grid; the
 * curvature is a local distortion near the mass, but the grid extends
 * forever. The engine is written in pure C, using only standard libraries.
 *
 * Compile: gcc -o spacetime spacetime.c -lm
 * Run: ./spacetime
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* Physical constants (geometrized units: G = c = 1) */
#define M 1.0          /* Mass of the central object (black hole) */
#define RS (2.0 * M)   /* Schwarzschild radius */

/* Simulation parameters */
#define DT 0.1         /* Time step for integration */
#define STEPS 1000     /* Number of integration steps */
#define IMPACT_PARAM 4.0  /* Initial impact parameter (distance from axis) */

/* Structure to hold a point in spacetime (2D spatial + time) */
typedef struct {
    double t;          /* coordinate time */
    double x, y;       /* Cartesian coordinates (spatial) */
    double vx, vy;     /* velocities (dx/dt, dy/dt) */
} State;

/* Geodesic equations in Schwarzschild metric in Cartesian-like coordinates
   (using transformed coordinates to avoid singularities at horizon) */
void geodesic_derivatives(State *s, double *dt_dtau, double *dx_dtau,
                          double *dy_dtau, double *dvx_dtau, double *dvy_dtau,
                          double *d2x_dtau2, double *d2y_dtau2) {
    double r = sqrt(s->x * s->x + s->y * s->y);
    double r3 = r * r * r;

    /* Schwarzschild metric coefficients (simplified) */
    double A = 1.0 - RS / r;
    double B = 1.0 / A;

    /* Proper time derivatives of coordinates (for massive particle) */
    *dt_dtau = 1.0 / A;  /* approximation for low velocities; full eqn would need energy */
    *dx_dtau = s->vx * (*dt_dtau);
    *dy_dtau = s->vy * (*dt_dtau);

    /* Acceleration components (from geodesic equation) */
    double factor = RS / (2.0 * r3);
    *d2x_dtau2 = -factor * s->x * (1.0 - 2.0 * M / r) * (*dt_dtau) * (*dt_dtau);
    *d2y_dtau2 = -factor * s->y * (1.0 - 2.0 * M / r) * (*dt_dtau) * (*dt_dtau);
}

/* Numerical integration (Euler method – simple but illustrative) */
void integrate(State *s, int steps) {
    FILE *fp = fopen("trajectory.txt", "w");
    if (!fp) {
        perror("Failed to open output file");
        return;
    }

    printf("Simulating particle trajectory in curved spacetime...\n");
    fprintf(fp, "# t\tx\ty\n");

    for (int i = 0; i <= steps; i++) {
        /* Output current state */
        fprintf(fp, "%lf\t%lf\t%lf\n", s->t, s->x, s->y);

        /* Compute derivatives */
        double dt_dtau, dx_dtau, dy_dtau, d2x_dtau2, d2y_dtau2;
        geodesic_derivatives(s, &dt_dtau, &dx_dtau, &dy_dtau,
                             &d2x_dtau2, &d2y_dtau2);

        /* Update state using Euler step (in proper time) */
        double dtau = DT;  /* step in proper time */
        s->t += dt_dtau * dtau;
        s->x += dx_dtau * dtau;
        s->y += dy_dtau * dtau;
        s->vx += d2x_dtau2 * dtau;
        s->vy += d2y_dtau2 * dtau;
    }

    fclose(fp);
    printf("Trajectory saved to trajectory.txt\n");
}

/* Initialize particle state */
State init_particle(double impact) {
    State s;
    s.t = 0.0;
    s.x = -20.0;               /* start far left */
    s.y = impact;              /* offset from x-axis */
    s.vx = 0.1;                /* initial horizontal velocity (towards right) */
    s.vy = 0.0;                /* no vertical velocity initially */
    return s;
}

/* Simple ASCII visualization of the final trajectory */
void visualize(void) {
    printf("\nASCII representation of spacetime curvature (top view):\n");
    printf("(The black hole at (0,0) bends the particle's path)\n\n");

    /* We'll read the trajectory and plot a small grid */
    FILE *fp = fopen("trajectory.txt", "r");
    if (!fp) return;

    char line[256];
    double t, x, y;
    int grid[41][81] = {{0}};  /* 41 rows (y), 81 columns (x) */

    /* Skip header */
    fgets(line, sizeof(line), fp);

    while (fscanf(fp, "%lf %lf %lf", &t, &x, &y) == 3) {
        /* Map coordinates to grid indices */
        int ix = (int)((x + 20.0) * 2.0);  /* x from -20 to 20 -> 0 to 80 */
        int iy = (int)((y + 10.0) * 2.0);  /* y from -10 to 10 -> 0 to 40 */
        if (ix >= 0 && ix < 81 && iy >= 0 && iy < 41) {
            grid[iy][ix] = 1;  /* mark path */
        }
    }
    fclose(fp);

    /* Print grid */
    for (int iy = 40; iy >= 0; iy--) {
        for (int ix = 0; ix < 81; ix++) {
            if (ix == 40 && iy == 20) {
                printf("@");  /* black hole location */
            } else if (grid[iy][ix]) {
                printf("*");  /* particle path */
            } else {
                printf(".");  /* empty space */
            }
        }
        printf("\n");
    }
    printf("\nLegend: @ = black hole, * = particle trajectory, . = empty space\n");
}

int main() {
    printf("========================================\n");
    printf("   SPACETIME CURVATURE ENGINE\n");
    printf("   Infinite Backroom Edition\n");
    printf("========================================\n");
    printf("Mass M = %.1lf (Schwarzschild radius = %.1lf)\n", M, RS);
    printf("Impact parameter = %.1lf\n\n", IMPACT_PARAM);

    State particle = init_particle(IMPACT_PARAM);
    integrate(&particle, STEPS);

    /* Also show a simple ASCII visualization */
    visualize();

    printf("\nThe path curves because mass warps the surrounding spacetime.\n");
    printf("This is the essence of general relativity, simulated in C.\n");
    return 0;
}