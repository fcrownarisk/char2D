/**
 * XYZT.c – Generic UI component "XYZT"
 * Part of the Infinite Backroom UI system.
 * This component is similar to ABCDEF but with a different visual style,
 * demonstrating that any component can be nested infinitely.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct XYZT {
    char name[64];                  /* Component name */
    struct XYZT *next;               /* Sibling link */
    struct XYZT *children;           /* First child – infinite nesting */
    void (*render)(struct XYZT*);    /* Rendering function */
} XYZT;

/* Forward declarations */
void xyzt_render(XYZT *comp);
void xyzt_append_child(XYZT *parent, XYZT *child);
XYZT* xyzt_new(const char *name);

/* Default render */
void xyzt_render(XYZT *comp) {
    if (!comp) return;
    printf("<XYZT name='%s'>\n", comp->name);
    XYZT *child = comp->children;
    while (child) {
        /* Indent to show depth */
        printf("  ");
        child->render(child);
        child = child->next;
    }
    printf("</XYZT>\n");
}

/* Append a child */
void xyzt_append_child(XYZT *parent, XYZT *child) {
    if (!parent || !child) return;
    if (!parent->children) {
        parent->children = child;
    } else {
        XYZT *last = parent->children;
        while (last->next) last = last->next;
        last->next = child;
    }
}

/* Create a new XYZT component */
XYZT* xyzt_new(const char *name) {
    XYZT *comp = (XYZT*)malloc(sizeof(XYZT));
    if (!comp) return NULL;
    strncpy(comp->name, name, sizeof(comp->name) - 1);
    comp->name[sizeof(comp->name) - 1] = '\0';
    comp->next = NULL;
    comp->children = NULL;
    comp->render = xyzt_render;
    return comp;
}

/*
int main() {
    XYZT *root = xyzt_new("Backroom_0");
    XYZT *a = xyzt_new("Backroom_1");
    XYZT *b = xyzt_new("Backroom_2");
    xyzt_append_child(root, a);
    xyzt_append_child(root, b);

    XYZT *deep = xyzt_new("Backroom_1_deep");
    xyzt_append_child(a, deep);

    root->render(root);
    return 0;
}
*/