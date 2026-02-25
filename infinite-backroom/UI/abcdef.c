/**
 * abcdef.c – Generic UI component "ABCDEF"
 * Part of the Infinite Backroom UI system.
 * This component serves as a flexible container with a label and a linked list
 * of children, allowing infinite nesting (the "backroom" effect).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Structure representing an ABCDEF component */
typedef struct ABCDEF {
    char label[64];                 /* Component label */
    struct ABCDEF *next;            /* Sibling link (for lists) */
    struct ABCDEF *children;        /* First child – enables recursion */
    void (*draw)(struct ABCDEF*);   /* Drawing function */
} ABCDEF;

/* Forward declarations */
void abcdef_draw(ABCDEF *comp);
void abcdef_add_child(ABCDEF *parent, ABCDEF *child);
ABCDEF* abcdef_create(const char *label);

/* Default draw implementation */
void abcdef_draw(ABCDEF *comp) {
    if (!comp) return;
    printf("[ABCDEF: %s]\n", comp->label);
    /* Recursively draw children – infinite backroom */
    ABCDEF *child = comp->children;
    while (child) {
        /* Indent to show nesting level (conceptual) */
        printf("  ");
        child->draw(child);
        child = child->next;
    }
}

/* Add a child to the end of the children list */
void abcdef_add_child(ABCDEF *parent, ABCDEF *child) {
    if (!parent || !child) return;
    if (!parent->children) {
        parent->children = child;
    } else {
        ABCDEF *last = parent->children;
        while (last->next) last = last->next;
        last->next = child;
    }
}

/* Create a new ABCDEF component */
ABCDEF* abcdef_create(const char *label) {
    ABCDEF *comp = (ABCDEF*)malloc(sizeof(ABCDEF));
    if (!comp) return NULL;
    strncpy(comp->label, label, sizeof(comp->label) - 1);
    comp->label[sizeof(comp->label) - 1] = '\0';
    comp->next = NULL;
    comp->children = NULL;
    comp->draw = abcdef_draw;
    return comp;
}

/* Example usage (commented out – not part of the component) */
/*
int main() {
    ABCDEF *root = abcdef_create("Room0");
    ABCDEF *child1 = abcdef_create("Room1");
    ABCDEF *child2 = abcdef_create("Room2");
    abcdef_add_child(root, child1);
    abcdef_add_child(root, child2);

    ABCDEF *grandchild = abcdef_create("Room1a");
    abcdef_add_child(child1, grandchild);

    root->draw(root);

    // Cleanup omitted for brevity
    return 0;
}
*/