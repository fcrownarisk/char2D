/**
 * window.c – Window UI component
 * Part of the Infinite Backroom UI system.
 * A window can contain any number of child components (including other windows),
 * leading to infinite nesting – the backrooms never end.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declaration for generic component – we use void* with type tags
   In a real system you'd have a base class. Here we keep it simple: Window
   can only contain other Windows, but the concept is the same. */
typedef struct Window {
    char title[64];                  /* Window title */
    struct Window *next;              /* Sibling windows */
    struct Window *children;          /* Child windows – infinite recursion */
    void (*paint)(struct Window*);    /* Paint function */
    void (*close)(struct Window*);    /* Close handler */
} Window;

/* Forward declarations */
void window_paint(Window *wnd);
void window_close(Window *wnd);
void window_add_child(Window *parent, Window *child);
Window* window_create(const char *title);

/* Default paint */
void window_paint(Window *wnd) {
    if (!wnd) return;
    printf("+----------------------+\n");
    printf("| Window: %s\n", wnd->title);
    printf("+----------------------+\n");
    Window *child = wnd->children;
    while (child) {
        /* Indent to show depth */
        printf("  ");
        child->paint(child);
        child = child->next;
    }
}

/* Default close */
void window_close(Window *wnd) {
    printf("Window '%s' closed.\n", wnd->title);
    /* Recursively close children? Could, but not necessary for demo */
}

/* Add a child window */
void window_add_child(Window *parent, Window *child) {
    if (!parent || !child) return;
    if (!parent->children) {
        parent->children = child;
    } else {
        Window *last = parent->children;
        while (last->next) last = last->next;
        last->next = child;
    }
}

/* Create a new window */
Window* window_create(const char *title) {
    Window *wnd = (Window*)malloc(sizeof(Window));
    if (!wnd) return NULL;
    strncpy(wnd->title, title, sizeof(wnd->title) - 1);
    wnd->title[sizeof(wnd->title) - 1] = '\0';
    wnd->next = NULL;
    wnd->children = NULL;
    wnd->paint = window_paint;
    wnd->close = window_close;
    return wnd;
}

/*
int main() {
    Window *root = window_create("Backroom Entrance");
    Window *room1 = window_create("Room 1");
    Window *room2 = window_create("Room 2");
    window_add_child(root, room1);
    window_add_child(root, room2);

    Window *hidden = window_create("Hidden Backroom");
    window_add_child(room1, hidden);

    root->paint(root);
    root->close(root);
    return 0;
}
*/