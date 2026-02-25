/**
 * button.c – Button UI component
 * Part of the Infinite Backroom UI system.
 * A button can have child components, allowing infinite nesting
 * (e.g., a button that contains other buttons).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Button {
    char text[64];                  /* Button label */
    struct Button *next;             /* Sibling link */
    struct Button *children;         /* Child buttons – infinite backroom */
    void (*click)(struct Button*);   /* Click handler */
    void (*draw)(struct Button*);    /* Drawing function */
} Button;

/* Forward declarations */
void button_draw(Button *btn);
void button_click(Button *btn);
void button_add_child(Button *parent, Button *child);
Button* button_create(const char *text);

/* Default draw */
void button_draw(Button *btn) {
    if (!btn) return;
    printf("[Button: %s]\n", btn->text);
    Button *child = btn->children;
    while (child) {
        printf("  ");
        child->draw(child);
        child = child->next;
    }
}

/* Default click handler */
void button_click(Button *btn) {
    printf("Button '%s' clicked!\n", btn->text);
    /* Recursively click children? Maybe not, but illustrates nesting */
}

/* Add a child button */
void button_add_child(Button *parent, Button *child) {
    if (!parent || !child) return;
    if (!parent->children) {
        parent->children = child;
    } else {
        Button *last = parent->children;
        while (last->next) last = last->next;
        last->next = child;
    }
}

/* Create a new button */
Button* button_create(const char *text) {
    Button *btn = (Button*)malloc(sizeof(Button));
    if (!btn) return NULL;
    strncpy(btn->text, text, sizeof(btn->text) - 1);
    btn->text[sizeof(btn->text) - 1] = '\0';
    btn->next = NULL;
    btn->children = NULL;
    btn->click = button_click;
    btn->draw = button_draw;
    return btn;
}

/*
int main() {
    Button *root = button_create("Main Button");
    Button *sub1 = button_create("Sub Button 1");
    Button *sub2 = button_create("Sub Button 2");
    button_add_child(root, sub1);
    button_add_child(root, sub2);

    Button *subsub = button_create("Deep Button");
    button_add_child(sub1, subsub);

    root->draw(root);
    root->click(root);   // simulate click
    return 0;
}
*/