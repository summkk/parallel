#ifndef TREAP_H_INCLUDED
#define TREAP_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>
#include <limits.h>


typedef struct value{
    int v;
} value;

typedef struct node{
    int key;
    value v;
    int priority;
    struct node *lson,*rson,*father;
} node;

typedef struct treap{
    int MAX_PRIORITY;
    node *root;
}treap;

typedef struct treaps{
    treap T1,T2;
}treaps;

treap null_treap();
//node* new_node(int key,value v,int priority,node *father);
//value node_lookup(node *T,int key);
value lookup(treap T,int key);
void insert(treap *T,int key,value v);
value remove(treap *T,int key);
value _remove(treap *T,int key);
treap initTreap(int key,value v);
treaps split(treap T,int key);
treap join(treap T1,treap T2);
treap meld(treap T1,treap T2);
treap difference(treap T1,treap T2);
double balanceFactor(treap T);
//treap joinM(treap T1,int key,value v,treap T2);
//int height(node *T);
//int number(node *T);
#endif // TREAP_H_INCLUDED
