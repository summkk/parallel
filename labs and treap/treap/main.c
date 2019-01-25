#include "treap.h"
/*although the key is continued and value is random, it dose not affect the veracity of the code*/
void NODE_DISPLAY(node* u) {
	if (u != NULL) {
		printf("<%d,%d,%d>", u->key, u->v.v, u->priority);
		if (u->lson != NULL) {
			printf("(");
			NODE_DISPLAY(u->lson);
			printf(")");
		}
		if (u->rson != NULL) {
			printf("(");
			NODE_DISPLAY(u->rson);
			printf(")");
		}
	}
}

bool test_BST(node *T)
{
    if(T == NULL)
        return true;
    if((T->lson != NULL) &&(T->key < T->lson->key))
    {
        printf("violate BST property:\n");
        return false;
    }
    else if((T->rson != NULL) &&(T->key > T->rson->key))
    {
        printf("violate BST property:\n");
        return false;
    }
    if(T->lson != NULL) test_BST(T->lson);
    if(T->rson != NULL) test_BST(T->rson);
    return true;
}
bool test_HEAP(node *T)
{
    if(T == NULL)
        return true;
    if((T->lson != NULL) && (T->lson->priority > T->priority))
    {
        printf("violate TREAP property:\n");
        return false;
    }

    if((T->rson != NULL) && (T->rson->priority > T->priority))
    {
        printf("violate TREAP property:\n");
        return false;
    }
    if(T->lson != NULL) test_HEAP(T->lson);
    if(T->rson != NULL) test_HEAP(T->rson);
    return true;
}
bool test_TREAP(node *T)
{
    if(test_BST(T) && test_HEAP(T))
        return true;
    else
        return false;
}
void test_initial()
{
    printf("test for initalTreap:\n");
    treap T1 = initTreap(3,(value){98});
    NODE_DISPLAY(T1.root),puts("");
}
/*change parameter to create different tree*/
treap NEW_TREAP(int m,int n)
{
    treap T = null_treap();
    for(int i = m; i < n;i++)
        insert(&T,i,(value){rand()});
    test_BST(T.root);
    return T;
}
void test_insert()
{
    printf("test for insert:\n");
    treap T2 = null_treap();
    for (int i = 0; i < 10; i++) {
		insert(&T2, i, (value) {rand()});
	}
	NODE_DISPLAY(T2.root), puts("");
	if(test_TREAP(T2.root))
        printf("treap pass!\n");
}
void test_lookup()
{
    treap T = NEW_TREAP(0,10);
    printf("test for lookup:\n");
	for (int i = 0; i < 11; i++) {
		printf("%d\n", lookup(T, i).v);
	}
}
void test_remove()
{
    treap T = NEW_TREAP(0,10);
    printf("test for remove:\n");
    for(int i = 3;i<8;i++)
    {
        _remove(&T,i);
        NODE_DISPLAY(T.root),puts("");
        if(test_TREAP(T.root))
            printf("treap pass!\n");
    }
}
void test_split()
{
    treaps t;
    treap to_split = NEW_TREAP(2,15);
    t = split(to_split,5);
    printf("test for split:\n");
    NODE_DISPLAY(t.T1.root),puts("");
    NODE_DISPLAY(t.T2.root),puts("");
    if(test_TREAP(t.T1.root))
        printf("treap left pass!\n");
    if(test_TREAP(t.T2.root))
        printf("treap right pass!\n");
}
void test_join()
{
    treap join1,join2;
    join1 = NEW_TREAP(0,30);
    join2 = NEW_TREAP(30,42);
    treap t = join(join1,join2);
    NODE_DISPLAY(t.root),puts("");
    if(test_TREAP(t.root))
        printf("treap pass!\n");
}
void test_meld()
{
    treap meld1,meld2;
    meld1 = NEW_TREAP(0,30);
    meld2 = NEW_TREAP(3,42);
    printf("test for meld:\n");
    treap t = meld(meld1,meld2);
    NODE_DISPLAY(t.root),puts("");
    if(test_TREAP(t.root))
        printf("treap pass!\n");
}
void test_difference()
{
    treap dif1,dif2;
    dif1 = NEW_TREAP(0,30);
    dif2 = NEW_TREAP(3,25);
    printf("test for difference:\n");
    treap t = difference(dif1,dif2);
    NODE_DISPLAY(t.root),puts("");
    if(test_TREAP(t.root))
        printf("treap pass!\n");
}
void test_balanceFactor(treap T)
{
    printf("test balance of treap:\t");
//    printf("%d\t",height(T.root));
//    printf("%d\t",number(T.root));
    printf("%f\n",balanceFactor(T));
}
int main() {
    test_initial();
	test_insert();

    treap T = NEW_TREAP(0,100);
	test_remove();

    test_lookup();
    test_balanceFactor(T);

    test_split();

    test_join();

    test_meld();

    test_difference();
}
