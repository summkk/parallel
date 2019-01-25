#include "treap.h"

treap null_treap()
{
	treap T;
	T.MAX_PRIORITY = RAND_MAX+1;
	T.root = NULL;
	return T;
}

node* new_node(int key,value v,int priority,node *father)
{
	node *new_treap_node = (node*)malloc(sizeof(node));
	if(new_treap_node == NULL) exit(1);//malloc check
    new_treap_node ->key = key;
	new_treap_node ->v = v;
	new_treap_node ->priority = priority;

	new_treap_node ->lson = NULL;
	new_treap_node ->rson = NULL;
	new_treap_node ->father = father;
	return new_treap_node ;
}
value node_lookup(node *T,int key)
{
	value tmp;
	tmp.v = 0;
	if (T == NULL)
		return tmp;
	if (key == T->key)
		return T->v;
	else if (key > T->key){
		if(T->rson != NULL)
			return node_lookup(T->rson,key);
		else
			return tmp;
	}
	else{
		if(T->lson != NULL)
			return node_lookup(T->lson,key);
		else
			return tmp;
	}
}

value lookup(treap T,int key)
{
	return node_lookup(T.root,key);
}

bool rotate_up(node *u,node **t_root)//insert时 结点上移
{
	node *father = u->father;

	/*right rotate*/
	if(father == NULL || father->priority >= u->priority)
       return false;
    node *grand = father->father;

	if(u == father->lson){
		father->lson = u->rson;
		if(u->rson != NULL)
			u->rson->father = father;
		u->rson = father;
	}
	/*left rotate*/
	else{
		father->rson = u->lson;
		if(u->lson != NULL)
			u->lson->father = father;
		u->lson = father;
	}
	father->father = u;
	/*修改father的父亲的儿子结点信息*/
	if(grand != NULL){
		if(father == grand->lson)
			grand->lson = u;
		else
			grand->rson = u;
	}
	else{
		*t_root = u;//if father is root
	}
	u->father = grand;
	return true;
}
void node_insert(node *T,int key,value v,int priority,node **root)
{
	if(T == NULL)
		*root = new_node(key,v,priority,NULL);
	else{
		node *new_treap_node  = new_node(key,v,priority,T);
		//key = for spilt
		if(key < T->key || (key == T->key && priority >= T->priority)){
			if(T->lson == NULL){
				T->lson = new_treap_node ;
				while(rotate_up(new_treap_node ,root));
			}
			else
				node_insert(T->lson,key,v,priority,root);
		}
		else{
			if(T->rson == NULL){
				T->rson = new_treap_node ;
				while(rotate_up(new_treap_node ,root));
			}
			else
				node_insert(T->rson,key,v,priority,root);
		}
	}
}

void insert(treap *T,int key,value v)
{
	node_insert(T->root,key,v,rand(),&T->root);
}

bool rotate_down(node *u,node **t_root)
{
    if(u->lson == NULL && u->rson == NULL)
        return false;
    node *son;
    if(u->lson == NULL)
        son = u->rson;
    else if(u->rson == NULL)
        son = u->lson;
    else if(u->lson->priority > u->rson->priority)
        son = u->lson;
    else
        son = u->rson;

    node *grand = u->father;//u= father
    if(son == u->lson){
        u->lson = son->rson;
        if(son->rson != NULL)
            son->rson->father = u;
        son->rson = u;
    }
    else{
        u->rson = son->lson;
        if(son->lson != NULL)
            son->lson->father = u;
        son->lson = u;
    }
    u->father = son;
    if(grand != NULL){
        if(u == grand->lson)
            grand->lson = son;
        else
            grand->rson = son;
    }
    else
        *t_root = son;
    son->father = grand;

    return true;
}
value node_remove(node *T,int key,node **t_root)
{
    	if (key == T->key) {
		node *remove_node = T;

		while (rotate_down(remove_node, t_root));

		node *father = remove_node->father;

		if (father == NULL) {
			*t_root = NULL;
		}
		else if (remove_node == father->lson) {
			father->lson = NULL;
		}
		else {
			father->rson = NULL;
		}

		value remove_value = remove_node->v;

		free(remove_node);

		return remove_value;
	}
	else if (key < T->key) {
		if (T->lson != NULL) {
			return node_remove(T->lson, key, t_root);
		}
		else {
			printf("Attempt to delete non-existent elements");
			return (value) {0};
		}
	}
	else {
		if (T->rson != NULL) {
			return node_remove(T->rson, key, t_root);
		}
		else {
			printf("Attempt to delete non-existent elements");
			return (value) {0};
		}
	}
}
value _remove(treap *T,int key)
{
    return node_remove(T->root,key,&T->root);
}
value remove(treap *T,int key)
{
    return node_remove(T->root,key,&T->root);
}
treap initTreap(int key,value v)
{
	treap T = null_treap();
	insert(&T,key,v);
	return T;
}

treaps split(treap T,int key)
{
	value tmp;
	tmp.v = 0;
	treaps t3;
	node_insert(T.root,key,tmp,T.MAX_PRIORITY,&T.root);

	treap t1 = null_treap();
	treap t2 = null_treap();

	t1.root = T.root->lson;
	if(t1.root != NULL)
		t1.root->father = NULL;

	t2.root = T.root->rson;
	if(t2.root != NULL)
		t2.root->father = NULL;
	t3.T1 = t1;
	t3.T2 = t2;
	return t3;
}

int lookupMax(treap T)
{
	node *max = T.root;
	if(max == NULL)
		return INT_MAX;
	while(max->rson != NULL){
		max = max->rson;
	}
	return max->key;
}
int lookupMin(treap T)
{
	node *min = T.root;
	if(min == NULL)
		return INT_MIN;
	while(min->lson != NULL){
		min = min->lson;
	}
	return min->key;
}

treap join(treap T1,treap T2)
{
    if(T1.root == NULL) return T2;
    if(T2.root == NULL) return T1;
	int left_key = lookupMax(T1);
	int right_key = lookupMin(T2);
	if(left_key == right_key)
    {
        _remove(&T1,left_key);
        left_key = lookupMax(T1);
    }
	int mid_key = (left_key + right_key) >> 1;

	treap T = initTreap(mid_key,(value) {0});
	T.root->priority = T.MAX_PRIORITY;
	T.root->lson = T1.root;
	T.root->rson = T2.root;

	if(T1.root != NULL) T1.root->father = T.root;
	if(T2.root != NULL) T2.root->father = T.root;
    _remove(&T,mid_key);
    return T;
}

treap joinM(treap T1,int key,value v,treap T2)
{
    treap tmp = initTreap(key,v);
    tmp = join(tmp,T2);
    tmp = join(T1,tmp);
    return tmp;
}
treap meld(treap T1,treap T2)
{
    if(T1.root == NULL)
        return T2;
    if(T2.root == NULL)
        return T1;
    node *to_split = T1.root;

    treaps T3 = split(T2,to_split->key);
    treap l,r;
    treap l2,r2;
    l2 = null_treap();
    r2 = null_treap();
    l2.root = T1.root->lson;
    r2.root = T1.root->rson;
    l = meld(l2,T3.T1);
    r = meld(r2,T3.T2);
    treap T_result;

    T_result = joinM(l,to_split->key,to_split->v,r);

    return T_result;
}
treap difference(treap T1,treap T2)
{
    if(T1.root == NULL) return null_treap();
    if(T2.root == NULL) return T1;
    node *to_split = T1.root;
    value tmp = lookup(T2,to_split->key);
    treaps T3 = split(T2,to_split->key);
    treap l,r,l2,r2;
    l2 = null_treap();
    r2 = null_treap();
    l2.root = T1.root->lson;
    r2.root = T1.root->rson;
    l = difference(l2,T3.T1);
    r = difference(r2,T3.T2);
    treap t_result;
    if(tmp.v != 0)
        t_result = join(l,r);
    else
        t_result =  joinM(l,to_split->key,to_split->v,r);
    return t_result;
}

int height(node *T)
{
    if(T == NULL)
        return 0;
    else{
        int hl = height(T->lson);
        int hr = height(T->rson);
        if(hl >= hr) return hl+1;
        else return hr+1;
    }
}
int number(node *T)
{
    if(T == NULL) return 0;
    else
        return number(T->lson)+number(T->rson)+1;
}
double balanceFactor(treap T)
{
    double h = (double)height(T.root);
    double n = (double)number(T.root);
    double f = h/(log(n)/log(2)+1);
    return f;
}

