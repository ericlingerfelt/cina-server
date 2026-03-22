/****************************************************************************/
/*! \file
 *
 *
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2007/12/03 17:48:51 $
 * $Id: rb.h,v 1.2 2007/12/03 17:48:51 bucknerk Exp $
 *
 * $Log: rb.h,v $
 * Revision 1.2  2007/12/03 17:48:51  bucknerk
 * merged with branch cina-rel-1-0
 *
 * Revision 1.1.2.1  2007/11/28 18:35:25  bucknerk
 * Renamed craz_list list_crazy.dat, updated the makefile for the create_input
 * files and added all those create_input files.
 *
 *
 *
 * \endverbatim
 * ********************************
 *
 * Original code by Jim Plank, modified for THINK C 6.0 for Macintosh by 
 * Chris Bartley
 *
 * I am not even going to pretend to have created this stuff.  It works great
 * even if it is somewhat cryptic.
 *
 **************************************************************************/
 
typedef struct {
  unsigned red : 1 ;
  unsigned internal : 1 ;
  unsigned left : 1 ;
  unsigned root : 1 ;
  unsigned head : 1 ;
} status;
 
typedef struct rb_node {
  union {
    struct {
      struct rb_node *flink;
      struct rb_node *blink;
    } list;
    struct {
      struct rb_node *left;
      struct rb_node *right;
    } child;
  } c;
  union {
    struct rb_node *parent;
    struct rb_node *root;
  } p;
  status s;
  union {
    int ikey;
    char *key;
    struct rb_node *lext;
  } k;
  union {
    char *val;
    struct rb_node *rext;
  } v;
} *Rb_node;
/*****/
/*function prototypes*/
extern Rb_node make_rb();
extern Rb_node rb_insert_b(Rb_node nd, char *key, char *val);
extern Rb_node rb_find_key(Rb_node root, char *key);
extern Rb_node rb_find_ikey(Rb_node root, int ikey);
extern Rb_node rb_find_gkey(Rb_node root, char *key, int (*func)());
extern Rb_node rb_find_key_n(Rb_node root, char *key, int *found);
extern Rb_node rb_find_ikey_n(Rb_node root, int ikey, int *found);
extern Rb_node rb_find_gkey_n(Rb_node root, char *key, int (*func)(),int *found);
extern void rb_delete_node(Rb_node node);
/* Deletes and frees an entire tree */
extern void rb_free_tree(Rb_node root);  
/* Returns node->v.val (this is to shutlint up */
extern char *rb_val(Rb_node node);  
/*****/
/*more function prototypes*/
void mk_new_int(Rb_node l, Rb_node r, Rb_node p, int il);
Rb_node lprev(Rb_node n);
Rb_node rprev(Rb_node n);
void recolor(Rb_node n);
void single_rotate(Rb_node y, int l);
void rb_print_tree(Rb_node t, int level);
void rb_iprint_tree(Rb_node t, int level);
int rb_nblack(Rb_node n);
int rb_plength(Rb_node n);
char *rb_val(Rb_node n);
 
void insert(Rb_node item, Rb_node list);
void delete_item(Rb_node item);
/*****/
/* Don't stress over these macros, just use them and move on */
#define rb_insert_a(n, k, v) rb_insert_b(n->c.list.flink, (char *) k, (char *) v)
#define rb_insert(t, k, v) rb_insert_b(rb_find_key(t, k), (char *) k, (char *) v)
#define rb_inserti(t, k, v) rb_insert_b(rb_find_ikey(t, k), (char *) k, (char *)v)
#define rb_insertg(t, k, v, f) rb_insert_b(rb_find_gkey(t, k, f), (char *) k, (char *) v)
#define rb_first(n) (n->c.list.flink)
#define rb_last(n) (n->c.list.blink)
#define rb_next(n) (n->c.list.flink)
#define rb_prev(n) (n->c.list.blink)
#define rb_empty(t) (t->c.list.flink == t)
#ifndef rb_nil
#define rb_nil(t) (t)
#endif
 
#define rb_traverse(ptr, lst) \
  for(ptr = rb_first(lst); ptr != rb_nil(lst); ptr = rb_next(ptr))
 


