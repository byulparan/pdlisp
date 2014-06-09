
#include "m_pd.h"
#include "ecl/ecl.h"
#include <stdlib.h>
#include <string.h>

//================= FOR ECL =======================================
/* lisp 쪽에서 호출할 C 함수 레지스트. 이 매크로로 등록을 해야 Lisp 에서 호출이 가능 */

cl_object get_inlets;
cl_object get_outlets;

cl_object make_object;

cl_object load_f;
cl_object get_new_object;
cl_object pd_ptr;
cl_object set_pd_ptr;

cl_object inlet_callback;
cl_object receive_callback;
cl_object clock_callback;
cl_object finalize;

static char* ecl_to_cstring(cl_object str) {
  return ecl_base_string_pointer_safe(si_copy_to_simple_base_string(str));
}

/* pd 콘솔창에 출력하기 위한 lisp에서 쓸 함수.정상작동!_-_*/
void print_to_pd_post(cl_object string) {
  post("%s", ecl_to_cstring(string));
}

void print_to_pd_error(cl_object string) {
  error("%s", ecl_to_cstring(string));
}

#define DEFUN(name,fun,args)			\
  cl_def_c_function(c_string_to_object(name),	\
		    (cl_objectfn_fixed)fun,	\
		    args)


void t_atom_to_cl_list(cl_object *list, int argc, t_atom* argv) {
  char* name;
  for(int i = 0; i < argc; i++) {
    switch(argv[i].a_type) {
    case A_FLOAT:
      *list = cl_nconc(2,*list, cl_list(1,ecl_make_single_float(argv[i].a_w.w_float)));
      break;
    case A_SYMBOL:
      name = argv[i].a_w.w_symbol->s_name;
      *list = cl_nconc(2, *list, cl_list(1, ecl_make_simple_base_string(name,strlen(name))));
      break;
    }
  }
}

void cl_list_to_t_atoms(cl_object list,int argc, t_atom* argv) {
  for (int i = 0; i < argc; i++) {
    cl_object arg = cl_nth(ecl_make_integer(i), list);
    if (cl_numberp(arg) == Ct) {
      argv[i].a_type = A_FLOAT;
      argv[i].a_w.w_float = ecl_to_float(arg);
    }
    else if (cl_stringp(arg) == Ct) {
      argv[i].a_type = A_SYMBOL;
      argv[i].a_w.w_symbol = gensym(ecl_to_cstring(arg));
    }
  }
}


void set_name(cl_object* dst, char* src) {
  if (src) *dst = ecl_make_simple_base_string(src, strlen(src));
  else *dst = NULL;
}

//================================================================


struct lisp;			/* 메인 객체 */
struct pdlisp_proxyinlet;
struct pdlisp_proxyreceive;
struct pdlisp_proxyclock;

static t_class *lisp_class;
static t_class *pdlisp_proxyinlet_class; 
static t_class *pdlisp_proxyreceive_class;
static t_class *pdlisp_proxyclock_class;

typedef struct lisp {
  t_object x_ob;
  cl_object object;
  cl_object objname;
  cl_object filename;
  cl_object curdir;
  int inlets;
  int outlets;
  t_outlet **out;
  struct pdlisp_proxyinlet *in;
} t_lisp;

typedef struct pdlisp_proxyinlet {
  t_pd pd;
  struct lisp *owner;
  unsigned int id;
  t_inlet* inlet;
}t_pdlisp_proxyinlet;

typedef struct pdlisp_proxyreceive {
  t_pd pd;
  cl_object owner;
  cl_object name;
}t_pdlisp_proxyreceive;

typedef struct pdlisp_proxyclock {
  t_pd pd;
  cl_object owner;
  t_clock *clock;
}t_pdlisp_proxyclock;



int make_lisp_object(t_lisp* lisp,cl_object rest) {
  cl_object ret;

  if (lisp->filename) {
    ret = cl_funcall(3 , load_f, lisp->filename, lisp->curdir);
    if (ecl_to_int32_t(ret) == -1) return -1;
  }

  ret = cl_funcall(3, make_object, lisp->objname, rest);
  if (ecl_to_int32_t(ret) == -1) return -1;
    
  return 0;
}


void* lisp_new(t_symbol *s, int argc, t_atom *argv) {
  s;
  t_lisp *lisp = (t_lisp*)pd_new(lisp_class);

  set_name(&lisp->curdir,(argc == 0 ? NULL : canvas_getcurrentdir()->s_name));
  set_name(&lisp->filename,(argc == 0 ? NULL : argv[0].a_w.w_symbol->s_name));
  set_name(&lisp->objname,(argc > 1 ? argv[1].a_w.w_symbol->s_name : "PDLISP"));

  
  cl_object list = Cnil;
  if (argc > 2) {
    t_atom_to_cl_list(&list,argc - 2, argv + 2);
  }

  
  if (make_lisp_object(lisp, list) == -1) {
    return NULL;
  }

  /* if (argc == 1) { */
  /*   lisp->in = malloc(sizeof(t_pdlisp_proxyinlet)); */
  /*   lisp->in->pd = pdlisp_proxyinlet_class; */
  /*   lisp->in->owner = lisp; */
  /*   lisp->in->id = 0; */
  /*   inlet_new(&lisp->x_ob,&(lisp->in->pd),0,0); */
  /*   return (void*)lisp; */
  /* } */
  lisp->object =  cl_funcall(1,get_new_object);
  cl_funcall(3,set_pd_ptr, ecl_make_integer((int)lisp), lisp->object);
  lisp->inlets = ecl_to_int32_t(cl_funcall(2, get_inlets, lisp->object));
  lisp->outlets = ecl_to_int32_t(cl_funcall(2, get_outlets,lisp->object));
  /* 인렛 */
  lisp->in = malloc(lisp->inlets * sizeof(t_pdlisp_proxyinlet));
  for (int i = 0; i < lisp->inlets; i++) {
    lisp->in[i].pd = pdlisp_proxyinlet_class;
    lisp->in[i].owner = lisp;
    lisp->in[i].id = i;
    inlet_new(&lisp->x_ob,&lisp->in[i].pd,0,0);
  }

  /* 아웃렛 */
  lisp->out = malloc(lisp->outlets*sizeof(t_outlet*));
  for(int i = 0; i < lisp->outlets; i++) {
    lisp->out[i] = outlet_new(&lisp->x_ob,0);
  }
  
  return (void*)lisp;
}

static void lisp_free(t_lisp *o) {
  if(o->in) {
    /* 메모리 보호로 인해서 직접 해제가 불가 */
    /* for(int i = 0; i < o->inlets;i++) { */
    /*   inlet_free(o->in[i].inlet); */
    /* } */
    free(o->in);
    o->in = NULL;
  }
  if(o->out) {
    for(int i = 0; i < o->outlets;i++) {
      outlet_free(o->out[i]);
    }
    free(o->out);
    o->out = NULL;
  }

  if (o->object) cl_funcall(2,finalize,o->object);
}



/* inlet 입력에 대한 핸들러 */
static void pdlisp_inlet_anything(t_pdlisp_proxyinlet* p, t_symbol *s, int argc,t_atom *argv) {
  if (!p->owner->object) {
    error("this object is broken!");
    return;
  }

  char* typename = s->s_name;
  cl_object list = Cnil;
  t_atom_to_cl_list(&list,argc, argv);


  if (!(strcmp(typename,"reload") == 0 && p->id == 0)) {
    cl_funcall(5,inlet_callback ,p->owner->object, ecl_make_integer(p->id),
	       ecl_make_simple_base_string(typename,strlen(typename)), list);
  }

  else {		
    cl_object pd_ptr_wrap = cl_funcall(2,pd_ptr,p->owner->object);
    cl_funcall(2,finalize, p->owner->object);
    p->owner->object = NULL;
    if (make_lisp_object(p->owner,list) == -1) {
      error("fail to re-instance object. this object is broken!");
      return;
    }
    cl_object newlisp = cl_funcall(1,get_new_object);
    if (p->owner->inlets != ecl_to_int32_t(cl_funcall(2,get_inlets,newlisp)) ||
	p->owner->outlets != ecl_to_int32_t(cl_funcall(2,get_outlets,newlisp))) {
      error("can't change object's inlets & outlets. you must be remake pd object");
    }
    cl_funcall(3,set_pd_ptr, pd_ptr_wrap ,newlisp);
    p->owner->object = newlisp;
  }
}

void pdlisp_outlet_anything(cl_object object,cl_object loutlet, cl_object lsel ,cl_object largc,cl_object list) {
  int outlet = ecl_to_int32_t(loutlet);
  int argc = ecl_to_int32_t(largc);
  t_lisp* lisp = (t_lisp*)ecl_to_int32_t(cl_funcall(2,pd_ptr,object));
  t_symbol *sel = gensym(ecl_to_cstring(lsel));
  t_atom argv[argc];
  cl_list_to_t_atoms(list, argc, argv);
  outlet_anything(lisp->out[outlet - 1], sel, argc, argv);
}


void pdlisp_receive_anything(t_pdlisp_proxyreceive *r,t_symbol *sel, int argc, t_atom *argv) {
  cl_object list = Cnil;
  t_atom_to_cl_list(&list,argc, argv);
  cl_funcall(5,receive_callback, r->owner, r->name,
	     ecl_make_simple_base_string(sel->s_name,strlen(sel->s_name)),list);
}

void pdlisp_send_anything(cl_object lreceiver,cl_object lsel ,cl_object largc,cl_object list) {
  t_symbol* receivesym = gensym(ecl_to_cstring(lreceiver));
  if (receivesym->s_thing) {	/* receiver 로 등록이 되지 않았다면 t_symbol 의 s_thing,s_next 는 NULL */
    t_symbol* sel = gensym(ecl_to_cstring(lsel));
    int argc = ecl_to_int32_t(largc);
    t_atom argv[argc];
    cl_list_to_t_atoms(list, argc, argv);
    typedmess(receivesym->s_thing, sel, argc, argv);
  }
}

cl_object pdlisp_proxyreceive_new(cl_object object, cl_object name) {
  t_pdlisp_proxyreceive* r = malloc(sizeof(t_pdlisp_proxyreceive));
  r->pd = pdlisp_proxyreceive_class;
  r->owner = object;
  r->name = name;
  pd_bind(&r->pd, gensym(ecl_to_cstring(r->name)));
  return ecl_make_integer((int)r);
}

void pdlisp_proxyreceive_free(cl_object object) {
  t_pdlisp_proxyreceive *r = (t_pdlisp_proxyreceive*)ecl_to_int32_t(object);
  pd_unbind(&r->pd, gensym(ecl_to_cstring(r->name)));
  r->pd = NULL;
  r->owner = NULL;
  r->name = NULL;
  free(r);
}





void pdlisp_proxyclock_bang(t_pdlisp_proxyclock *c) {
  cl_funcall(2, clock_callback, c->owner);
}


cl_object pdlisp_proxyclock_new(cl_object object) {
  t_pdlisp_proxyclock *c = malloc(sizeof(t_pdlisp_proxyclock));
  c->pd = pdlisp_proxyclock_class;
  c->owner = object;
  c->clock = clock_new(c, (t_method) pdlisp_proxyclock_bang);
  return ecl_make_integer((int)c);
}

void pdlisp_proxyclock_free(cl_object clock) {
  t_pdlisp_proxyclock *c = (t_pdlisp_proxyclock*)ecl_to_int32_t(clock);
  clock_free(c->clock);
  free(c);
}

void pdlisp_clock_delay(cl_object clock,cl_object delaytime) {
  t_pdlisp_proxyclock *c = (t_pdlisp_proxyclock*)ecl_to_int32_t(clock);
  clock_delay(c->clock, ecl_to_double(delaytime));
}

void pdlisp_clock_set(cl_object clock,cl_object sys_time) {
  t_pdlisp_proxyclock *c = (t_pdlisp_proxyclock*)ecl_to_int32_t(clock);
  clock_set(c->clock, ecl_to_double(sys_time));
}

void pdlisp_clock_unset(cl_object clock) {
  t_pdlisp_proxyclock *c = (t_pdlisp_proxyclock*)ecl_to_int32_t(clock);
  clock_unset(c->clock);
}







static void pdlisp_proxyinlet_setup() {
  pdlisp_proxyinlet_class = class_new(gensym("pdlisp proxy inlet"),0,0,sizeof(t_pdlisp_proxyinlet),0,0);
  class_addanything(pdlisp_proxyinlet_class, pdlisp_inlet_anything);
}

static void pdlisp_proxyreceive_setup() {
  pdlisp_proxyreceive_class = class_new(gensym("pdlisp proxy receive"),0, 0,sizeof(t_pdlisp_proxyreceive),0,0);
  class_addanything(pdlisp_proxyreceive_class, pdlisp_receive_anything);
}

static void pdlisp_proxyclock_setup() {
  pdlisp_proxyclock_class = class_new(gensym("pdlisp proxy clock"), 0, 0, sizeof(t_pdlisp_proxyclock), 0, 0);
}


void pdlisp_setup(void) {
  char* dummy[] = {""};
  
  cl_boot(1,dummy);

  pdlisp_proxyinlet_setup();
  pdlisp_proxyreceive_setup();
  pdlisp_proxyclock_setup();

  lisp_class = class_new(gensym("pdlisp"), (t_newmethod)lisp_new, (t_method)lisp_free,
			 sizeof(t_lisp), CLASS_NOINLET,A_GIMME,0);
  
  extern void init_lib_SOCKETS(cl_object);
  extern void init_lib_ASDF(cl_object);

  cl_eval(c_string_to_object("(setq si::*documentation-pool* nil)"));
  cl_eval(c_string_to_object("(setq si::*keep-documentation* t)"));
    
  ecl_init_module(NULL, init_lib_SOCKETS);
  ecl_init_module(NULL, init_lib_ASDF);

  cl_make_package(1, c_string_to_object("PD-CORE"));
  si_select_package(c_string_to_object("PD-CORE"));
  
  DEFUN("pd_post",print_to_pd_post,1);
  DEFUN("pd_error",print_to_pd_error,1);
  DEFUN("pd-outlet-anything", pdlisp_outlet_anything,5);
  DEFUN("pd-send-anything",pdlisp_send_anything,4);
  DEFUN("receiver-new", pdlisp_proxyreceive_new,2);
  DEFUN("receiver-free", pdlisp_proxyreceive_free,1);
  DEFUN("timer-new", pdlisp_proxyclock_new, 1);
  DEFUN("timer-free", pdlisp_proxyclock_free, 1);
  DEFUN("timer-set", pdlisp_clock_set,2);
  DEFUN("timer-delay", pdlisp_clock_delay,2);
  DEFUN("timer-unset", pdlisp_clock_unset,1);

  extern void init_lib_PDLISP_CORE(cl_object);
  ecl_init_module(NULL, init_lib_PDLISP_CORE);
  
  make_object = ecl_make_symbol("MAKE-OBJECT", "PD-CORE");
  get_new_object =  ecl_make_symbol("GET-NEW-OBJECT","PD-CORE");
  
  get_inlets = ecl_make_symbol("INLETS","PD-CORE");
  get_outlets = ecl_make_symbol("OUTLETS","PD-CORE");

  pd_ptr = ecl_make_symbol("PD-PTR","PD-CORE");
  set_pd_ptr = ecl_make_symbol("SET-PD-PTR","PD-CORE");


  load_f = ecl_make_symbol("PD-LOAD","PD-CORE");
  
  inlet_callback = ecl_make_symbol("INLET-CALLBACK","PD-CORE");
  receive_callback = ecl_make_symbol("RECEIVE-CALLBACK", "PD-CORE");
  clock_callback = ecl_make_symbol("CLOCK-CALLBACK", "PD-CORE");
  finalize = ecl_make_symbol("WRAP-FINALIZE","PD-CORE");
}
