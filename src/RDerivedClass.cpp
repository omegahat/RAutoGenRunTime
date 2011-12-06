#include "RDerivedClass.h"


RDerivedClass::RDerivedClass(SEXP methodsList)
{
  R_setMethods(methodsList);
}

SEXP 
RDerivedClass::lookupFunction(const char *name, SEXP methods) const
{
  int i, n;
  SEXP names;

  if(methods == NULL || methods == R_NilValue)
    return(NULL);

  names = GET_NAMES(methods);
  n = GET_LENGTH(names);
  for(i = 0; i < n; i++) {
    if(strcmp(name, CHAR(STRING_ELT(names, i))) == 0)
      return(VECTOR_ELT(methods, i));
  }

  return(NULL);
}


SEXP 
RDerivedClass::lookupFunction(const char *name) const
{
  SEXP ans;
  ans = lookupFunction(name, methods);
  if(!ans)
    ans = lookupFunction(name, get_R_sharedMethods());

  return(ans);
}




SEXP 
RDerivedClass::callMethod(SEXP expr)
{
  SEXP val;
  int errorOccurred = 0;

#if 0
  val = R_tryEval(expr, R_GlobalEnv, &errorOccurred);
#else
  val = Rf_eval(expr, R_GlobalEnv);
#endif

  if(errorOccurred) 
      raiseRError(expr);

  return(val);
}

void
RDerivedClass::raiseRError(SEXP expr)
{
    // throw a C++ exception.
  PROBLEM "problem when calling an R function as a method from a C++ class"
    ERROR;
}

const char *RDerivedClass::getMethodNames(int *num)
{
    if(num) 
        *num = 0;
    return(NULL);
}


SEXP 
RDerivedClass::R_GetMethodInfo()
{
  SEXP ans;
  int n = 0;

  const char *els = getMethodNames(&n);

  PROTECT(ans = allocVector(STRSXP, n));
  if(n > 0) {
    for(int i = 0 ; i < n; i++)
      SET_STRING_ELT(ans, i, mkChar(els));
  }

  UNPROTECT(1);
  return(ans);
}


SEXP Protected_Indicator_Attribute;

extern "C"
void
RDerivedClass_init()
{
  if(!Protected_Indicator_Attribute) {
    Protected_Indicator_Attribute = Rf_install("C++Protected");
    R_PreserveObject(Protected_Indicator_Attribute);
  }
}

SEXP
R_make_protected_callable(SEXP obj)
{
  RDerivedClass_init();

  PROTECT(obj);
  Rf_setAttrib(obj, Protected_Indicator_Attribute, ScalarLogical(TRUE));
  UNPROTECT(1);
  return(obj);
}



/* XXX
  This will introduce extra calls into the call stack.
  We need to be able to do this directl in the engine's
  C code, but we don't worry about that quite for the moment.
*/
void 
raiseError(const char *errorClass, const char *msg)
{
   raiseErrorWithValues(errorClass, msg, NULL);
}


#include <stdarg.h>

void 
raiseErrorWithValues(const char *errorClass, const char *msg, ...)
{
  SEXP  e, classes, obj;
  int numObjects = 0, i;
  va_list ap;

  va_start(ap, msg);
  while(va_arg(ap, void *))
      numObjects++;
  va_end(ap);
  numObjects /= 2;



  PROTECT(e = allocVector(LANGSXP, 2));
  SETCAR(e, Rf_install("stop"));

  PROTECT(obj = NEW_LIST(2 + numObjects));
  SET_VECTOR_ELT(obj, 0, mkString(msg));
  PROTECT(classes = NEW_CHARACTER(2 + numObjects));
  SET_STRING_ELT(classes, 0, mkChar("message"));
  SET_STRING_ELT(classes, 1, mkChar("call"));
  
  va_start(ap, msg);
  for(i = 0; i < numObjects; i++) {
     SET_STRING_ELT(classes, i + 2,  mkChar(va_arg(ap, const char *)));
     SET_VECTOR_ELT(obj, i + 2,va_arg(ap, SEXP));
  }

  SET_NAMES(obj, classes);

  PROTECT(classes = NEW_CHARACTER(3));
  SET_STRING_ELT(classes, 0, mkChar(errorClass));
  SET_STRING_ELT(classes, 1, mkChar("error"));
  SET_STRING_ELT(classes, 2, mkChar("condition"));
  SET_CLASS(obj, classes);
  
  SETCAR(CDR(e), obj);
  Rf_eval(e, R_GlobalEnv);
  /* Never get back here. */
  UNPROTECT(4);
}


#if 0

#ifdef __cplusplus
#include <typeinfo>
extern "C"
SEXP
R_typeid(SEXP r_This)
{
    void * This ;
    This  =  R_getNativeReference(r_This, "Circle", NULL) ;
    //    const std::type_info type(typeid(*This));
    return(mkString(typeid(*This).name()));
}
#endif
#endif


extern "C"
SEXP
R_getDerivedClassMethods(SEXP r_This)
{
  RDerivedClass *This;
  //  This = (RDerivedClass *) R_getNativeReference(r_This, NULL, NULL);
  // fails
  This = static_cast<RDerivedClass *>(R_getNativeReference(r_This, NULL, NULL));
  This = dynamic_cast<RDerivedClass *>((RDerivedClass *)R_getNativeReference(r_This, NULL, NULL));

  if(!This) {
    PROBLEM ""
      ERROR;
  }

  return(This->R_getMethods());
}


extern "C"
int
RDerivedClass_isProtectedNativeObject(SEXP r_this)
{
  RDerivedClass_init();                                                
  SEXP tmp = Rf_getAttrib(r_this, Protected_Indicator_Attribute);   

  return(tmp != R_NilValue && TYPEOF(tmp) == LGLSXP && Rf_asLogical(tmp) == TRUE);
}

extern "C"
SEXP
R_isProtectedNativeObject(SEXP r_This)
{
  return(ScalarLogical(RDerivedClass_isProtectedNativeObject(r_This)));
}
