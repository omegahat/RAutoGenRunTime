#ifndef RDERIVEDCLASS_H
#define RDERIVEDCLASS_H

#include <Rinternals.h>
#include <Rdefines.h>

#include "RConverters.h"

class RDerivedClass {

 public:

  RDerivedClass(SEXP methods);
  RDerivedClass() : methods(NULL) {};

  static SEXP R_GetMethodInfo();
  static const char *getMethodNames(int *num);

  virtual SEXP R_getMethods() {
    return(methods);
  };

  virtual void R_setMethods(SEXP newMethods) {
	  if(methods) 
   	     R_ReleaseObject(methods);
	  methods = newMethods;
	  if(newMethods && newMethods != R_NilValue)
	      R_PreserveObject(methods);
  }

#if 0
  virtual SEXP r_getMethodNames() = 0;
#endif


  virtual SEXP get_R_sharedMethods() const = 0;

 protected:

  SEXP lookupFunction(const char *name) const;
  SEXP lookupFunction(const char *name, SEXP methods) const;

  SEXP callMethod(SEXP expr);

  void raiseRError(SEXP expr);

 protected:
  SEXP methods;

#ifdef RTU_USE_INDIVIDUAL_METHOD_FIELDS
  SEXP _R_finalize_m;
  SEXP _R_new_m;
#endif
};


#include "RError.h"
   
extern SEXP Protected_Indicator_Attribute;

extern "C" void RDerivedClass_init();
extern "C" int RDerivedClass_isProtectedNativeObject(SEXP r_This);

#define CHECK_IS_PROTECTED(r_this) \
  {  \
     if(!RDerivedClass_isProtectedNativeObject(r_this)) {		\
        PROBLEM "cannot call the protected method outside of a protected R function" \
        R_ERROR("ProtectedMethodInvocationError");	\
     } \
  }




SEXP R_make_protected_callable(SEXP obj);


#endif
