#include "RConverters.h"

SEXP
R_getIntPtr_els(SEXP r_obj, SEXP r_indices)
{
   int *obj = (int *) R_ExternalPtrAddr(r_obj);
   int len = Rf_length(r_indices), i;
   SEXP ans;

   PROTECT(ans = allocVector(INTSXP, len));
   for(i = 0; i < len; i++)  {
       int off = INTEGER(r_indices)[i];
       int val = obj[ off ];
       INTEGER(ans)[i] = val;
   }
   UNPROTECT(1);
   return(ans);
}


SEXP
R_getDoublePtr_els(SEXP r_obj, SEXP r_indices)
{
   double *obj; // = (double *) R_getNativeReference(r_obj, "doublePtr", "doublePtr");
   int len = Rf_length(r_indices), i;
   SEXP ans;

   obj = (double *) R_ExternalPtrAddr(r_obj);

   PROTECT(ans = allocVector(REALSXP, len));
   for(i = 0; i < len; i++)  {
       int off = INTEGER(r_indices)[i];
       double val = obj[ off ];
       REAL(ans)[i] = val;
   }
   UNPROTECT(1);
   return(ans);
}

SEXP
R_getDoublePtr_setEls(SEXP r_obj, SEXP r_indices, SEXP r_vals)
{
   double *obj = (double *) R_getNativeReference(r_obj, "doublePtr", "doublePtr");
   int len = Rf_length(r_indices), i;
   SEXP ans;

   for(i = 0; i < len; i++)  {
       int off = INTEGER(r_indices)[i];
       double val = obj[ off ];
       obj[i] = REAL(r_vals)[i];
   }
   return(r_obj);
}



SEXP
R_getStringFromPtr(SEXP ref, SEXP r_dup)
{
    char *ptr = (char *) R_ExternalPtrAddr(ref);
    return(ptr ? ScalarString(mkChar(INTEGER(r_dup)[0] ? strdup(ptr) : ptr)) : NEW_CHARACTER(0));
}
