#include <Rinternals.h>
#include <Rdefines.h>

#include "RConverters.h"

#ifdef __cplusplus
extern "C"
#endif
SEXP
R_RCppReference_addFinalizer(SEXP r_obj, SEXP finalizer)
{
 
    SEXP obj = r_obj;

    if(TYPEOF(obj) != EXTPTRSXP) 
	obj = GET_SLOT(r_obj, Rf_install("ref"));

    if(TYPEOF(obj) != EXTPTRSXP)  {
	PROBLEM "addFinalizer must be given an external pointer or an RC++Reference with a slot containing an external ptr"
	    ERROR;
    }

    if(R_ExternalPtrAddr(obj) == NULL) {
	PROBLEM "attempting to add a finalizer to a NULL pointer"
        ERROR;
    }


    if(TYPEOF(finalizer) == EXTPTRSXP) {
	//XXX check tag on the EXTPTRSXP to match that in the native symbol info.
	R_CFinalizer_t f = (R_CFinalizer_t) R_ExternalPtrAddr(finalizer);
	R_RegisterCFinalizer(obj, f);
    } else if(TYPEOF(finalizer) == CLOSXP) {
	R_RegisterFinalizer(obj, finalizer);
    }

    return(R_NilValue);
}
