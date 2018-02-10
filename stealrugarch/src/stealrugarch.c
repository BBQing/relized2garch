#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


extern void fncA(void *, void *);
extern void real2garchfilterC(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,void *,void *);


static const R_CMethodDef CEntries [] = {
	{"fncA",(DL_FUNC) &fncA, 2},
		{"real2garchfilterC", (DL_FUNC) &real2garchfilterC,25 },
	{NULL,NULL,0}

}

;

void R_init_stealrugarch(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
