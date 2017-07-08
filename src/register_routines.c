#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "sysfonts.h"

static R_CallMethodDef call_methods[] = {
    {"load_font",  (DL_FUNC) &load_font,  1},
    {"clean_font", (DL_FUNC) &clean_font, 1},
    {NULL, NULL, 0}
};

void R_init_sysfonts(DllInfo* info)
{
    R_registerRoutines(info, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
