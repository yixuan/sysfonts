#ifndef SYSFONTS_H_INCLUDED
#define SYSFONTS_H_INCLUDED

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SFNT_NAMES_H
#include FT_TRUETYPE_IDS_H

typedef struct fontDesc {
    FT_Library library;
    FT_Face face;
} FontDesc;

SEXP load_font  (SEXP font_path);
SEXP clean_font (SEXP font_desc_ptr);
SEXP font_name  (SEXP font_path);

#endif /* SYSFONTS_H_INCLUDED */
