#include "sysfonts.h"

SEXP loadFont(SEXP fontPath)
{
    const char* filePath = CHAR(STRING_ELT(fontPath, 0));
    pFontDesc font = (pFontDesc) calloc(1, sizeof(FontDesc));
    SEXP extPtr;
    FT_Error err;
    
    err = FT_Init_FreeType(&(font->library));
    if(err)
    {
        if(font) free(font);
        Rf_error("freetype: unable to initialize freetype, error code %d", err);
    }
    err = FT_New_Face(font->library, filePath, 0, &(font->face));
    if(err)
    {
        if(font->library) FT_Done_FreeType(font->library);
        if(font) free(font);
        switch(err)
        {
            case 0x01:
                Rf_error("freetype: cannot open resource, error code %d", err);
                break;
            case 0x02:
                Rf_error("freetype: unknown file format, error code %d", err);
                break;
            case 0x03:
                Rf_error("freetype: broken file, error code %d", err);
                break;
            default:
                Rf_error("freetype: unable to load font file, error code %d", err);
                break;
        }
    }
    
    extPtr = R_MakeExternalPtr(font, R_NilValue, R_NilValue);
    return extPtr;
}

SEXP cleanFont(SEXP extPtr)
{
    pFontDesc font = (pFontDesc) R_ExternalPtrAddr(extPtr);

    if(!font) return R_NilValue;
    
    if(font->face) FT_Done_Face(font->face);
    if(font->library) FT_Done_FreeType(font->library);
    if(font) free(font);
    
    return R_NilValue;
}

