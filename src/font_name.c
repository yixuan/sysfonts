#include "sysfonts.h"
#include <R_ext/Riconv.h>

/* Font names may use different languages and encodings. For simplicity
 * we only consider names in English with an ASCII/UTF-16BE encoding.
 * 
 * Return value: 0  - English name in ASCII
 *               1  - English name in UTF-16BE
 *               <0 - others
 */
char font_name_enc(FT_UShort platform_id, FT_UShort encoding_id, FT_UShort language_id)
{
    if((language_id != TT_MAC_LANGID_ENGLISH) &&
       (language_id != TT_MS_LANGID_ENGLISH_UNITED_STATES))
        return -1;
    
    if(platform_id == TT_PLATFORM_APPLE_UNICODE)
        return 1;
    
    if((platform_id == TT_PLATFORM_MACINTOSH) &&
       (encoding_id == TT_MAC_ID_ROMAN))
        return 0;
    
    if((platform_id == TT_PLATFORM_MICROSOFT) &&
       (encoding_id == TT_MS_ID_UNICODE_CS))
        return 1;
    
    return -1;
}

SEXP font_name(SEXP font_path)
{
    const char* file_path = CHAR(STRING_ELT(font_path, 0));
    FontDesc font;
    FT_Error err;
    FT_UInt num_entries, i, j;
    FT_SfntName name_table;
    char name_enc;
    void* riconv;
    size_t in_str_size, out_str_size;
    const char* in_str;
    char *out_str, *out_str_start;
    SEXP res;
    
    /* Result: a length-four string vector */
    /* family, face, version, ps_name */
    PROTECT(res = Rf_allocVector(STRSXP, 4));
    
    /* Initialize FreeType library */
    err = FT_Init_FreeType(&(font.library));
    if(err)
    {
        UNPROTECT(1);
        return res;
    }
    /* Open font face */
    err = FT_New_Face(font.library, file_path, 0, &(font.face));
    if(err)
    {
        if(font.library) FT_Done_FreeType(font.library);
        UNPROTECT(1);
        return res;
    }
    
    /* Get the number of name entries in the font */
    num_entries = FT_Get_Sfnt_Name_Count(font.face);
    for(i = 0; i < num_entries; i++)
    {
        /* Get the name entry */
        err = FT_Get_Sfnt_Name(font.face, i, &name_table);
        if(err)
            continue;
        /* Only extract English names */
        name_enc = font_name_enc(name_table.platform_id,
                                 name_table.encoding_id,
                                 name_table.language_id);
        if(name_enc < 0)
            continue;
        
        /* Map the entry ID to the index in the result */
        j = 99;
        switch(name_table.name_id)
        {
        case TT_NAME_ID_FONT_FAMILY:
            j = 0;
            break;
        case TT_NAME_ID_FONT_SUBFAMILY:
            j = 1;
            break;
        case TT_NAME_ID_VERSION_STRING:
            j = 2;
            break;
        case TT_NAME_ID_PS_NAME:
            j = 3;
            break;
        }
        
        if(j < 4)
        {
            /* Rprintf("str_len0 = %d, enc_id = %d, plat_id = %d\n",
                    name_table.string_len, name_table.encoding_id, name_table.platform_id); */
            
            /* If encoding is ASCII, we can directly write it to R */
            if(name_enc == 0)
            {
                SET_STRING_ELT(
                    res,
                    j,
                    Rf_mkCharLen((char*) name_table.string, name_table.string_len)
                );
            } else {
                /* Otherwise, the name is encoded in UTF-16BE, with each character
                 * occupying two bytes, and we need to convert it to UTF-8
                 */
                in_str = (const char*) name_table.string;
                in_str_size = out_str_size = name_table.string_len;
                out_str = (char*) calloc(out_str_size, sizeof(char));
                out_str_start = out_str;
                
                riconv = Riconv_open("UTF-8", "UTF-16BE");
                err = Riconv(riconv, &in_str, &in_str_size, &out_str, &out_str_size);
                Riconv_close(riconv);

                if(!err)
                {
                    SET_STRING_ELT(
                        res,
                        j,
                        Rf_mkCharLenCE(out_str_start, out_str - out_str_start, CE_UTF8)
                    );
                }
                
                free(out_str_start);
            }
        }
    }
    
    /* Free resources */
    if(font.face)    FT_Done_Face(font.face);
    if(font.library) FT_Done_FreeType(font.library);
    
    UNPROTECT(1);
    return res;
}
