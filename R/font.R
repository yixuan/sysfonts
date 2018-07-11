# Environment to store several important variables
# Note that the name of this variable should not be changed,
# since showtext relies on the name to find this variable.
.pkg.env = new.env()

# Current font list, a list of pointers to freetype structures
# The name of this variable should not be changed, since showtext
# relies on the name to find this variable.
.pkg.env$.font.list = list()

# All fonts previously added, used to free memories when exiting
.pkg.env$.font.list.all = list()

# Font search path
.pkg.env$.font.path = character(0)

# Add default font search paths
add_default_font_paths = function()
{
    if(.Platform$OS.type == "windows") {
        path = normalizePath(file.path(Sys.getenv("windir"), "Fonts"))
    } else if(.Platform$OS.type == "unix") {
        if(Sys.info()["sysname"] == "Darwin")
        {
            path = list.dirs(c("/Library/Fonts",
                               "~/Library/Fonts"))
        } else {
            path = list.dirs(c("/usr/share/fonts",
                               "/usr/local/share/fonts",
                               "~/.fonts",
                               "~/.local/share/fonts"))
        }
    } else stop("unknown OS type")
    
    .pkg.env$.font.path = path
}

#' Get/Set Font Search Paths
#' 
#' @description The two versions of this function are equivalent, but the
#' "underscore" naming is preferred.
#' 
#' This function gets/sets the search paths for font files.
#' See \code{\link{font_add}()} for details about how \pkg{sysfonts} looks for
#' font files. There is also a complete example showing the usage of these
#' functions in the help page of \code{\link{font_add}()}.
#' 
#' @param new a character vector indicating the search paths to be
#'        prepended. If the argument is missing, the function will
#'        return the current search paths.
#' @return The updated search paths.
#' 
#' @details Default search paths will be assigned when package is loaded:
#' \itemize{
#' \item For Windows, it is \code{\%windir\%\\Fonts}, usually expanded
#'       into \code{C:\\Windows\\Fonts}
#'
#' \item For Mac OS, default paths are \code{/Library/Fonts}
#'       and \code{~/Library/Fonts} and their subdirectories
#'
#' \item For Linux and other Unix-like OS, \code{/usr/share/fonts},
#'       \code{/usr/local/share/fonts}, \code{~/.fonts},
#'       \code{~/.local/share/fonts}, and their subdirectories
#' }
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{https://statr.me/}>
font_paths = function(new)
{
    if(!missing(new))
    {
        new = path.expand(new)
        paths = unique(normalizePath(c(new, .pkg.env$.font.path)))
        .pkg.env$.font.path = paths
    }
    
    .pkg.env$.font.path
}

#' @rdname font_paths
#' @export
font.paths = function(new)
{
    deprecate_message_once("fond.paths()", "font_paths()")
    font_paths(new)
}

#' List Font Families Loaded by 'sysfonts'
#' 
#' @description The two versions of this function are equivalent, but the
#' "underscore" naming is preferred.
#' 
#' This function lists font families currently available that can be
#' used by \pkg{R2SWF} and \pkg{showtext} packages.
#' 
#' @return A character vector of available font family names.
#' 
#' @details By default there are three font families loaded automatically,
#' i.e., "sans", "serif" and "mono". If one wants to use other fonts,
#' \code{\link{font_add}()} needs to be called
#' to register new fonts by specifying a family name and corresponding
#' font files. See \code{\link{font_add}()} for details about
#' the meaning of "family name" in this context, as well as
#' a complete example of registering and using a new font.
#' 
#' @seealso \code{\link{font_add}()}
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{https://statr.me/}>
#' 
#' @examples font_families()
#' 
font_families = function()
{
    names(.pkg.env$.font.list)
}

#' @rdname font_families
#' @export
font.families = function()
{
    deprecate_message_once("font.families()", "font_families()")
    font_families()
}

#' List Font Files Available in the Search Paths
#' 
#' @description The two versions of this function are equivalent, but the
#' "underscore" naming is preferred.
#' 
#' This function lists font files in the search path that can be
#' loaded by \code{\link{font_add}()}.
#' Currently supported formats include TrueType fonts(*.ttf, *.ttc) and OpenType fonts(*.otf).
#' 
#' @return A character vector of font filenames.
#' 
#' @seealso \code{\link{font_paths}()}, \code{\link{font_add}()}
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{https://statr.me/}>
#' 
#' @examples font_files()
#' 
font_files = function()
{
    list.files(font_paths(), "\\.tt[cf]$|\\.otf$", ignore.case = TRUE)
}

#' @rdname font_files
#' @export
font.files = function()
{
    deprecate_message_once("font.files()", "font_files()")
    font_files()
}

# Check whether a specified path points to a font file
check_font_path = function(path, type)
{
    # If it really exists
    if(file.exists(path) && !file.info(path)$isdir)
        return(normalizePath(path))
    
    # If it doesn't exist, search the file in the search paths
    filename = basename(path)
    search_paths = font_paths()
    found = FALSE
    for(dir in search_paths)
    {
        path = file.path(dir, filename)
        if(file.exists(path) && !file.info(path)$isdir)
        {
            found = TRUE
            break
        }
    }
    if(!found) stop(sprintf("font file not found for '%s' type", type))
    
    normalizePath(path)
}

#' Add New Font Families to 'sysfonts'
#' 
#' @description The two versions of this function are equivalent, but the
#' "underscore" naming is preferred.
#' 
#' This function registers new font families that can be used by package
#' \pkg{showtext} and the SWF device in package \pkg{R2SWF}.
#' Currently supported formats include but not limited to
#' TrueType fonts(*.ttf, *.ttc) and OpenType fonts(*.otf).
#' 
#' @param family a character string of maximum 200-byte size,
#'               indicating the family name of the font.
#'               See "Details" for further explanation.
#' @param regular path of the font file for "regular" font face.
#'                This argument must be specified as a character string
#'                and cannot be missing.
#' @param bold path of the font file for "bold" font face.
#'             If it is \code{NULL}, the function will use the value of
#'             argument \code{regular}.
#' @param italic,bolditalic,symbol ditto
#' 
#' @return A character vector (invisible) of currently available
#'         font family names.
#' 
#' @details In R graphics device, there are two parameters combined together
#' to select a font to show text. \code{par("family")} is a character
#' string giving a name to a \strong{series} of font faces. Here
#' \strong{series} implies that there may be different fonts with the
#' same family name, and actually they are distinguished by the parameter
#' \code{par("font")}, indicating whether it is regular, bold, or italic,
#' etc. In R, \code{par("font")} is an integer from 1 to 5 representing
#' regular, bold, italic, bold italic, and symbol, respectively.
#' 
#' In \pkg{sysfonts} package, there are three default font families, sans, serif, and mono,
#' each with five font faces as mentioned above. If one wants
#' to use other font families, the function \code{font_add()} needs to be called
#' to register new fonts. Note that the \code{family} argument in this function can be
#' an arbitrary string that does not need to be the real font name. The specified
#' family name will be used in functions like \code{par(family = "myfont")}
#' and \code{text("Some text", family = "myfont")}. The \strong{Examples} section
#' shows a complete demonstration of the usage.
#' 
#' To find the font file of argument \code{regular} (and the same for
#' other font faces), this function will first check the existence
#' of the specified path. If not found, file will be searched in the
#' directories returned by \code{\link{font_paths}()} in turn. If the
#' file cannot be found in any of the locations,
#' an error will be issued.
#' 
#' @seealso See \code{\link[graphics]{par}()} for explanation of
#'          the parameters \code{family} and \code{font}.
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{https://statr.me/}>
#' 
#' @examples \dontrun{
#' ## Example: download the font file of WenQuanYi Micro Hei,
#' ##          add it to SWF device, and use it to draw text in swf().
#' ##          WenQuanYi Micro Hei is an open source and high quality
#' ##          Chinese (and CJKV) font.
#' 
#' wd = setwd(tempdir())
#' ft.url = "http://sourceforge.net/projects/wqy/files/wqy-microhei"
#' ft.url = paste(ft.url, "0.2.0-beta/wqy-microhei-0.2.0-beta.tar.gz",
#'                sep = "/")
#' download.file(ft.url, basename(ft.url))
#'
#' ## Extract and add the directory to search path
#' untar(basename(ft.url), compressed = "gzip")
#' font_paths("wqy-microhei")
#'
#' ## Register this font file and assign the family name "wqy"
#' ## Other font faces will be the same with regular by default
#' font_add("wqy", regular = "wqy-microhei.ttc")
#' 
#' ## A more concise way to add font is to give the path directly,
#' ## without calling font_paths()
#' # font_add("wqy", "wqy-microhei/wqy-microhei.ttc")
#' 
#' ## List available font families
#' font_families()
#'
#' if(require(R2SWF))
#' {
#'     ## Now it shows that we can use the family "wqy" in swf()
#'     swf("testfont.swf")
#'
#'     ## Select font family globally
#'     op = par(family = "serif", font.lab = 2)
#'     ## Inline selecting font
#'     plot(1, type = "n")
#'     text(1, 1, intToUtf8(c(20013, 25991)), family = "wqy", font = 1, cex = 2)
#'
#'     dev.off()
#'     swf2html("testfont.swf")
#' }
#'
#' setwd(wd)
#' 
#' }
font_add = function(family,
                    regular,
                    bold = NULL,
                    italic = NULL,
                    bolditalic = NULL,
                    symbol = NULL)
{
    family = as.character(family)[1]
    
    # Shouldn't modify default fonts
    if((family %in% c("sans", "serif", "mono")) &&
       (all(c("sans", "serif", "mono") %in% font_families())))
        stop("default font families ('sans', 'serif', 'mono') cannot be modified")
    
    # The maximum length for font family name is 200 bytes
    if(nchar(family, type = "bytes") > 200)
        stop("family name is too long (max 200 bytes)")
    
    r = .Call("load_font", check_font_path(regular, "regular"), PACKAGE = "sysfonts");
    
    # If other font faces are not specified, use the regular one
    b = if(is.null(bold)) r
        else .Call("load_font", check_font_path(bold, "bold"), PACKAGE = "sysfonts");
    
    i = if(is.null(italic)) r
        else .Call("load_font", check_font_path(italic, "italic"), PACKAGE = "sysfonts");
    
    bi = if(is.null(bolditalic)) r
         else .Call("load_font", check_font_path(bolditalic, "bolditalic"), PACKAGE = "sysfonts");
    
    s = if(is.null(symbol)) r
        else .Call("load_font", check_font_path(symbol, "symbol"), PACKAGE = "sysfonts");
    
    lst = .pkg.env$.font.list
    new_family = list(regular = r, bold = b, italic = i, bolditalic = bi, symbol = s)
    lst[[family]] = new_family
    .pkg.env$.font.list = lst
    .pkg.env$.font.list.all = c(.pkg.env$.font.list.all, new_family)
    
    invisible(font_families())
}

#' @rdname font_add
#' @export
font.add = function(family,
                    regular,
                    bold = NULL,
                    italic = NULL,
                    bolditalic = NULL,
                    symbol = NULL)
{
    deprecate_message_once("font.add()", "font_add()")
    font_add(family, regular, bold, italic, bolditalic, symbol)
}

# Use font_add() to add default fonts
add_default_fonts = function()
{
    # packageStartupMessage("Loading fonts...")

    lib.loc = if("sysfonts" %in% loadedNamespaces())
                  dirname(getNamespaceInfo("sysfonts", "path"))
              else NULL
    
    default_fonts_path = function(family, face)
    {
        system.file("fonts", sprintf("Liberation%s-%s.ttf", family, face),
                    package = "sysfonts", lib.loc = lib.loc)
    }

    sans.r   = default_fonts_path("Sans",  "Regular")
    sans.b   = default_fonts_path("Sans",  "Bold")
    sans.i   = default_fonts_path("Sans",  "Italic")
    sans.bi  = default_fonts_path("Sans",  "BoldItalic")
    
    serif.r  = default_fonts_path("Serif", "Regular")
    serif.b  = default_fonts_path("Serif", "Bold")
    serif.i  = default_fonts_path("Serif", "Italic")
    serif.bi = default_fonts_path("Serif", "BoldItalic")
    
    mono.r   = default_fonts_path("Mono",  "Regular")
    mono.b   = default_fonts_path("Mono",  "Bold")
    mono.i   = default_fonts_path("Mono",  "Italic")
    mono.bi  = default_fonts_path("Mono",  "BoldItalic")
    
    font_add("sans",  sans.r,  sans.b,  sans.i,  sans.bi,  NULL)
    font_add("serif", serif.r, serif.b, serif.i, serif.bi, NULL)
    font_add("mono",  mono.r,  mono.b,  mono.i,  mono.bi,  NULL)
    
    # We do some "hacks" here. For default families(sans, serif, mono),
    # we want to set their symbol fonts to be serif-italic
    lst = .pkg.env$.font.list
    lst[["sans"]][["symbol"]]  = lst[["serif"]][["italic"]]
    lst[["serif"]][["symbol"]] = lst[["serif"]][["italic"]]
    lst[["mono"]][["symbol"]]  = lst[["serif"]][["italic"]]
    .pkg.env$.font.list = lst
    
    # packageStartupMessage("Loading fonts finished")
    
    invisible(NULL)
}

# Free memories when exiting
clean_fonts = function()
{
    lst = unique(unlist(.pkg.env$.font.list.all))
    for(i in seq_along(lst))
    {
        .Call("clean_font", lst[[i]], PACKAGE = "sysfonts")
    }
    .pkg.env$.font.list = list()
    .pkg.env$.font.list.all = list()
    gc()
    invisible(NULL)
}
