## Object to store the Google Fonts database
.pkg.env$.google.db = NULL;

.google.font.db = function()
{
    ## If database already exists, return it
    if(!is.null(.pkg.env$.google.db))
        return(.pkg.env$.google.db);
    
    ## Else, download it

    ## Download from Google API, slow and relying on RCurl
    # baseurl = "https://www.googleapis.com/webfonts/v1/webfonts";
    # key = "AIzaSyDilHY_z1p7WltVNj5gOEHVHD3AIpW8R4o";
    # apiurl = sprintf("%s?key=%s", baseurl, key);
    # ret = getURLContent(apiurl, ssl.verifypeer = FALSE);

    ## Download from my own site, faster but not as up-to-date as Google
    webfonts = url("http://statr.me/files/webfonts");
    ret = readLines(webfonts);
    close(webfonts);

    res = jsonlite::fromJSON(ret, FALSE);
    .pkg.env$.google.db = res;
    return(res);
}

.google.font.list = function()
{
    db = .google.font.db();
    family = sapply(db[[2]], function(x) x$family);
    category = sapply(db[[2]], function(x) x$category);
    return(data.frame(family = family, category = category,
                      stringsAsFactors = FALSE));
}

.search.db = function(family)
{
    gflist = .google.font.list();
    gffamily = gflist$family;
    ind = grep(sprintf("^%s$", family), gffamily, ignore.case = TRUE);
    if(!length(ind))
    {
        ind = grep(family, gffamily, ignore.case = TRUE);
        if(length(ind))
        {
            cat("Do you mean one of the following font(s)?\n");
            cat(paste("  ", gffamily[ind], collapse = "\n"), "\n");
        }
        stop("font not found");
    }
    return(ind);
}

# download font file and return the path of destination
.download.file = function(url)
{
    # Use proxy instead of the original link address
    url = gsub("fonts\\.gstatic\\.com", "fontstatic\\.useso\\.com", url);
    
    path = file.path(tempdir(), basename(url));
    tryCatch(download.file(url, path, quiet = TRUE, mode = "wb"),
             warning = function(w) NULL,
             error = function(e) NULL);
    if(!file.exists(path)) stop("failed to download font file");
    return(path);
}



#' List available fonts in Google Fonts
#' 
#' This function lists family names of the fonts that are currently
#' available in Google Fonts. When running this function for the first time, 
#' it may take a few seconds to fetch the font information database.
#' This function requires \pkg{RCurl} and \pkg{jsonlite} packages.
#' 
#' @return A character vector of available font family names in Google Fonts
#' 
#' @seealso \code{\link{font.add.google}()}
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{http://yixuan.cos.name/}>
#' 
#' @examples \dontrun{
#' font.families.google()
#' }
#' 
font.families.google = function()
{
    if(!require("RCurl"))
        stop("cannot load 'RCurl' package");
    if(!require("jsonlite"))
        stop("cannot load 'jsonlite' package");
    
    return(.google.font.list()$family);
}


#' Download and add Google Fonts
#' 
#' This function will search the Google Fonts repository
#' (\url{http://www.google.com/fonts}) for a specified
#' family name, download the proper font files and then add them to R.
#' This function requires \pkg{RCurl} and \pkg{jsonlite} packages.
#' 
#' @param name name of the font that will be searched in Google Fonts
#' @param family family name of the font that will be used in R
#' @param regular.wt font weight for the regular font face, usually 400
#' @param bold.wt font weight for the bold font face, usually 700
#' 
#' @details There are hundreds of open source fonts in the Google Fonts
#'          repository (\url{http://www.google.com/fonts}).
#'          This function will try to search the font family specified
#'          by the \code{family} argument, and then automatically
#'          download the font files for all possible font faces
#'          ("regular", "bold", "italic" and "bold italic",
#'          but no"symbol").
#'          If fonts are found and downloaded successfully, they will be
#'          also added to R with the given family name.
#' 
#' @export
#' 
#' @seealso \code{\link{font.families.google}()}
#' 
#' @author Yixuan Qiu <\url{http://yixuan.cos.name/}>
#' 
#' @examples \dontrun{
#' font.add.google("Alegreya Sans", "aleg");
#' 
#' if(require(showtext))
#' {
#'     wd = setwd(tempdir());
#'     pdf("google-fonts-ex.pdf");
#'     showtext.begin();
#'     
#'     par(family = "aleg");
#'     plot(0:5,0:5, type="n");
#'     text(1:4, 1:4, "Alegreya Sans", font=1:4, cex = 2);
#'     
#'     showtext.end();
#'     dev.off();
#'     setwd(wd);
#' }
#' 
#' }
font.add.google = function(name, family = name, regular.wt = 400,
                           bold.wt = 700)
{
    if(!require("RCurl"))
        stop("cannot load 'RCurl' package");
    if(!require("jsonlite"))
        stop("cannot load 'jsonlite' package");
    
    name = as.character(name);
    
    db = .google.font.db();
    ind = .search.db(name);
    font = db[[2]][[ind]];
    
    ## names of type variants to search in the db
    ## e.g., "regular", "700", "italic", 700italic", etc.
    regular = as.character(regular.wt);
    bold = as.character(bold.wt);
    italic = paste(regular, "italic", sep = "");
    bolditalic = paste(bold, "italic", sep = "");
    if(regular.wt == 400)
    {
        regular = "regular";
        italic = "italic"
    }
    
    ## download regular font face
    r.url = font$files[[regular]];
    if(is.null(r.url))
        stop(sprintf("regular(weight=%d) variant of '%s' font not found",
                     regular.wt, name));
    r.file = .download.file(r.url);
    
    ## download bold font face
    b.url = font$files[[bold]];
    b.file = if(is.null(b.url)) NULL else .download.file(b.url);
    
    ## download italic font face
    i.url = font$files[[italic]];
    i.file = if(is.null(i.url)) NULL else .download.file(i.url);
    
    ## download bold-italic font face
    bi.url = font$files[[bolditalic]];
    bi.file = if(is.null(bi.url)) NULL else .download.file(bi.url);

    font.add(family, r.file, b.file, i.file, bi.file);
}
