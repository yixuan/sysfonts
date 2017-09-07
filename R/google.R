## Object to store the Google Fonts database
.pkg.env$.google.db = NULL

google_font_db = function()
{
    ## We need to use packages jsonlite and curl here
    
    ## If database already exists, return it
    if(!is.null(.pkg.env$.google.db))
        return(.pkg.env$.google.db)
    
    ## Else, download it

    ## Download from Google API, slow and relying on curl
    # baseurl = "https://www.googleapis.com/webfonts/v1/webfonts"
    # key = "AIzaSyDilHY_z1p7WltVNj5gOEHVHD3AIpW8R4o"
    # apiurl = sprintf("%s?key=%s", baseurl, key)
    # ret = curl::curl_fetch_memory(apiurl)

    ## Download from my own site, faster but not as up-to-date as Google
    db = paste(tempfile(), ".bz2", sep = "")
    db = tryCatch(
    
    ## First try to download the database
    {
        utils::download.file("http://statr.me/files/webfonts.bz2", db,
                             quiet = TRUE, mode = "wb")
        db
    },
    
    ## If not successful, use the built-in one
    error = function(e) {
        system.file("fonts", "webfonts.bz2", package = "sysfonts")
    }
    
    )
    
    con = bzfile(db, "rb")
    font_list = readLines(con)
    close(con)
    res = jsonlite::fromJSON(font_list, FALSE)
    .pkg.env$.google.db = res
    
    res
}

google_font_list = function()
{
    db = google_font_db()
    family = sapply(db[[2]], function(x) x$family)
    category = sapply(db[[2]], function(x) x$category)
    
    data.frame(family = family, category = category,
               stringsAsFactors = FALSE)
}

search_db = function(family)
{
    gflist = google_font_list()
    
    gffamily = gflist$family
    ind = grep(sprintf("^%s$", family), gffamily, ignore.case = TRUE)
    if(!length(ind))
    {
        ind = grep(family, gffamily, ignore.case = TRUE)
        if(length(ind))
        {
            cat("Do you mean one of the following font(s)?\n")
            cat(paste("  ", gffamily[ind], collapse = "\n"), "\n")
        }
        stop("font not found")
    }
    
    ind
}

# Download font file and return the path of destination
download_font_file = function(url, repo = "http://fonts.gstatic.com/", handle = curl::new_handle())
{
    ## We need to use curl package here
    
    dest = file.path(tempdir(), basename(url))
    
    if(repo != "http://fonts.gstatic.com/")
    {
        url_repo = gsub("http://fonts.gstatic.com/", repo, url)
        tryCatch(
            
            ## Try the user-specified repository
            {
                curl::curl_download(url_repo, dest, handle = handle)
            },
            
            ## If not successful, use the default one
            error = function(e) {
                message("font not found in the user-provided repo, try default repo...")
                curl::curl_download(url, dest, handle = handle)
            }
            
        )
    } else {
        curl::curl_download(url, dest, handle = handle)
    }
    
    dest
}



#' List Font Families Available in Google Fonts
#' 
#' This function lists family names of the fonts that are currently
#' available in Google Fonts. When running this function for the first time, 
#' it may take a few seconds to fetch the font information database.
#' This function requires the \pkg{jsonlite} and \pkg{curl} packages.
#' 
#' @return A character vector of available font family names in Google Fonts.
#' 
#' @seealso \code{\link{font.add.google}()}
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' 
#' @examples \dontrun{
#' font.families.google()
#' }
#' 
font.families.google = function()
{
    google_font_list()$family
}


#' Load Google Fonts into 'sysfonts'
#' 
#' This function will search the Google Fonts repository
#' (\url{https://fonts.google.com/}) for a specified
#' family name, download the proper font files, and then add them to \pkg{sysfonts}.
#' This function requires the \pkg{jsonlite} and \pkg{curl} packages.
#' 
#' @param name name of the font that will be searched in Google Fonts
#' @param family family name of the font that will be used in R
#' @param regular.wt font weight for the regular font face, usually 400
#' @param bold.wt font weight for the bold font face, usually 700
#' @param repo the site that hosts the font files. Default is the official
#'             repository \code{http://fonts.gstatic.com/} provided by
#'             Google Fonts.
#' @param handle a curl handle object passed to \code{curl::curl_download()}.
#' 
#' @details There are hundreds of open source fonts in the Google Fonts
#'          repository (\url{https://fonts.google.com/}).
#'          This function will try to search the font family specified
#'          by the \code{name} argument, and then automatically
#'          download the font files for all possible font faces
#'          ("regular", "bold", "italic" and "bold italic",
#'          but no"symbol").
#'          If fonts are found and downloaded successfully, they will be
#'          also added to \pkg{sysfonts} with the given family name.
#' 
#' @export
#' 
#' @seealso \code{\link{font.families.google}()}
#' 
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' 
#' @examples \dontrun{
#' font.add.google("Alegreya Sans", "aleg")
#' 
#' if(require(showtext))
#' {
#'     wd = setwd(tempdir())
#'     pdf("google-fonts-ex.pdf")
#'     showtext.begin()
#'     
#'     par(family = "aleg")
#'     plot(0:5,0:5, type="n")
#'     text(1:4, 1:4, "Alegreya Sans", font=1:4, cex = 2)
#'     
#'     showtext.end()
#'     dev.off()
#'     setwd(wd)
#' }
#' 
#' }
font.add.google = function(name, family = name, regular.wt = 400,
                           bold.wt = 700, repo = "http://fonts.gstatic.com/",
                           handle = curl::new_handle())
{   
    name   = as.character(name)[1]
    family = as.character(family)[1]
    repo   = as.character(repo)[1]
    
    db = google_font_db()
    ind = search_db(name)
    font = db[[2]][[ind]]
    
    ## Names of type variants to search in the db
    ## e.g., "regular", "700", "italic", 700italic", etc.
    regular = as.character(regular.wt)
    bold = as.character(bold.wt)
    italic = paste(regular, "italic", sep = "")
    bolditalic = paste(bold, "italic", sep = "")
    if(regular.wt == 400)
    {
        regular = "regular"
        italic = "italic"
    }
    
    ## Download regular font face
    r.url = font$files[[regular]]
    if(is.null(r.url))
        stop(sprintf("regular (weight=%d) variant of '%s' font not found", regular.wt, name))
    r.file = download_font_file(r.url, repo, handle = handle)
    
    ## Download bold font face
    b.url = font$files[[bold]]
    b.file = if(is.null(b.url)) NULL else download_font_file(b.url, repo, handle = handle)
    
    ## Download italic font face
    i.url = font$files[[italic]]
    i.file = if(is.null(i.url)) NULL else download_font_file(i.url, repo, handle = handle)
    
    ## Download bold-italic font face
    bi.url = font$files[[bolditalic]]
    bi.file = if(is.null(bi.url)) NULL else download_font_file(bi.url, repo, handle = handle)

    font.add(family, r.file, b.file, i.file, bi.file)
}
