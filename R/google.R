# something like
# @font-face {
#     font-family: 'Coming Soon';
#     font-style: normal;
#     font-weight: 400;
#     src: local('Coming Soon'), local('ComingSoon'), url(http://themes.googleusercontent.com/static/fonts/comingsoon/v3/myblyOycMnPMGjfPG-DzP4bN6UDyHWBl620a-IRfuBk.woff) format('woff');
# }
.parse.url = function(str)
{
    line = grep("url\\(http://.*\\)", str, value = TRUE);
    url = gsub(".*url\\(([^\\)]*)\\).*", "\\1", line);
    return(url);
}

# download font file and return the path of destination
.download.file = function(url)
{
    path = file.path(tempdir(), basename(url));
    download.file(url, path, quiet = TRUE, mode = "wb");
    if(!file.exists(path)) stop("failed to download font file");
    return(path);
}

#' Download and add Google Fonts
#' 
#' This function will search the Google Fonts repository for a specified
#' family name, download the proper font files and then add them to R.
#' 
#' @param family family name that will be searched in Google Fonts. Case
#'               sensitive.
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
#'          also added to R with the same family name.
#' 
#' @export
#' 
#' @author Yixuan Qiu <\url{http://yixuan.cos.name/}>
#' 
#' @examples \dontrun{
#' font.google("Alegreya Sans");
#' 
#' if(require(showtext))
#' {
#'     wd = setwd(tempdir());
#'     pdf("google-fonts-ex.pdf");
#'     showtext.begin();
#'     
#'     par(family = "Alegreya Sans");
#'     plot(0:5,0:5, type="n");
#'     text(1:4, 1:4, "Alegreya Sans", font=1:4, cex = 2);
#'     
#'     showtext.end();
#'     dev.off();
#'     setwd(wd);
#' }
#' 
#' }
font.google = function(family, regular.wt = 400, bold.wt = 700)
{
    regular.wt = as.integer(regular.wt);
    bold.wt = as.integer(bold.wt);
    familysp = family;
    family = gsub(" ", "+", family);
    
    ## download regular font face
    r.page = sprintf("http://fonts.googleapis.com/css?family=%s:%d",
                     family, regular.wt);
    link = url(r.page);
    r.url = tryCatch(.parse.url(readLines(link)),
                     error = function(e) {stop("font not found")});
    close(link);
    r.file = .download.file(r.url);
    
    ## download bold font face
    b.page = sprintf("http://fonts.googleapis.com/css?family=%s:%d",
                     family, bold.wt);
    link = url(b.page);
    b.url = tryCatch(.parse.url(readLines(link)),
                     warning = function(w) NULL,
                     error = function(e) NULL);
    close(link);
    b.file = if(is.null(b.url)) NULL else .download.file(b.url);
    
    ## download italic font face
    i.page = sprintf("http://fonts.googleapis.com/css?family=%s:%ditalic",
                     family, regular.wt);
    link = url(i.page);
    i.url = tryCatch(.parse.url(readLines(link)),
                     warning = function(w) NULL,
                     error = function(e) NULL);
    close(link);
    i.file = if(is.null(i.url)) NULL else .download.file(i.url);
    
    ## download bold-italic font face
    bi.page = sprintf("http://fonts.googleapis.com/css?family=%s:%ditalic",
                      family, bold.wt);
    link = url(bi.page);
    bi.url = tryCatch(.parse.url(readLines(link)),
                     warning = function(w) NULL,
                     error = function(e) NULL);
    close(link);
    bi.file = if(is.null(bi.url)) NULL else .download.file(bi.url);
    
    font.add(familysp, r.file, b.file, i.file, bi.file);
}
