\name{font.families.google}
\alias{font.families.google}
\title{List available fonts in Google Fonts}
\usage{
font.families.google()
}
\value{
A character vector of available font family names in Google
Fonts
}
\description{
This function lists family names of the fonts that are
currently available in Google Fonts. When running this
function for the first time, it may take a few seconds to
fetch the font information database. This function requires
the packages \pkg{RCurl} and \pkg{jsonlite} to be
installed.
}
\examples{
\dontrun{
font.families.google()
}
}
\author{
Yixuan Qiu <\url{http://yixuan.cos.name/}>
}
\seealso{
\code{\link{font.add.google}()}
}

