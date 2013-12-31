\name{font.google}
\alias{font.google}
\title{Download and add Google Fonts}
\usage{
font.google(family, regular.wt = 400, bold.wt = 700)
}
\arguments{
  \item{family}{family name that will be searched in Google
  Fonts. Case sensitive.}

  \item{regular.wt}{font weight for the regular font face,
  usually 400}

  \item{bold.wt}{font weight for the bold font face,
  usually 700}
}
\description{
This function will search the Google Fonts repository for a
specified family name, download the proper font files and
then add them to R.
}
\details{
There are hundreds of open source fonts in the Google Fonts
repository (\url{http://www.google.com/fonts}).  This
function will try to search the font family specified by
the \code{family} argument, and then automatically download
the font files for all possible font faces ("regular",
"bold", "italic" and "bold italic", but no"symbol").  If
fonts are found and downloaded successfully, they will be
also added to R with the same family name.
}
\examples{
\dontrun{
font.google("Alegreya Sans");

if(require(showtext))
{
    wd = setwd(tempdir());
    pdf("google-fonts-ex.pdf");
    showtext.begin();

    par(family = "Alegreya Sans");
    plot(0:5,0:5, type="n");
    text(1:4, 1:4, "Alegreya Sans", font=1:4, cex = 2);

    showtext.end();
    dev.off();
    setwd(wd);
}

}
}
\author{
Yixuan Qiu <\url{http://yixuan.cos.name/}>
}

