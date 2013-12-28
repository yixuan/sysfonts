\name{font.families}
\alias{font.families}
\title{List available font families loaded by sysfonts}
\usage{
font.families()
}
\value{
A character vector of available font family names
}
\description{
This function lists font families currently available that
can be used by \pkg{R2SWF} and \pkg{showtext} packages.
}
\details{
By default there are three font families loaded
automatically, i.e., "sans", "serif" and "mono". If you
want to use other ones, you need to call
\code{\link{font.add}()} to register new fonts by
specifying a family name and corresponding font file paths.
See \code{\link{font.add}()} for details about what's the
meaning of "family name" in this context, as well as a
complete example of registering and using a new font.
}
\examples{
font.families()
}
\author{
Yixuan Qiu <\url{http://yixuan.cos.name/}>
}
\seealso{
\code{\link{font.add}()}
}

