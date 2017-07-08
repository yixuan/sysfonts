\name{NEWS}
\title{News for Package "sysfonts"}

\section{Changes in sysfonts version 0.6}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated the included Liberation fonts to version 1.07.4.
      \item Updated Google Fonts database.
      \item \pkg{sysfonts} now uses the \pkg{curl} package to fetch Google Fonts files.
      \item \code{font.add()} now supports tilde path expansion,
            for example \code{font.add("xkcd", "~/xkcd.ttf")}
            (\href{https://github.com/yixuan/sysfonts/issues/1}{#1}),
            thanks to \href{https://github.com/r2evans}{@r2evans}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Added a 'cleanup' script per CRAN policies.
      \item Registered native routines per CRAN policies.
    }
  }
}


\section{Changes in sysfonts version 0.5}{
  \subsection{BUG FIXES}{
    \itemize{
      \item More robust way to fetch font list.
    }
  }
}

\section{Changes in sysfonts version 0.4}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a problem reported by Philippe Massicotte involving
            incomplete downloading of font files.
    }
  }
}

\section{Changes in sysfonts version 0.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item No longer prints message at start-up.
      \item Now has faster loading of Google Fonts list.
    }
  }
}

\section{Changes in sysfonts version 0.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \pkg{sysfonts} can now add fonts from Google Fonts
            (\url{https://fonts.google.com/}).
    }
  }
}

\section{Changes in sysfonts version 0.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Initial release.
    }
  }
}
