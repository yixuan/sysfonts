\name{NEWS}
\title{News for Package "sysfonts"}

\section{Changes in sysfonts version 0.8.9}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated Google Fonts database.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated the linking on Windows for upcoming version of Rtools,
            thanks to Tomas Kalibera.
    }
  }
}

\section{Changes in sysfonts version 0.8.8}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{font_add()} now gives a warning if the parameters are not
            length-one vectors.
      \item C functions are now called using registered native symbols.
    }
  }
}

\section{Changes in sysfonts version 0.8.7}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated Google Fonts database.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated the documentation for \code{font_add_google()}.
      \item Fixed the bug that \code{db_cache = FALSE} is not effective in
            the second call of \code{font_add_google()}.
    }
  }
}

\section{Changes in sysfonts version 0.8.6}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Supported UCRT on Windows, thanks to Tomas Kalibera and Uwe Ligges.
    }
  }
}

\section{Changes in sysfonts version 0.8.5}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed the configure script, thanks to Prof. Brian Ripley.
      \item Fixed compiler warnings.
    }
  }
}

\section{Changes in sysfonts version 0.8.4}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated Google Fonts database.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Added a new search path in the configure script
            (\href{https://github.com/yixuan/sysfonts/issues/17}{#17}).
    }
  }
}

\section{Changes in sysfonts version 0.8.3}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a string comparison issue in the configure script
            (\href{https://github.com/yixuan/sysfonts/issues/14}{#14}).
    }
  }
}

\section{Changes in sysfonts version 0.8.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added a new system font path for macOS, suggested by Athanasia Mowinckel
            (\href{https://github.com/yixuan/sysfonts/issues/13}{#13}).
      \item Updated Google Fonts database.
    }
  }
}

\section{Changes in sysfonts version 0.8.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated Google Fonts database.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated the download method for installation on Windows.
    }
  }
}

\section{Changes in sysfonts version 0.8}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Now \code{font_files()} can report more information of the fonts, for
            example the family name, font face, version, and PostScript font name,
            suggested by pull request
            \href{https://github.com/eddelbuettel/binb/pull/7}{#7} of the \pkg{binb}
            package.
      \item Updated Google Fonts database.
    }
  }
}

\section{Changes in sysfonts version 0.7.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added function \code{font_info_google()} to retrieve metadata of
            Google Fonts (\href{https://github.com/yixuan/sysfonts/issues/7}{#7}),
            thanks to
            \href{https://github.com/camille-s}{@camille-s}.
      \item Added an argument \code{db_cache} to \code{font_info_google()},
            \code{font_families_google()}, and \code{font_add_google()} to force
            using Google Fonts API to retrieve font metadata, suggested by
            issue \href{https://github.com/yixuan/showtext/issues/23}{#23} of the
            \pkg{showtext} package.
      \item Updated Google Fonts database.
    }
  }
}

\section{Changes in sysfonts version 0.7.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Updated Google Fonts database.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed installation problems on Windows.
    }
  }
}

\section{Changes in sysfonts version 0.7.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed the use of one deprecated function.
    }
  }
}

\section{Changes in sysfonts version 0.7}{
  \subsection{API CHANGES}{
    \itemize{
      \item All previous API functions now have aliases replacing the dots
            with underscores in the function names, for example \code{font_add()}
            is equivalent to \code{font.add()}.
      \item The "underscore" naming is preferred, and the "dot" version will be
            gradually deprecated.
            
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added a "\code{handle}" parameter to \code{font_add_google()} in
            order to support proxies
            (\href{https://github.com/yixuan/sysfonts/pull/4}{#4}), thanks to
            \href{https://github.com/Sdurier}{@Sdurier}.
    }
  }
}

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
