\name{get_filedir}
\alias{set_filedir}
\alias{get_filedir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get the directory an Rscript is in. Can also set working directory to this}
\description{
  Simple functions for getting the rscript file directory and setting the working directory to this folder. This MUST be placed in the script and ran in rstudio. CANNOT run in console.
}
\usage{
get_filedir(file, sep = '/')
set_filedir()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The file name that is in the same directory as the open editor.}
  \item{sep}{The folder separator to be used if a file is specified.}
}

\details{
  This function is useful for loading data. Simply run \code{set_filedir} and have data file in the same folder. Then you do not need to specify a filepath.
}
\value{
}
\references{}
\author{Brian Keller}
\seealso{\code{\link{getwd}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
