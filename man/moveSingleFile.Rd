\name{moveSingleFile}
\alias{moveSingleFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Moves a file to a new location on Synapse and creates a link where the file used to be
%%  ~~function to do ... ~~
}
\description{ Moves a file to a new location on Synapse and creates a link where the file used to be
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moveSingleFile(synId, newParentId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synId}{  the Synapse ID of the file to be moved.
%%     ~~Describe \code{synId} here~~

}
  \item{newParentId}{  the Synapse ID of the new parent folder or project where the file will be moved to.
%%     ~~Describe \code{newParentId} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (synId, newParentId)
{
    foo <- synGet(synId, downloadFile = F)
    oldParentId <- synGetProperties(foo)$parentId
    fileName <- synGetProperties(foo)$name
    moveFile(synId, newParentId)
    makeLink(synId, oldParentId, linkName = fileName)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
