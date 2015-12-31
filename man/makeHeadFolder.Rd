\name{makeHeadFolder}
\alias{makeHeadFolder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  make the top entity a folder
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeHeadFolder(synObj, synid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synObj}{
%%     ~~Describe \code{synObj} here~~
}
  \item{synid}{
%%     ~~Describe \code{synid} here~~
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
function (synObj, synid)
{
    synObj$adj[1, 1] <- 1
    synObj$type[1] <- "org.sagebionetworks.repo.model.Folder"
    synObj2 <- synGet(synid, downloadFile = F)
    synObj$name[1] <- synObj2@properties$name
    return(synObj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
