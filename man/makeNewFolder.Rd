\name{makeNewFolder}
\alias{makeNewFolder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  make a new folder
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeNewFolder(parentId, folderId, Q, G)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parentId}{
%%     ~~Describe \code{parentId} here~~
}
  \item{folderId}{
%%     ~~Describe \code{folderId} here~~
}
  \item{Q}{
%%     ~~Describe \code{Q} here~~
}
  \item{G}{
%%     ~~Describe \code{G} here~~
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
function (parentId, folderId, Q, G)
{
    folderName <- G$name[folderId]
    folderParentId <- Q$newid[parentId]
    myFolder <- Folder(name = folderName, parentId = folderParentId)
    myFolder <- synStore(myFolder)
    return(myFolder$properties$id)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
