\name{crawlSynapseEntity}
\alias{crawlSynapseEntity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  recursively query the file hiearachy of a given synapse entity
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crawlSynapseEntity(synId, G = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synId}{
%%     ~~Describe \code{synId} here~~
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
function (synId, G = NULL)
{
    result <- adjacentEdges(synId)
    while (!is.null(result)) {
        if (is.null(G)) {
            G <- list()
            id1 <- c(synId, result$entity.id)
            name1 <- c("head", result$entity.name)
            type1 <- c("head", result$entity.concreteType)
            G$id <- id1
            G$name <- name1
            G$type <- type1
            G$adjList <- vector("list", length(G$id))
            names(G$adjList) <- G$id
        }
        else {
            a <- which(!result$entity.id \%in\% G$id)
            id1 <- result$entity.id[a]
            name1 <- result$entity.name[a]
            type1 <- result$entity.concreteType[a]
            if (length(a) > 0) {
                G$adjList <- c(G$adjList, vector("list", length(id1)))
                G$id <- c(G$id, id1)
                G$name <- c(G$name, name1)
                G$type <- c(G$type, type1)
                names(G$adjList) <- G$id
            }
        }
        for (i in 1:length(result$entity.id)) {
            if (!as.character(result$entity.id[i]) \%in\% G$adjList[[as.character(synId)]]) {
                G$adjList[[as.character(synId)]] <- c(G$adjList[[as.character(synId)]],
                  as.character(result$entity.id[i]))
                cat("Crawling", result$entity.id[i], "\n")
                cat("Number of branches:", length(c(unlist(G$adjList))),
                  "\n")
                cat("Number of nodes:", length(G$adjList), "\n")
                G <- crawlSynapseEntity(result$entity.id[i],
                  G)
            }
            else {
                return(G)
            }
        }
    }
    return(G)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
