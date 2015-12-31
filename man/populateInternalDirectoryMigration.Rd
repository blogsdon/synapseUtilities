\name{populateInternalDirectoryMigration}
\alias{populateInternalDirectoryMigration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  migration function to populate internal directory with necessary structure after migration
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
populateInternalDirectoryMigration(synId, G, Q = NULL, topId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synId}{
%%     ~~Describe \code{synId} here~~
}
  \item{G}{
%%     ~~Describe \code{G} here~~
}
  \item{Q}{
%%     ~~Describe \code{Q} here~~
}
  \item{topId}{
%%     ~~Describe \code{topId} here~~
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
function (synId, G, Q = NULL, topId)
{
    if (is.null(Q)) {
        Q <- list()
        Q$adj <- G$adj
        Q$adj[1:length(Q$adj)] <- 0
        Q$id <- G$id
        Q$id[1] <- topId
        G$id[1] <- topId
        rownames(G$adj) <- G$id
        colnames(G$adj) <- G$id
        rownames(Q$adj) <- G$id
        colnames(Q$adj) <- G$id
        names(G$type) <- G$id
        names(G$name) <- G$id
        Q$newid <- Q$id
        names(Q$newid) <- Q$id
        synId <- topId
    }
    e <- names(which(G$adj[synId, ] == 1))
    if (length(e) > 0) {
        for (i in 1:length(e)) {
            if (Q$adj[synId, e[i]] == 0) {
                Q$adj[synId, e[i]] <- 1
                if (G$type[e[i]] == "org.sagebionetworks.repo.model.Folder") {
                  Q$newid[e[i]] <- makeNewFolder(synId, e[i],
                    Q, G)
                  Q <- populateNewDirectory2(e[i], G, Q, topId)
                }
                else if (G$type[e[i]] == "org.sagebionetworks.repo.model.FileEntity") {
                  w1 <- which(Q$adj[, Q$newid[e[i]]] == 1)
                  print(c(G$name[e[i]], Q$newid[w1], e[i]))
                  makeLink(fileId = as.character(e[i]), parentId = as.character(Q$newid[w1]),
                    linkName = as.character(G$name[e[i]]))
                }
                else {
                  stop("Object type not recognized\n")
                }
            }
            else {
                return(Q)
            }
        }
    }
    else {
        return(Q)
    }
    return(Q)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
