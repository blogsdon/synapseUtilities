crawlProject <- function(synId){
  library(synapseClient)
  synapseLogin()

  crawledProject <- crawlSynapseEntity(synId)
  #crawledProject <- synapseUtilities::makeHeadFolder(crawledProject,synId)
  crawledProject$syn <- lapply(crawledProject$id,synapseClient::synGet,downloadFile=FALSE)
  crawledProject$anno <- lapply(crawledProject$syn,synapseClient::synGetAnnotations)

  return(crawledProject)
}
