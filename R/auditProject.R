auditProject <- function(crawledProject,dictionaryId,tableUploadId,auditName){
  #crawledProject: list with the following elements
  #adjList -> list with children of each synapseId
  #id -> synapse ids
  #name -> synapse entity names
  #type -> entity types
  #anno <- synapse annotations
  #syn <- synapse meta data about each entity

  library(synapseClient)
  library(utilityFunctions)
  library(Hmisc)
  synapseLogin()

  fxn3 <- function(x,dictionaryFields,dictionary,i){
    foobaz <- FALSE
    try(foobaz <- x[[dictionaryFields[i]]] %in% dictionary[[dictionaryFields[i]]],silent=T)
    if(length(foobaz)>0){
      if(is.na(foobaz)){
        foobaz <- FALSE
      }
    }else {
      foobaz <- FALSE
    }
    return(foobaz[1])
  }

  #grab current version of dictionary from synapse table
  dictionaryObj <- synTableQuery(paste0('SELECT * FROM ',dictionaryId))

  #turn dictionary into list
  uniqueFields <- unique(dictionaryObj@values$field)
  dictionary <- lapply(uniqueFields,listify,dictionaryObj@values$value,dictionaryObj@values$field)
  names(dictionary) <- uniqueFields

  #check which entities have annotations
  annoExist <- sapply(crawledProject$anno,function(x){return(length(x)>0)})

  #get the entity types
  entityType <- sapply(crawledProject$type,splitPeriod,n=5)
  entityType[1] <- 'Project'
  entityType[2] <- 'Table'

  annotationAuditDataFrame <- data.frame(synId=crawledProject$id,
                                         entityType=entityType,
                                         hasAnnotation=annoExist,
                                         stringsAsFactors = F)

  fieldsAudit <- lapply(crawledProject$anno,getNames)
  dictionaryFields <- names(dictionary)

  fieldAuditNew <- sapply(fieldsAudit,function(x,y){return(y%in%x)},dictionaryFields)
  fieldAuditNew <- t(fieldAuditNew)
  colnames(fieldAuditNew) <- paste0('has',dictionaryFields)

  annotationAuditDataFrame <- cbind(annotationAuditDataFrame,fieldAuditNew)

  #assayTargetValueInDictionary
  valueAuditNew <- fieldAuditNew
  colnames(valueAuditNew) <- paste0(dictionaryFields,'ValueInDictionary')


  for (i in 1:ncol(valueAuditNew)){
    valueAuditNew[,i] <- sapply(crawledProject$anno,fxn3,dictionaryFields,dictionary,i)
  }

  annotationAuditDataFrame <- cbind(annotationAuditDataFrame,valueAuditNew)
  colnames(annotationAuditDataFrame)[1] <- 'synapseID'
  tcresult<-as.tableColumns(annotationAuditDataFrame)
  cols<-tcresult$tableColumns
  fileHandleId<-tcresult$fileHandleId
  projectId<-"syn2397881"
  schema<-TableSchema(name="AMP AD Audit Full Table Ver 2", parent=projectId, columns=cols)
  table<-Table(schema, fileHandleId)
  table<-synStore(table, retrieveData=TRUE)

  write.csv(annotationAuditDataFrame,file='annotationAuditDecember2015ver2.csv',quote=F,row.names=F)
  require(dplyr)
  annotationAuditFolderSummary <- dplyr::filter(annotationAuditDataFrame,entityType=='Folder') %>%
    dplyr::select(synapseID)

  annotationAuditFolderSummary$meanChildFilehasAnnotation <- rep(NA,nrow(annotationAuditFolderSummary))
  annotationAuditFolderSummary$totalChildFilehasAnnotation <- rep(NA,nrow(annotationAuditFolderSummary))
  annotationAuditFolderSummary$meanhasMinimumNecessaryAnnotations <- rep(NA,nrow(annotationAuditFolderSummary))
  annotationAuditFolderSummary$totalhasMinimumNecessaryAnnotations <- rep(NA,nrow(annotationAuditFolderSummary))
  annotationAuditFolderSummary$meanDictionaryErrors <- rep(NA,nrow(annotationAuditFolderSummary))
  annotationAuditFolderSummary$totalDictionaryErrors <- rep(NA,nrow(annotationAuditFolderSummary))

  #minimum necessary
  #Consortium
  #Center
  #Study
  #Disease
  #Assay
  #File Type
  #Model System
  #Tissue Type
  #Organism


  rownames(annotationAuditDataFrame) <- annotationAuditDataFrame$synapseID
  for (i in 1:nrow(annotationAuditFolderSummary)){
    children <- crawledProject$adjList[[annotationAuditFolderSummary$synapseID[i]]]
    if(length(children)>0){
      entityTypes <- crawledProject$type[crawledProject$id%in%children]
      w1 <- which(entityTypes=='org.sagebionetworks.repo.model.FileEntity')
      if(length(w1)>0){
        #print(children[w1])
        #print(annotationAuditDataFrame[children[w1],3])
        annotationAuditFolderSummary$meanChildFilehasAnnotation[i] <- mean(annotationAuditDataFrame[children[w1],3])
        annotationAuditFolderSummary$totalChildFilehasAnnotation[i] <- sum(annotationAuditDataFrame[children[w1],3])
        annotationAuditFolderSummary$meanhasMinimumNecessaryAnnotations[i] <- annotationAuditDataFrame[children[w1],] %>% dplyr::select(hasconsortium,hascenter,hasstudy,hasdisease,hasassay,hasfileType,hasmodelSystem,hastissueType,hasorganism) %>%as.matrix %>% mean(na.rm=T)

        #annotationAuditDataFrame[children[w1],] %>% dplyr::select(hasconsortium,hascenter,hasstudy,hasdisease,hasassay,hasfileType,hasmodelSystem,hastissueType,hasorganism) %>% print
        annotationAuditFolderSummary$totalhasMinimumNecessaryAnnotations[i] <- annotationAuditDataFrame[children[w1],] %>% dplyr::select(hasconsortium,hascenter,hasstudy,hasdisease,hasassay,hasfileType,hasmodelSystem,hastissueType,hasorganism) %>%as.matrix %>% sum(na.rm=T)
        annotationAuditFolderSummary$meanDictionaryErrors[i] <- (!annotationAuditDataFrame[children[w1],25:43]) %>%as.matrix %>% mean(na.rm=T)
        annotationAuditFolderSummary$totalDictionaryErrors[i] <- (!annotationAuditDataFrame[children[w1],25:43]) %>%as.matrix %>% sum(na.rm=T)
      }
    }
  }

  annotationAuditFolderSummary <- dplyr::filter(annotationAuditFolderSummary,!is.na(meanChildFilehasAnnotation))


  tcresult<-as.tableColumns(annotationAuditFolderSummary)
  cols<-tcresult$tableColumns
  fileHandleId<-tcresult$fileHandleId
  projectId<-"syn2397881"
  schema<-TableSchema(name="AMP AD Audit Folder Summaries Ver 2", parent=projectId, columns=cols)
  table<-Table(schema, fileHandleId)
  table<-synStore(table, retrieveData=TRUE)
}
