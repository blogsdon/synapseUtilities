auditProject <- function(crawledProject,dictionaryId,tableUploadId,auditName,isFolder=TRUE){
  #crawledProject: list with the following elements
  #adjList -> list with children of each synapseId in crawled project
  #id -> synapse ids of crawled project
  #name -> synapse entity names of crawled project
  #type -> entity types  of crawled project
  #anno <- synapse annotations of crawled project
  #syn <- synapse meta data about each entity in crawled project

  #dictionaryId: synapse ID of the synapse Table with the dictionary
  #tableUploadId: project where the audit table will go
  #auditName: name of the audit table

  library(synapseClient)
  library(utilityFunctions)
  library(Hmisc)
  synapseLogin()

  checkValues <- function(x,dictionaryFields,dictionary,i){
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
  if(isFolder){
    entityType[1] <- 'Folder'
  }else{
    entityType[1] <- 'Project'
  }
  #entityType[1] <- 'Project'
  #entityType[2] <- 'Table'

  #define the result data frame for the audit
  annotationAuditDataFrame <- data.frame(synId=crawledProject$id,
                                         entityType=entityType,
                                         hasAnnotation=annoExist,
                                         stringsAsFactors = F)

  #get the fields for each syn ID


  fieldsAudit <- lapply(crawledProject$anno,utilityFunctions::getNames)
  dictionaryFields <- names(dictionary)

  #check if the fields are in the dictionary
  fieldAuditNew <- sapply(fieldsAudit,function(x,y){return(y%in%x)},dictionaryFields)
  fieldAuditNew <- t(fieldAuditNew)

  #define the column names for the test
  colnames(fieldAuditNew) <- paste0('has',dictionaryFields)

  #add to the result data frame for the audit
  annotationAuditDataFrame <- cbind(annotationAuditDataFrame,fieldAuditNew)

  #assayTargetValueInDictionary
  valueAuditNew <- fieldAuditNew
  colnames(valueAuditNew) <- paste0(dictionaryFields,'ValueInDictionary')

  #
  for (i in 1:ncol(valueAuditNew)){
    valueAuditNew[,i] <- sapply(crawledProject$anno,checkValues,dictionaryFields,dictionary,i)
  }

  annotationAuditDataFrame <- cbind(annotationAuditDataFrame,valueAuditNew)
  colnames(annotationAuditDataFrame)[1] <- 'synapseID'
  tcresult<-as.tableColumns(annotationAuditDataFrame)
  cols<-tcresult$tableColumns
  fileHandleId<-tcresult$fileHandleId
  #tableUploadId<-"syn2397881"
  schema<-TableSchema(name=auditName, parent=tableUploadId, columns=cols)
  table<-Table(schema, fileHandleId)
  table<-synStore(table, retrieveData=TRUE)

  #write.csv(annotationAuditDataFrame,file='annotationAuditDecember2015ver2.csv',quote=F,row.names=F)

}
