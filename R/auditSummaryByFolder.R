auditSummaryByFolder <- function(){


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
