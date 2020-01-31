##################################################################
### module name         : xml-ddic-generator-elipss.R            #
### module in-charge    : alexandre.mairot@sciencespo.fr         #
### module title        : Creation XML from xlxs file            #
### module program      : R                                      #
### module version date : 2020-01-31                             #
##################################################################
require(xlsx)
require(XML)
#############################################################
metaFileLoad <- function(inFile){
  listLoad <- list()
  listName <- NA
  ### Description du document
  inSheet <- read.xlsx(paste(inFile), sheetName="docDscr", stringsAsFactors = FALSE, encoding = "UTF-8")
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[(!(is.na(inSheet[,1])))|(!(is.na(inSheet[,2])))|(!(is.na(inSheet[,3]))), c(1:6)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("elementLevel1","elementLevel2","elementLevel3","ordre","attribut","value")  
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "docDscr"
  }
  ### Description de l'étude
  inSheet <- read.xlsx(paste(inFile), sheetName="stdyDscr", stringsAsFactors = FALSE, encoding = "UTF-8")
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[(!(is.na(inSheet[,1])))|(!(is.na(inSheet[,2])))|(!(is.na(inSheet[,3])))|(!(is.na(inSheet[,4])))|(!(is.na(inSheet[,5])))|(!(is.na(inSheet[,6]))),c(1:9)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("elementLevel1","elementLevel2","elementLevel3", "elementLevel4","elementLevel5","elementLevel6","ordre","attribut","value")
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "stdyDscr"
  }
  ### Description du fichier de données
  inSheet <- read.xlsx(paste(inFile), sheetName="fileDscr", stringsAsFactors = FALSE, encoding = "UTF-8")
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[!(is.na(inSheet[,1])), c(1:10)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("fileName", "fileID", "fileDate", "fileLang","caseQnty", "varQnty", "fileCont", "dataMsng", "notes", "dataPath")  
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "fileDscr"
  }
  ### Liste des variables
  inSheet <- read.xlsx(paste(inFile), sheetName="variableList", stringsAsFactors = FALSE, encoding = "UTF-8")
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[!(is.na(inSheet[,1])), c(1:14)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("variableName","variableLabel","literalQuestion","interviewerInstructions","universeLabel","universeGenerationCode","notes","controlConstruct","preQuestion","postQuestion","isCharacter","isContinus","isWgt","minusInvalrng") 
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "variableList"
  }  
  ### Liste des modalités de réponses
  inSheet <- read.xlsx(paste(inFile), sheetName="itemList", stringsAsFactors = FALSE, encoding = "UTF-8")  
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[!(is.na(inSheet[,1])), c(1:4)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("variableName", "itemValue", "itemName", "itemLabel")  
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "itemList"
  }   
  ### Liste des valeurs noSurveyed
  inSheet <- read.xlsx(paste(inFile), sheetName="noSurveyed", stringsAsFactors = FALSE, encoding = "UTF-8")  
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[!(is.na(inSheet[,1])), c(1:2)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("variableName","itemValue") 
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "noSurveyed"
  }
  ### Table des groupes de variables
  inSheet <- read.xlsx(paste(inFile), sheetName="variableGroup", stringsAsFactors = FALSE, encoding = "UTF-8") 
  if (nrow(inSheet)>0){
    inSheet <- inSheet[!(is.na(inSheet[,1])),]
    i <- ncol(inSheet)
    repeat {
      if (length(inSheet[,i][!(is.na(inSheet[,i]))])==0){
        inSheet <- data.frame(inSheet[, c(1:(i - 1))], stringsAsFactors = FALSE)
        if (i>1){
          i <- i - 1
        } else {
          break
        }
      } else {
        break
      }
    }
    for (i in 1:(ncol(inSheet)-1)){
      if (i==1){
        inSheetCol <- paste("groupNameLevel",i,sep="")
      } else {
        inSheetCol <- cbind(inSheetCol, paste("groupNameLevel",i,sep=""))
      }
      
    }
    inSheetCol <- cbind("variableName",inSheetCol)
    colnames(inSheet)<-inSheetCol
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "variableGroup"
  }
  ### Correspondance des noms de variables
  inSheet <- read.xlsx(paste(inFile), sheetName="variableName", stringsAsFactors = FALSE, encoding = "UTF-8") 
  if (nrow(inSheet)>0){
    inSheet <- inSheet[!(is.na(inSheet[,2])), c(1:2)]
    colnames(inSheet) <- c("variableInitialName","variableFinalName")
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "variableName"
  }  
  ### Valeurs manquantes  
  inSheet <- read.xlsx(paste(inFile), sheetName="missingItemList", stringsAsFactors = FALSE, encoding = "UTF-8") 
  if (nrow(inSheet)>0){
    inSheet <- data.frame(inSheet[!(is.na(inSheet[,1])), c(1:7)], stringsAsFactors = FALSE)
    colnames(inSheet) <- c("missItemName", "missItemLabel", "missItemBase", "missItemDigit", "missItemLevel", "noApplicableFlag", "noSurveyedFlag")
    listLoad[[length(listLoad)+1]] <- inSheet
    listName[length(listLoad)] <- "missingItemList"
  }   
  ###
  names(listLoad)<-listName
  listLoad
}
#############################################################
metaFileControl <- function(inMeta){
  if(exists("variableList", where=inMeta)){
    ### Impossibilite de doublon
    errTable <- data.frame(unique(inMeta$variableList[which(duplicated(inMeta$variableList$variableName)),1]))
    if (nrow(errTable)>0){
      errTable[2] <- "variableList"
      errTable[3] <- "variableName en double"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables doivent avoir un libellé
    errTable <- data.frame(inMeta$variableList[is.na(inMeta$variableList$variableLabel), 1],stringsAsFactors = FALSE)
    if (nrow(errTable)>0){
      errTable[2] <- "variableList"
      errTable[3] <- "variableLabel manquant"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables filtrées doivent avec un minusInvalrng
    errTable <- data.frame(inMeta$variableList[(is.na(inMeta$variableList$minusInvalrng))&(!(is.na(inMeta$variableList$universeGenerationCode)))&(is.na(inMeta$variableList$isCharacter)),1])
    if (nrow(errTable)>0){
      errTable[2] <- "variableList"
      errTable[3] <- "minusInvalrng manquant"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
  }
  if((exists("variableList", where=inMeta))&&(exists("variableGroup", where=inMeta))){
    ### Recherche doublon dans variableGroup
    errTable <- data.frame(unique(inMeta$variableGroup[which(duplicated(inMeta$variableGroup$variableName)),1]))
    if (nrow(errTable)>0){
      errTable[2] <- "variableGroup"
      errTable[3] <- "variableName en double"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables sont dans un groupe de variables
    errTable <- data.frame(inMeta$variableGroup[is.na(inMeta$variableGroup$groupNameLevel1),1])
    if (nrow(errTable)>0){
      errTable[2] <- "variableGroup"
      errTable[3] <- "variable sans groupe"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables de variableList sont dans variableGroup
    errTable <- data.frame(setdiff(inMeta$variableList$variableName, inMeta$variableGroup$variableName))
    if (nrow(errTable)>0){
      errTable[2] <- "variableList"
      errTable[3] <- "Variable absente dans la table variableGroup"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables de variableGroup sont dans variableList
    errTable <- data.frame(setdiff(inMeta$variableGroup$variableName,inMeta$variableList$variableName))
    if (nrow(errTable)>0){
      errTable[2] <- "variableGroup"
      errTable[3] <- "Variable absente dans la table variableList"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }    
  }
  if((exists("variableList", where=inMeta))&&(exists("itemList", where=inMeta))){
    ### Toutes les variables de itemList sont dans variableList
    errTable <- data.frame(setdiff(unique(inMeta$itemList$variableName),unique(inMeta$variableList$variableName)))
    if (nrow(errTable)>0){
      errTable[2] <- "itemList"
      errTable[3] <- "Variable absente dans la table variableList"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Toutes les variables de variableList sont dant itemList
    errTable <- data.frame(setdiff(inMeta$variableList[(((is.na(inMeta$variableList$isCharacter))&(is.na(inMeta$variableList$isContinus)))|((!(is.na(inMeta$variableList$isContinus)))&(!(is.na(inMeta$variableList$minusInvalrng))))),1],unique(inMeta$itemList$variableName)))
    if (nrow(errTable)>0){
      errTable[2] <- "variableList"
      errTable[3] <- "Variable absente dans la table itemList"
      colnames(errTable) <- c("variableName","Table","Erreur")
      if (exists("errTableFull")){
        errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
      } else {
        errTableFull <- errTable [,c(2,3,1)]
      }
    }
    ### Verification des valeurs manquantes
    refTable <- data.frame(inMeta$variableListinMeta$variableList[(((is.na(inMeta$variableList$isCharacter))&(is.na(inMeta$variableList$isContinus)))|((!(is.na(inMeta$variableList$isContinus)))&(!(is.na(inMeta$variableList$minusInvalrng))))),c(1,14)])
    if (nrow(refTable)>0){
      for(i in 1:nrow(refTable)){
        if (i==1){
          testTable <- inMeta$itemList[(inMeta$itemList$VariableName=refTable$variableName[i])&(inMeta$itemList$itemValue>=refTable$minusInvalrng),c(1:4)]
        } else {
          testTable <- rbind(testTable,inMeta$itemList[(inMeta$itemList$variableName=refTable$variableName[i])&(inMeta$itemList$itemValue>=refTable$minusInvalrng),c(1:4)] )
        }
      }
      if(exists("missingItemList", where=inMeta)){
        testTable <- data.frame(setdiff(unique(testTable[3,]),inMeta$missingItemList[1,]))
        if(nrow(testTable>0)){
          for(i in 1:nrow(testTable)){
            if (i==1){
              errTable <- inMeta$itemList[(inMeta$itemList$itemName=testTable[i]),1]
            } else {
              errTable <- rbind(errTable,inMeta$itemList[(inMeta$itemList$itemName=testTable[i]),1])
            }
          }
          errTable[2] <- "itemList"
          errTable[3] <- "Modalité de réponse parmi les valeurs manquantes"
          colnames(errTable) <- c("variableName","Table","Erreur")
          if (exists("errTableFull")){
            errTableFull <- rbind(errTableFull,errTable [,c(2,3,1)])
          } else {
            errTableFull <- errTable [,c(2,3,1)]
          }
        }
      }
    }
  }
  if(exists("errTableFull")){
    inMetaNames <- names(inMeta)
    inMeta[[length(inMeta)+1]] <- errTableFull
    names(inMeta) <- c(inMetaNames,"errTable")
  }
  inMeta
}
#############################################################
ddicProcess <- function(inMeta){
  ### Identifiant variables
  if (exists("variableList", where=inMeta)&&(!(exists("errTable", where=inMeta)))){
    for (i in 1:nrow(inMeta$variableList)){
      inMeta$variableList$ddicVariable[i] <- paste0("V",i)
    }
    inMeta$variableList <- inMeta$variableList[,c(ncol(inMeta$variableList),1:(ncol(inMeta$variableList)-1))]
  }
  ### Groupe de variables
  if (exists("variableGroup",where=inMeta)&&(!(exists("errTable", where=inMeta)))){
    ### Définition des paths des variables
    inMeta$variableGroup$pathGroup <- ""
    for(i in 1:nrow(inMeta$variableGroup)){
      for (j in 2:(ncol(inMeta$variableGroup)-1)){
        if (!(is.na(inMeta$variableGroup[i,j]))){
          inMeta$variableGroup$pathGroup[i] <- paste(inMeta$variableGroup$pathGroup[i],inMeta$variableGroup[i,j],"/",sep="") 
        } else {
          inMeta$variableGroup$pathGroup[i] <- inMeta$variableGroup$pathGroup[i]
          }
      }
    }
    varGrpPath <- merge(inMeta$variableList[,c(1:2)],inMeta$variableGroup[,c(1,ncol(inMeta$variableGroup))], by="variableName")[,c(2,3)]
    inMeta$variableGroup<-inMeta$variableGroup[,c(1:(ncol(inMeta$variableGroup)-1))]
    ### Definition des paths des groupes
    grpCount <- 1
    for (i in ncol(inMeta$variableGroup):2){
      grpList <- data.frame(unique(inMeta$variableGroup[(!(is.na(inMeta$variableGroup[i]))),c(2:i)]))
      for (j in 1:nrow(grpList)){
        grpList$ddicGroup[j] <- paste0("VG",grpCount)
        grpCount <- grpCount + 1
      }
      grpList$pathGroup<-""
      if(ncol(grpList)>3){
        for (j in 1:nrow(grpList)){
          for(k in 1:(ncol(grpList)-3)){
            if (!(is.na(grpList[j,k]))){
              grpList$pathGroup[j] <- paste(grpList$pathGroup[j],grpList[j,k],"/",sep="")
            } else {
              grpList$pathGroup[j] <- grpList$pathGroup[j]
            }
          }
        }
      }
      if(ncol(grpList)>3){
        grpList <- grpList[,c((ncol(grpList)-2):ncol(grpList))]
      }
      names(grpList) <- c("groupeName","ddicGroup","pathGroup")
      if(i==ncol(inMeta$variableGroup)){
        grpGrpPath <- grpList
      } else {
        grpGrpPath <- rbind (grpGrpPath,grpList)
      }
    }
    grpGrpPath$variableIncluded<-""
    for(i in 1:nrow(grpGrpPath)){
      varIncluded <- varGrpPath[(varGrpPath$pathGroup==paste0(grpGrpPath$pathGroup[i],grpGrpPath$groupeName[i],"/")),]
      if (nrow(varIncluded)>0){
        for (j in 1:nrow(varIncluded)){
          if (j<nrow(varIncluded)){
            grpGrpPath$variableIncluded[i]<-paste0(grpGrpPath$variableIncluded[i],varIncluded$ddicVariable[j]," ")
          } else {
            grpGrpPath$variableIncluded[i]<-paste0(grpGrpPath$variableIncluded[i],varIncluded$ddicVariable[j])
          }
        }
      } else {
        grpGrpPath$variableIncluded[i] <- NA
      }
    }
    grpGrpPath$groupeIncluded<-""
    for (i in 1:nrow(grpGrpPath)){
      grpIncluded <- grpGrpPath[(grpGrpPath$pathGroup==paste0(grpGrpPath$pathGroup[i],grpGrpPath$groupeName[i],"/")),]
      if (nrow(grpIncluded)>0){
        for(j in 1:nrow(grpIncluded)){
          if(j<nrow(grpIncluded)){
            grpGrpPath$groupeIncluded[i]<-paste0(grpGrpPath$groupeIncluded[i],grpIncluded$ddicGroup[j]," ")
          } else {
            grpGrpPath$groupeIncluded[i]<-paste0(grpGrpPath$groupeIncluded[i],grpIncluded$ddicGroup[j])
          }
        }
      } else {
        grpGrpPath$groupeIncluded[i] <- NA
      }
    }
    grpGrpPath <- grpGrpPath[,c(2,1,5,4)]
  }
  if (nrow(grpGrpPath)>0){
    inMetaNames <- names(inMeta)
    inMeta[[length(inMeta)+1]] <- grpGrpPath
    names(inMeta) <- c(inMetaNames,"ddicGroup")
  }
  inMeta
}
#############################################################
joinMeta <- function(metaInfo, inVar){
  inMeta <- metaFileLoad(metaInfo[1])
  if (!(is.na(metaInfo[2]))){
    if (exists("variableList", where=inMeta)){
      inMeta$variableList$variableName <- paste0(metaInfo[2],"_",inMeta$variableList$variableName)
    }
    if (exists("itemList", where=inMeta)){
      inMeta$itemList$variableName <- paste0(metaInfo[2],"_",inMeta$itemList$variableName)
    }
    if (exists("noSurveyed", where=inMeta)){
      inMeta$noSurveyed$variableName <- paste0(metaInfo[2],"_",inMeta$noSurveyed$variableName)
    }
    if (exists("variableGroup", where=inMeta)){
      inMeta$variableGroup$variableName <- paste0(metaInfo[2],"_",inMeta$variableGroup$variableName)
    }
  }
  if (!(is.na(metaInfo[3]))){
    if (exists("variableGroup", where=inMeta)){
      inMeta$variableGroup[ncol(inMeta$variableGroup)+1]<-NA
      for(i in ncol(inMeta$variableGroup):2){
        inMeta$variableGroup[i] <- inMeta$variableGroup[i-1]
      }
      inMeta$variableGroup[2]<-metaInfo[3]
      for (i in 1:(ncol(inMeta$variableGroup)-1)){
        if (i==1){
          variableGroupCol <- paste("groupNameLevel",i,sep="")
        } else {
          variableGroupCol <- cbind(variableGroupCol, paste("groupNameLevel",i,sep=""))
        }
        
      }
      variableGroupCol <- cbind("variableName",variableGroupCol)
      colnames(inMeta$variableGroup) <- variableGroupCol
    }
  }
  if (!(is.na(metaInfo[4]))){
    if (exists("noSurveyed", where=inMeta)){
      specItem <- inMeta$noSurveyed
      specItem[ncol(specItem)+1] <- "ne"
      specItem[ncol(specItem)+1] <- "Non enquêté"
      colnames(specItem) <- c("variableName","itemValue","itemName","itemLabel")
      inMeta$itemList<- rbind(inMeta$itemList,specItem)
    }
  }
  if (exists("variableList", where=inMeta)){
    inTemp <- merge(inVar, inMeta$variableList)
    inTemp <- inTemp[order(inTemp$ordre),-2]
    inMeta$variableList <- inTemp
  }
  if (exists("itemList", where=inMeta)){
    inTemp <- merge(inVar, inMeta$itemList)
    inTemp <- inTemp[order(inTemp$ordre,inTemp$itemValue),-2]
    inMeta$itemList <- inTemp
  }
  if (exists("noSurveyed", where=inMeta)){
    inTemp <- merge(inVar, inMeta$noSurveyed)
    inTemp <- inTemp[order(inTemp$ordre),-2]
    inMeta$noSurveyed <- inTemp
  }
  if (exists("variableGroup", where=inMeta)){
    inTemp <- merge(inVar, inMeta$variableGroup)
    inTemp <- inTemp[order(inTemp$ordre),-2]
    if((nrow(unique(inTemp[ncol(inTemp)]))==1)&&(is.na(unique(inTemp[ncol(inTemp)])[1]))){
      inTemp <- inTemp[1:(ncol(inTemp)-1)]
    }
    inMeta$variableGroup <- inTemp
  }
  inMeta
}
#############################################################
joinLoad <- function(inFile){
  surveyLoad <- list()
  surveyName <- NA
  inSheet <- read.xlsx(paste(inFile), sheetName="survey", stringsAsFactors=FALSE, encoding="UTF-8")
  if (nrow(inSheet)>0){
    colnames(inSheet) <- c("element","value","x","metaPath","dataPrefix","grpPrefix","noSurveyed","surveyIncluded")
    fileSurvey <- inSheet[(!(is.na(inSheet$element))),c(1:2)]
    surveyLoad[[length(surveyLoad)+1]] <- fileSurvey
    surveyName[length(surveyLoad)] <- "fileSurvey" 
    metaSurvey <- inSheet[(!(is.na(inSheet$metaPath))),c(4:8)]
    surveyLoad[[length(surveyLoad)+1]] <- metaSurvey
    surveyName[length(surveyLoad)] <- "metaSurvey" 
    rm(inSheet)
  }
  if (!(is.na(fileSurvey[(fileSurvey$element=="dataPath"),][1,2]))){
    inData <- read.csv2(fileSurvey[(fileSurvey$element=="dataPath"),][1,2], encoding="UTF-8")
    surveyLoad[[length(surveyLoad)+1]] <- inData
    surveyName[length(surveyLoad)] <- "inData" 
    inVar <- data.frame(colnames(inData), stringsAsFactors = FALSE)
    colnames(inVar) <- c("variableName")
    inVar$variableName<-gsub("X.U.FEFF.","",inVar$variableName)
    inVar$ordre<-row(inVar)[,1] 
  }
  metaInfo <- metaSurvey[(metaSurvey$surveyIncluded==1),][1,]
  if (nrow(metaInfo)==1){
    inMeta <- joinMeta(metaInfo, inVar)
    if (exists("variableList", where=inMeta)){
      setVar <- data.frame(inMeta$variableList[,1], stringsAsFactors=FALSE)
      colnames(setVar) <- c("variableName")
      setVar <- data.frame(setdiff(inVar$variableName,setVar$variableName), stringsAsFactors=FALSE)
      colnames(setVar) <- c("variableName")
      inVar <- merge(setVar,inVar)
      inVar <- inVar[order(inVar$ordre),]
    }
  }
  metaInfo <-  metaSurvey[(is.na(metaSurvey$surveyIncluded)),] 
  for (i in 1:nrow(metaInfo)){
    if (exists("inMeta")){
      inTemp <- joinMeta(metaInfo[i,],inVar)
      if(exists("variableList", where=inTemp)){
        setVar <- data.frame(inTemp$variableList[,1], stringsAsFactors=FALSE)
        colnames(setVar) <- c("variableName")
        setVar <- data.frame(setdiff(inVar$variableName,setVar$variableName), stringsAsFactors=FALSE)
        colnames(setVar) <- c("variableName")
        inVar <- merge(setVar,inVar)
        inVar <- inVar[order(inVar$ordre),]
      }
      if(exists("variableList", where=inTemp)){
        inMeta$variableList <- rbind(inMeta$variableList,inTemp$variableList)
      }
      if(exists("itemList", where=inTemp)){
        inMeta$itemList <- rbind(inMeta$itemList,inTemp$itemList)
      }
      if(exists("noSurveyed", where=inTemp)){
        inMeta$noSurveyed <- rbind(inMeta$noSurveyed,inTemp$noSurveyed)
      }
      if (ncol(inMeta$variableGroup)<ncol(inTemp$variableGroup)){
        while (ncol(inMeta$variableGroup)<ncol(inTemp$variableGroup)){
          inMeta$variableGroup[ncol(inMeta$variableGroup)+1]<-NA
        }
        colnames(inMeta$variableGroup)<-colnames(inTemp$variableGroup)
      } else {
        if (ncol(inMeta$variableGroup)>ncol(inTemp$variableGroup)){
          while (ncol(inTemp$variableGroup)<ncol(inMeta$variableGroup)){
            inTemp$variableGroup[ncol(inTemp$variableGroup)+1]<-NA
          }
          colnames(inTemp$variableGroup)<-colnames(inMeta$variableGroup)
        }
      }
      inMeta$variableGroup <- rbind(inMeta$variableGroup,inTemp$variableGroup)
    } else {
      inMeta <- joinMeta(metaInfo[i,],inVar)
      if (exists("variableList", where=inMeta)){
        setVar <- data.frame(inMeta$variableList[,1], stringsAsFactors=FALSE)
        colnames(setVar) <- c("variableName")
        setVar <- data.frame(setdiff(inVar$variableName,setVar$variableName), stringsAsFactors=FALSE)
        colnames(setVar) <- c("variableName")
        inVar <- merge(setVar,inVar)
        inVar <- inVar[order(inVar$ordre),]
      }
    }
  }
  if(nrow(inVar)>0){
    errTable <- data.frame(inVar[1], stringsAsFactors = FALSE)
    errTable[2] <- "inMeta"
    errTable[3] <- "variableName absent"
    colnames(errTable) <- c("variableName","Table","Erreur")
    surveyLoad[[length(surveyLoad)+1]] <- errTable[,c(2,3,1)]
    surveyName[length(surveyLoad)] <- "errTable" 
  }
  ####
  inVar <- data.frame(colnames(inData), stringsAsFactors = FALSE)
  colnames(inVar) <- c("variableName")
  inVar$variableName<-gsub("X.U.FEFF.","",inVar$variableName)
  inVar$ordre<-row(inVar)[,1] 
  if(exists("variableList", where=inMeta)){
    specFile <- merge (inVar,inMeta$variableList)
    inMeta$variableList <- specFile[order(specFile$ordre),-2]
    if (exists("fileSurvey")){
      if (!(is.na(fileSurvey[(fileSurvey$element=="fileAbbr"),2]))){
        inMeta$variableList$variableLabel <- sub("#", fileSurvey[(fileSurvey$element=="fileAbbr"),2], inMeta$variableList$variableLabel)
      }
    }
  }
  if(exists("itemList",where = inMeta)){
    specFile <- merge (inVar,inMeta$itemList)
    inMeta$itemList <- specFile[order(specFile$ordre),-2]
  }
  if(exists("noSurveyed", where=inMeta)){
    specFile <- merge (inVar,inMeta$noSurveyed)
    inMeta$noSurveyed <- specFile[order(specFile$ordre),-2]
  }
  if(exists("variableGroup", where=inMeta)){
    specFile <- merge (inVar,inMeta$variableGroup)
    inMeta$variableGroup <- specFile[order(specFile$ordre),-2]
  }
  surveyLoad[[length(surveyLoad)+1]] <- inMeta
  surveyName[length(surveyLoad)] <- "inMeta"  
  ####
  names(surveyLoad) <- surveyName
  surveyLoad
}
#############################################################
ddicGenerator <- function(inSurvey, flavor){
  ### Noeud codeBook
  if (exists("inSurvey")){
    if (exists("fileSurvey", where=inSurvey)){
      ddiID <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="idDDI"),2]
      if(is.na(ddiID)){
        ddiID <- "X1.X8.X1.X2.X7.X9"
      }
      xmlLang <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="xmlLang"),2]
      if(is.na(xmlLang)){
        xmlLang <- "fr"
      }
    } 
  } else {
    ddiID <- "X1.X8.X1.X2.X7.X9"
    xmlLang <- "fr"
  }

  codeBook <-xmlNode(
      "codeBook",
      attrs = c(
        ID = ddiID,
        'xml-lang' = xmlLang,
        version = "2.5",
        'xsi:schemaLocation' = "ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
      ),
      namespaceDefinitions = c("ddi:codebook:2_5", xsi = "http://www.w3.org/2001/XMLSchema-instance")
    )
  ### Noeud docDscr
  if (exists("docDscr", where=inSurvey$inMeta)){
    xmlTable <- inSurvey$inMeta$docDscr
    tempXml <- list()
    docDscr <- xmlNode("docDscr")
    if(nrow(xmlTable)>0){
      i <- 1
      while(i<=nrow(xmlTable)){
        depthXml <- 1
        while(is.na(xmlTable[i,depthXml])){
          depthXml <- depthXml+1
        }
        nodeXml <- NULL
        ##### Création du noeud XML
        while (is.null(nodeXml)){
          nameNode <- xmlTable[i, depthXml]
          nameTest <- nameNode
          ordreNode <- xmlTable$ordre[i]
          if (is.na(xmlTable$ordre[i])){
            ordreNode<-0
          } else {
            ordreNode <- xmlTable$ordre[i]
          }
          ordreTest <- ordreNode
          j <- 1
          attrsXml <- list()
          while((nameNode==nameTest)&&(ordreNode==ordreTest)){
            if (is.na(xmlTable$attribut[i])){
              if (length(grep("\n",xmlTable$value[i]))==0){
                valueXml <- xmlTable$value[i]
              } else {
                valueXml <- xmlCDataNode(xmlTable$value[i])
              }
            } else {
              if (!(is.na(xmlTable$value[i]))){
                attrsXml[[j]] <- xmlTable$value[i]
                names(attrsXml)[[j]] <- xmlTable$attribut[i]
                j <- j + 1
              }
            }
            i <- i+1
            if (is.na(xmlTable$ordre[i])){
              ordreTest<-0
            } else {
              ordreTest <- xmlTable$ordre[i]
            }
            if (is.na(xmlTable[i, depthXml])){
              nameTest<-''
            } else {
              nameTest <- xmlTable[i, depthXml]
            }
          }
          if (!(is.null(attrsXml))||(exists("valueXml"))){
            if (exists("valueXml")){
              if (!(is.na(valueXml))){
                if (is.null(attrsXml)){
                  nodeXml <- xmlNode(nameNode, valueXml)
                } else {
                  nodeXml <- xmlNode(nameNode, valueXml, attrs=attrsXml)
                  attrsXml <- list()
                }
                rm(valueXml)
              } else {
                nodeXml <- xmlNode(nameNode, attrs=attrsXml)
                attrsXml <- list()
              }
            } else {
              nodeXml <- xmlNode(nameNode, attrs=attrsXml)
              attrsXml <- list()
            }
          } else {
            i <- i + 1
            nodeXml <- NULL
          }
        }
        #####
        while(depthXml<=length(tempXml)){
          if ((!(is.null(tempXml[[length(tempXml)]])))&&(length(tempXml)>1)){
            tempXml[[length(tempXml)-1]] <- addChildren(tempXml[[length(tempXml)-1]],tempXml[[length(tempXml)]])
          }
          if ((depthXml==1)&&(length(tempXml)==1)){
            docDscr <- addChildren(docDscr,tempXml[[1]])
          }
          tempXml[[length(tempXml)]] <- NULL
        } 
        while (length(tempXml)<depthXml){
          tempXml[[length(tempXml)+1]] <- NA
        }
        tempXml[[depthXml]] <- nodeXml
        #i <- i + 1
      }
      while(length(tempXml)>0){
        if ((!(is.null(tempXml[[length(tempXml)]])))&&(length(tempXml)>1)){
          tempXml[[length(tempXml)-1]] <- addChildren(tempXml[[length(tempXml)-1]],tempXml[[length(tempXml)]])
        }
        if (length(tempXml)==1){
          docDscr <- addChildren(docDscr,tempXml[[1]])
        }
        tempXml[[length(tempXml)]] <- NULL
      }
    }
    codeBook <- addChildren (codeBook, docDscr)
    rm(docDscr)
  }
  ### Noeud stdyDscr
  if (exists("stdyDscr", where=inSurvey$inMeta)){
    xmlTable <- inSurvey$inMeta$stdyDscr
    tempXml <- list()
    stdyDscr <- xmlNode("stdyDscr")
    if(nrow(xmlTable)>0){
      i <- 1
      while(i<=nrow(xmlTable)){
        depthXml <- 1
        while(is.na(xmlTable[i,depthXml])){
          depthXml <- depthXml+1
        }
        nodeXml <- NULL
        ##### Création du noeud XML
        while (is.null(nodeXml)){
          nameNode <- xmlTable[i, depthXml]
          nameTest <- nameNode
          ordreNode <- xmlTable$ordre[i]
          if (is.na(xmlTable$ordre[i])){
            ordreNode<-0
          } else {
            ordreNode <- xmlTable$ordre[i]
          }
          ordreTest <- ordreNode
          j <- 1
          attrsXml <- list()
          while((nameNode==nameTest)&&(ordreNode==ordreTest)){
            if (is.na(xmlTable$attribut[i])){
              if (length(grep("\n",xmlTable$value[i]))==0){
                valueXml <- xmlTable$value[i]
              } else {
                valueXml <- xmlCDataNode(xmlTable$value[i])
              }
            } else {
              if (!(is.na(xmlTable$value[i]))){
                attrsXml[[j]] <- xmlTable$value[i]
                names(attrsXml)[[j]] <- xmlTable$attribut[i]
                j <- j + 1
              }
            }
            i <- i+1
            if (is.na(xmlTable$ordre[i])){
              ordreTest<-0
            } else {
              ordreTest <- xmlTable$ordre[i]
            }
            if (is.na(xmlTable[i, depthXml])){
              nameTest<-''
            } else {
              nameTest <- xmlTable[i, depthXml]
            }
          }
          if (!(is.null(attrsXml))||(exists("valueXml"))){
            if (exists("valueXml")){
              if (!(is.na(valueXml))){
                if (is.null(attrsXml)){
                  nodeXml <- xmlNode(nameNode, valueXml)
                } else {
                  nodeXml <- xmlNode(nameNode, valueXml, attrs=attrsXml)
                  attrsXml <- list()
                }
                rm(valueXml)
              } else {
                nodeXml <- xmlNode(nameNode, attrs=attrsXml)
                attrsXml <- list()
              }
            } else {
              nodeXml <- xmlNode(nameNode, attrs=attrsXml)
              attrsXml <- list()
            }
          } else {
            i <- i + 1
            nodeXml <- NULL
          }
        }
        #####
        while(depthXml<=length(tempXml)){
          if ((!(is.null(tempXml[[length(tempXml)]])))&&(length(tempXml)>1)){
            tempXml[[length(tempXml)-1]] <- addChildren(tempXml[[length(tempXml)-1]],tempXml[[length(tempXml)]])
          }
          if ((depthXml==1)&&(length(tempXml)==1)){
            stdyDscr <- addChildren(stdyDscr,tempXml[[1]])
          }
          tempXml[[length(tempXml)]] <- NULL
        } 
        while (length(tempXml)<depthXml){
          tempXml[[length(tempXml)+1]] <- NA
        }
        tempXml[[depthXml]] <- nodeXml
        #i <- i + 1
      }
      while(length(tempXml)>0){
        if ((!(is.null(tempXml[[length(tempXml)]])))&&(length(tempXml)>1)){
          tempXml[[length(tempXml)-1]] <- addChildren(tempXml[[length(tempXml)-1]],tempXml[[length(tempXml)]])
        }
        if (length(tempXml)==1){
          stdyDscr <- addChildren(stdyDscr,tempXml[[1]])
        }
        tempXml[[length(tempXml)]] <- NULL
      }
    }
  } else {
    xml1 <- xmlNode("titl","Description de l'étude absente")
    xml2 <- xmlNode("titlStmt")
    xml2 <- addChildren(xml2,xml1)
    xml1 <- xmlNode("citation")
    xml1 <- addChildren(xml1, xml2)
    stdyDscr <- addChildren(stdyDscr,xml1)
    rm(xml1, xml2)
  }
  codeBook <- addChildren (codeBook, stdyDscr)
  rm(stdyDscr)
  #### Noeud fileDscr
  if (exists("inSurvey")){
    if (exists("fileSurvey", where=inSurvey)){
      attrsXml <- list()
      j <- 1
      if (is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileID"),2])){
        attrsXml[[j]] <- "F1"
      } else {
        attrsXml[[j]] <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileID"),2]
      }
      names(attrsXml)[[j]] <- "ID"
      j <- j + 1
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileDate"),2]))){
        attrsXml[[j]] <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileDate"),2]
        names(attrsXml)[[j]] <- "elementVersionDate"
        j <- j + 1
      }
      fileDscr <- xmlNode("fileDscr", attrs=attrsXml)
      rm(j, attrsXml)
      attrsXml <- list()
      j <- 1
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]))){
        attrsXml[[j]] <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]
        names(attrsXml)[[j]] <- "xml-lang"
        j <- j + 1
        fileTxt <- xmlNode("fileTxt", attrs=attrsXml)
      }
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileCont"),2]))){
        if(length(grep("\n",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileCont"),2]))==0){
          fileCont <- xmlNode("fileCont",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileCont"),2])
        } else {
          fileCont <- xmlNode("fileCont",xmlCDataNode(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileCont"),2]))
        }
        if (!(exists("fileTxt"))){
          fileTxt <- xmlNode("fileTxt")
        }
        fileTxt <- addChildren(fileTxt, fileCont)
        rm(fileCont)
      }
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="caseQnty"),2]))){
        dimensns <- xmlNode("dimensns")
        caseQnty <- xmlNode("caseQnty", as.character(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="caseQnty"),2]))
        dimensns <- addChildren(dimensns,caseQnty)
        rm(caseQnty)
      }
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="varQnty"),2]))){
        if (!(exists("dimensns"))){
          dimensns <- xmlNode("dimensns")
        }
        varQnty <- xmlNode("varQnty", as.character(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="varQnty"),2]))
        dimensns <- addChildren(dimensns,varQnty)
        rm(varQnty)
      }
      if (exists("dimensns")){
        if (!(exists("fileTxt"))){
          fileTxt <- xmlNode("fileTxt")
        }
        fileTxt <- addChildren(fileTxt,dimensns)
        rm(dimensns)
      }
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="dataMsng"),2]))){
        if(length(grep("\n",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="dataMsng"),2]))==0){
          dataMsng <- xmlNode("dataMsng",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="dataMsng"),2])
        } else {
          fileCont <- xmlNode("dataMsng",xmlCDataNode(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="dataMsng"),2]))
        }
        if (!(exists("fileTxt"))){
          fileTxt <- xmlNode("fileTxt")
        }
        fileTxt <- addChildren(fileTxt, dataMsng)
        rm(dataMsng)
      }
      if (exists("fileTxt")){
        fileDscr <- addChildren(fileDscr,fileTxt)
        rm(fileTxt)
      }
      if (!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="notes"),2]))){
        if(length(grep("\n",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="notes"),2]))==0){
          notes <- xmlNode("notes",inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="notes"),2])
        } else {
          notes <- xmlNode("notes",xmlCDataNode(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="notes"),2]))
        }
        fileDscr <- addChildren(fileDscr,notes)
        rm(notes)
      }
    } else {
      fileDscr <- xmlNode("fileDscr", attrs=c(ID="F1"))
    }
  } else {
    fileDscr <- xmlNode("fileDscr", attrs=c(ID="F1"))
  }
  codeBook <- addChildren(codeBook, fileDscr)
  rm(fileDscr)
  #### Noeud dataDscr
  #### Noeud "varGrp"
  if (exists("ddicGroup",where=inSurvey$inMeta)){
    dataDscr <- xmlNode("dataDscr")
    for (i in 1:nrow(inSurvey$inMeta$ddicGroup)){
      attrsXml <- list()
      attrsXml[[1]]<-inSurvey$inMeta$ddicGroup$ddicGroup[i]
      names(attrsXml)[[1]] <- "ID"
      j<-2
      if(!(is.na(inSurvey$inMeta$ddicGroup$variableIncluded[i]))){
        attrsXml[[j]]<-inSurvey$inMeta$ddicGroup$variableIncluded[i]
        names(attrsXml)[[j]] <- "var"
        j<-j+1
      }
      if(!(is.na(inSurvey$inMeta$ddicGroup$groupeIncluded[i]))){
        attrsXml[[j]]<-inSurvey$inMeta$ddicGroup$groupeIncluded[i]
        names(attrsXml)[[j]] <- "varGrp"
        j<-j+1
      }
      varGrp <- xmlNode("varGrp", attrs=attrsXml)
      rm(attrsXml,j)
      if (!(is.na(inSurvey$inMeta$ddicGroup$groupeName[i]))){
        if(length(grep("\n",inSurvey$inMeta$ddicGroup$groupeName[i]))==0){
          labl <- xmlNode("labl",inSurvey$inMeta$ddicGroup$groupeName[i])
        } else {
          labl <- xmlNode("labl",xmlCDataNode(inSurvey$inMeta$ddicGroup$groupeName[i]))
        }
        varGrp <- addChildren(varGrp,labl)
        rm(labl)
      }
      dataDscr <- addChildren(dataDscr,varGrp)
      rm(varGrp)
    }
  }
  #### Noeud var
  if (exists("variableList",where=inSurvey$inMeta)){
    if(!(exists("dataDscr"))){
      dataDscr <- xmlNode("dataDscr")
    }
    ### Tableau des fréquences
    if(exists("inData", where=inSurvey)){
      for (i in 1:ncol(inSurvey$inData)){
        if (i==1){
          freqData <- data.frame(names(inSurvey$inData[i]),table(inSurvey$inData[i]), stringsAsFactors=FALSE)
        } else {
          freqData <- rbind(freqData, data.frame(names(inSurvey$inData[i]),table(inSurvey$inData[i]), stringsAsFactors=FALSE))
        }
      }
      colnames(freqData)<-c("var","value","freq")
      freqData$value <- as.numeric(freqData$value)
      freqData$var <- gsub("X.U.FEFF.","",freqData$var) 
      if (exists("freqData", where=inSurvey)){
        inSurvey$freqData <- freqData
      } else {
        inSurvey[[length(inSurvey)+1]] <- freqData
        names(inSurvey)[[length(inSurvey)]] <- "freqData"
      }
    }
    ### Création du noeud xml
    for (i in 1:nrow(inSurvey$inMeta$variableList)){
      attrsXml <- list()
      if(!(is.na(inSurvey$inMeta$variableList$ddicVariable[i]))){
        attrsXml[[1]] <- inSurvey$inMeta$variableList$ddicVariable[i]
        names(attrsXml)[[1]] <- "ID"
      }
      j <- 2
      if(!(is.na(inSurvey$inMeta$variableList$variableName[i]))){
        attrsXml[[j]] <- inSurvey$inMeta$variableList$variableName[i]
        names(attrsXml)[[j]] <- "name"
        j <- j + 1
      }
      if(!(is.na(inSurvey$inMeta$variableList$isWgt[i]))){
        attrsXml[[j]] <- "wgt"
        names(attrsXml)[[j]] <- "wgt"
        j <- j + 1
      } else {
        attrsXml[[j]] <- "not-wgt"
        names(attrsXml)[[j]] <- "wgt"
        j <-  j + 1
      }
      if(!(is.na(inSurvey$inMeta$variableList$isContinus[i]))){
        attrsXml[[j]] <- "contin"
        names(attrsXml)[[j]] <- "intrvl"
        j <- j + 1
      } else {
        if ((is.na(inSurvey$inMeta$variableList$isCharacter[i]))){
          attrsXml[[j]] <- "discrete"
          names(attrsXml)[[j]] <- "intrvl"
          j <-  j + 1
        }
      }
      var <- xmlNode("var", attrs=attrsXml)
      rm(j, attrsXml)
      if(!(is.na(inSurvey$inMeta$variableList$variableLabel[i]))){
        labl <- xmlNode("labl",inSurvey$inMeta$variableList$variableLabel[i])
        var <- addChildren(var, labl)
      }
      if(!(is.na(inSurvey$inMeta$variableList$preQuestion[i]))){
        if (!(exists("qstn"))){
          qstn <- xmlNode("qstn")
        }
        preQTxt <- xmlNode("preQTxt",inSurvey$inMeta$variableList$preQuestion[i])
        qstn <- addChildren(qstn,preQTxt)
        rm(preQTxt)
      }
      if(!(is.na(inSurvey$inMeta$variableList$literalQuestion[i]))){
        if(!(exists("qstn"))){
          qstn <- xmlNode("qstn")
        }
        qstnLit <- xmlNode("qstnLit",inSurvey$inMeta$variableList$literalQuestion[i])
        qstn <- addChildren(qstn,qstnLit)
        rm(qstnLit)
      }
      if(!(is.na(inSurvey$inMeta$variableList$postQuestion[i]))){
        if(!(exists("qstn"))){
          qstn <- xmlNode("qstn")
        }
        postQTxt <- xmlNode("postQTxt",inSurvey$inMeta$variableList$postQuestion[i])
        qstn <- addChildren(qstn,postQTxt)
        rm(postQTxt)
      }
      if(!(is.na(inSurvey$inMeta$variableList$interviewerInstructions[i]))){
        if(!(exists("qstn"))){
          qstn <- xmlNode("qstn")
        }
        ivuInstr <- xmlNode("ivuInstr",inSurvey$inMeta$variableList$interviewerInstructions[i])
        qstn <- addChildren(qstn,ivuInstr)
        rm(ivuInstr)
      }
      if (exists("qstn")){
        var <- addChildren(var, qstn)
        rm(qstn)
      }
      if(!(is.na(inSurvey$inMeta$variableList$minusInvalrng[i]))){
        valrng <- xmlNode("valrng")
        range <- xmlNode("range", attrs=c(UNITS="INT", maxExclusive=inSurvey$inMeta$variableList$minusInvalrng[i]))
        valrng <- addChildren(valrng, range)
        rm(range)
        var <- addChildren(var,valrng)
        rm(valrng)
        invalrng <- xmlNode("invalrng")
        range <- xmlNode("range", attrs=c(UNITS="INT", min=inSurvey$inMeta$variableList$minusInvalrng[i]))
        invalrng <- addChildren(invalrng,range)
        rm(range)
        var <- addChildren(var,invalrng)
        rm(invalrng)
      }
      if (!(is.na(inSurvey$inMeta$variableList$universeGenerationCode[i]))){
        if(exists("fileSurvey", where=inSurvey)){
          if(!(is.na(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]))){
            if(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]=="eng"){
              universe <- xmlNode("universe", gsub("  "," ",paste(inSurvey$inMeta$variableList$universeLabel[i]," (meaning ",inSurvey$inMeta$variableList$universeGenerationCode[i],")",sep = "")))
            } else {
              ### Langue par defaut
              universe <- xmlNode("universe", gsub("  "," ",paste(inSurvey$inMeta$variableList$universeLabel[i]," (c'est-à-dire ",inSurvey$inMeta$variableList$universeGenerationCode[i],")",sep = "")))
            }
          } else {
            universe <- xmlNode("universe", gsub("  "," ",paste(inSurvey$inMeta$variableList$universeLabel[i]," (c'est-à-dire ",inSurvey$inMeta$variableList$universeGenerationCode[i],")",sep = "")))
          }
        } else {
          universe <- xmlNode("universe", gsub("  "," ",paste(inSurvey$inMeta$variableList$universeLabel[i]," (c'est-à-dire ",inSurvey$inMeta$variableList$universeGenerationCode[i],")",sep = "")))
        }
        var <- addChildren(var,universe)
        rm(universe)
      }
      if((is.na(inSurvey$inMeta$variableList$isCharacter[i]))&&(is.na(inSurvey$inMeta$variableList$isContinus[i]))){
        tblItem <- inSurvey$inMeta$itemList[(inSurvey$inMeta$itemList$variableName==inSurvey$inMeta$variableList$variableName[i]),c(1:4)]
        if (nrow(tblItem)>0){
          for (j in 1:nrow(tblItem)){
            if(is.na(inSurvey$inMeta$variableList$minusInvalrng[i])){
              catgry <- xmlNode("catgry")
            } else {
              if(tblItem$itemValue[j]<inSurvey$inMeta$variableList$minusInvalrng[i]){
                catgry <- xmlNode("catgry")
              } else {
                catgry <- xmlNode("catgry", attrs = c(missing = "Y"))
              }
            }
            catValu <- xmlNode("catValu", tblItem$itemValue[j], attrs=c(ID=tblItem$itemName[j]))
            catgry <- addChildren(catgry, catValu)
            rm(catValu)
            catgry <- addChildren(catgry, xmlNode("labl",tblItem$itemLabel[j]))
            if (exists("freqData", where=inSurvey)){
              catStatVal <- inSurvey$freqData[(inSurvey$freqData$var==inSurvey$inMeta$variableList$variableName[i])&(inSurvey$freqData$value==tblItem$itemValue[j]),3]
              if (length(catStatVal)==0){
                catStatVal <- NA
              }
              if(!(is.na(catStatVal))){
                catStat <- xmlNode("catStat", catStatVal,attrs=c(type="freq"))
                catgry <- addChildren(catgry,catStat)
                rm(catStat)
              }
              rm(catStatVal)
            }
            if (exists("var")){
              var <- addChildren(var,catgry)
              rm(catgry)
            } 
          }
        }
      }
      if (flavor!="nesstar"){
        if (!(is.na(inSurvey$inMeta$variableList$controlConstruct[i]))){
          derivation <- xmlNode("derivation")
          if (length(grep("\n",inSurvey$inMeta$variableList$controlConstruct[i]))==0){
            drvdesc <- xmlNode("drvdesc",inSurvey$inMeta$variableList$controlConstruct[i])
          } else {
            drvdesc <- xmlNode("drvdesc",xmlCDataNode(inSurvey$inMeta$variableList$controlConstruct[i]))
          }
          derivation <- addChildren(derivation,drvdesc)
          rm(drvdesc)
          var <- addChildren(var,derivation)
          rm(derivation)
        }
      }
      if (!(is.na(inSurvey$inMeta$variableList$isCharacter[i]))){
        varFormat <- xmlNode("varFormat",attrs=c(type="character", schema="other"))
      } else {
        varFormat <- xmlNode("varFormat",attrs=c(type="numeric", schema="other"))
      }
      var <- addChildren(var, varFormat)
      rm(varFormat)
      if (flavor!="nesstar"){
        if(!(is.na(inSurvey$inMeta$variableList$notes[i]))){
          if (length(grep("\n",inSurvey$inMeta$variableList$notes[i]))==0){
            notes <- xmlNode("notes",inSurvey$inMeta$variableList$notes[i])
          } else {
            notes <- xmlNode("notes",xmlCDataNode(inSurvey$inMeta$variableList$notes[i]))
          }
          var <- addChildren(var,notes)
          rm(notes)
        }
      } else {
        if(!(is.na(inSurvey$inMeta$variableList$notes[i]))){
          if (is.na(inSurvey$inMeta$variableList$controlConstruct[i])){
            if (length(grep("\n",inSurvey$inMeta$variableList$notes[i]))==0){
              notes <- xmlNode("notes", as.character(gsub("  "," ",inSurvey$inMeta$variableList$notes[i])))
            } else {
              notes <- xmlNode("notes", xmlCDataNode(as.character(gsub("  "," ",inSurvey$inMeta$variableList$notes[i]))))
            }
          } else {
            if(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]=="eng"){
              notes <- xmlNode("notes", xmlCDataNode(paste0(as.character(gsub("  "," ",inSurvey$inMeta$variableList$notes[i])),"\nFormula :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep="")))
            } else {
              ### Langue par defaut
              notes <- xmlNode("notes", xmlCDataNode(paste0(as.character(gsub("  "," ",inSurvey$inMeta$variableList$notes[i])),"\nFormule :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep="")))
            }
          }
        } else {
          if(!(is.na(inSurvey$inMeta$variableList$controlConstruct[i]))){
            if (length(grep("\n",inSurvey$inMeta$variableList$controlConstruct[i]))==0){
              if(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]=="eng"){
                notes <- xmlNode("notes", paste0("Formula :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep=""))
              } else {
                ### Langue par defaut
                notes <- xmlNode("notes", paste0("Formule :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep=""))
              }
            } else {
              if(inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="fileLang"),2]=="eng"){
                notes <- xmlNode("notes", xmlCDataNode(paste0("Formula :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep="")))
              } else {
                ### Langue par defaut
                notes <- xmlNode("notes", xmlCDataNode(paste0("Formule :\n",as.character(gsub("#","   ",inSurvey$inMeta$variableList$controlConstruct[i])),sep="")))
              }              
            }
          }
        }
        if (exists("notes")){
          var <- addChildren(var,notes)
          rm(notes) 
        }
      }
      dataDscr <- addChildren(dataDscr,var)
      rm(var)
    }
  }
  if(exists("dataDscr")){
    codeBook <- addChildren(codeBook,dataDscr)
    rm(dataDscr)
  }
  codeBook
}
#############################################################
ddicSave <- function(inSurvey, ddicObject){
  if(exists("fileSurvey", where=inSurvey)){
    outFile <- inSurvey$fileSurvey[(inSurvey$fileSurvey$element=="xmlPath"),2]
    if (!(is.na(outFile))){
      if (file.exists(outFile)){
        i <- 1
        while(file.exists(paste0(gsub(".xml","",outFile),"_",i,".xml"))){
          i <- i + 1
        }
        nameFile <- paste0(gsub(".xml","",outFile),"_",i,".xml")
      } else {
        nameFile <- outFile
      }
      saveXML(ddicObject,nameFile,indent=TRUE,prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')        
    }
  }
  "done"
}
#############################################################
