#!/usr/bin/env Rscript

getWallpaper <- function() {
    library(XML)
    library(httr)
    library(jpeg)
    library(png)
    
    setwd("C:/PATH/TO/WALLPAPER/PROGRAM/LOCATION")
    
    tagDataFile <- "PATH/TO/DATA/FILES/tagLikes.rds"
    
    if(file.exists(tagDataFile)) {
        tagDataF <- readRDS(tagDataFile)
    } else {
        tagDataF <- data.frame(tagID = character(), tagNames = character(), tagLikes = integer(), stringsAsFactors = FALSE)
    }
    
    handle <- handle("http://alpha.wallhaven.cc")
    path   <- "auth/login"
    login <- list(username = "yourUserName", password = "yourPassword")
    
    response <- POST(handle = handle, path = path, body = login)
    
    cookiesResponse <- cookies(response)
    
    controlThumbs <- 1
    controlPaper <- -1
    
    while(controlThumbs == 1 && controlPaper < 0) {
        docRaw <- GET(url = "http://alpha.wallhaven.cc/search?categories=111&purity=111&sorting=random&order=desc", 
                      set_cookies(a = cookiesResponse[1, 7], b = cookiesResponse[2, 7], cookiesResponse[3, 7]), 
                      handle = handle)
        
        doc <- htmlTreeParse(docRaw, useInternalNodes = T)
        
        paperNodes <- getNodeSet(doc, "//figure[@class]")
        links <- sapply(paperNodes, xmlGetAttr, "data-wallpaper-id")
        
        while(controlPaper < 0) {
            wallGallUrl <- paste0("http://alpha.wallhaven.cc/wallpaper/", links[controlThumbs])
            
            xxxGalResponse <- GET(url = wallGallUrl, 
                                  set_cookies(a = cookiesResponse[1, 7], b = cookiesResponse[2, 7], cookiesResponse[3, 7]), 
                                  handle = handle)
            
            docGall <- htmlTreeParse(xxxGalResponse, useInternalNodes = T)
            
            tagNodes <- getNodeSet(docGall, "//li[@data-tag-id]")
            tagNames <- sapply(tagNodes, xmlGetAttr, "data-tag-id")
            tagRealNames <- sapply(tagNodes, xmlValue, "//li[@data-tag-id]")
            
            controlPaper <- sum(tagDataF[tagDataF$tagID %in% tagNames, 3])
            
            if(controlPaper < 0) {
                
                write.table(links[controlThumbs], "PATH/TO/DATA/FILES/rejects.txt", append = TRUE, row.names = FALSE, col.names = FALSE)
                
                if(controlThumbs == 64) {
                    controlThumbs <- 1
                    break
                } else {
                    controlThumbs <- controlThumbs + 1
                }
            }
        }
    }
        
    galNodes <- getNodeSet(docGall, "//img[@id]")
    linkName <- sapply(galNodes, xmlGetAttr, "src")
    linkUrl <- paste0("http:", linkName)
    
    wallNumList <- strsplit(linkName, "-")
    wallNum <- wallNumList[[1]][[2]]
    picNumList <- unlist(strsplit(wallNum, "[.]"))
    
    destName <- paste0("PATH/TO/WALLPAPERS/", wallNum)
    
    if(!file.exists(destName)) {
        download.file(linkUrl, destfile = destName, mode = "wb")
    }
    
    currentName <- paste("PATH/TO/CURRENT/WALLPAPER/current", picNumList[2], sep = ".")
    
    file.copy(destName, currentName, overwrite = T)
    
    if(picNumList[2] == "jpg") {
        resize(currentName)
        
        shell(shQuote(normalizePath("changeWallPaperJPG.vbs")), "cscript", flag = "//nologo")
    } else if(picNumList[2] == "png") {
        resizePNG(currentName)
        
        shell(shQuote(normalizePath("changeWallPaperJPG.vbs")), "cscript", flag = "//nologo")
    } else if(picNumList[2] == "gif") {
        shell(shQuote(normalizePath("changeWallPaperGIF.vbs")), "cscript", flag = "//nologo")
    } else if(picNumList[2] == "bmp") {
        shell(shQuote(normalizePath("changeWallPaperBMP.vbs")), "cscript", flag = "//nologo")
    } else {
        write(picNumList[2], "newFileType.txt")
    }
    
    like <- youLike()
    
    tagNameDF <- data.frame(tagID = tagNames, tagNames = tagRealNames, stringsAsFactors = FALSE)
    tagNameDF <- unique(tagNameDF)
    learningIsFun(like, tagNameDF)
}

resize <- function(currentName = "PATH/TO/CURRENT/WALLPAPER/current.jpg") {
    library(abind)
    library(jpeg)

    img <- readJPEG(currentName)
    
    rowDim <- nrow(img)
    colDim <- ncol(img)
    chanDim <- dim(img)[3]
    
    picAR <- colDim / rowDim
    
    aspectRatio <- 1920 / 1080
    
    if(picAR == aspectRatio) return()
    
    if(picAR > aspectRatio) {
        missingRows <- (colDim / aspectRatio) - rowDim
        rowsSplit <- missingRows %/% 2
        
        arNew <- array(0, c(rowsSplit, colDim, chanDim))
        
        imgNew <- abind(arNew, img, arNew, along = 1)
        
        writeJPEG(imgNew, currentName, quality = 1)
    } else if (picAR < aspectRatio) {
        missingCols <- (rowDim * aspectRatio) - colDim
        colsSplit <- missingCols %/% 2
        
        arNew <- array(0, c(rowDim, colsSplit, chanDim))
        
        imgNew <- abind(arNew, img, arNew, along = 2)
        
        writeJPEG(imgNew, currentName, quality = 1)
    }
}

resizePNG <- function(currentName = "PATH/TO/CURRENT/WALLPAPER/current.png") {
    library(abind)
    library(jpeg)
    library(png)
    
    newName <- "PATH/TO/CURRENT/WALLPAPER/current.jpg"
    
    img <- readPNG(currentName)
    
    rowDim <- nrow(img)
    colDim <- ncol(img)
    chanDim <- dim(img)[3]
    
    picAR <- colDim / rowDim
    
    aspectRatio <- 1920 / 1080
    
    if(picAR == aspectRatio) {
        writeJPEG(img, newName, quality = 1)
        
        return()
    }
    
    if(picAR > aspectRatio) {
        missingRows <- (colDim / aspectRatio) - rowDim
        rowsSplit <- missingRows %/% 2
        
        arNew <- array(0, c(rowsSplit, colDim, chanDim))
        
        imgNew <- abind(arNew, img, arNew, along = 1)
        
        writeJPEG(imgNew, newName, quality = 1)
    } else if (picAR < aspectRatio) {
        missingCols <- (rowDim * aspectRatio) - colDim
        colsSplit <- missingCols %/% 2
        
        arNew <- array(0, c(rowDim, colsSplit, chanDim))
        
        imgNew <- abind(arNew, img, arNew, along = 2)
        
        writeJPEG(imgNew, newName, quality = 1)
    }
}

youLike <- function() {
    
    library(tcltk)
    
    choices <- data.frame(feedback = c("yes", "no", "cancel"), numEquiv = c(1, -1, 0), stringsAsFactors = FALSE)
    
    valTCL <- tkmessageBox(title = "Rank the Wallpaper", message = "Right click the desktop, go to personalize, and click on Desktop Background to change the background. You like?", icon = "question", type = "yesnocancel", default = "yes")
    val <- tclvalue(valTCL)
    
    val <- choices[choices[1] == val, 2]
    return(val)
    
#    library(gWidgets)
#    options(guiToolkit="tcltk")
    
#    val <- gconfirm("Right click the desktop, go to personalize, and click on Desktop Background to change the background. You like?", title = "Rank the Wallpaper", icon = "question")
#    return(as.numeric(val))
#    choices <- data.frame(feedback = c("YES", "NO", "CANCEL"), numEquiv = c(1, -1, 0), stringsAsFactors = FALSE)
    
#    val <- winDialog(type = "yesnocancel", "Right click the desktop, go to personalize, and click on Desktop Background to change the background. You like?")
#    val <- choices[choices[1] == val, 2]
#    return(val)
    
#    library(tcltk)
    
#    tt <- tktoplevel()
    
#    tktitle(tt) <- "Rank the Wallpaper"
    
#    tkgrid(tklabel(tt, text = "Right click the desktop, go to personalize, and click on Desktop Background to change the background. You like?"))
    
#    done <- tclVar(2)
    
#    yes.but <- tkbutton(tt, text = "Yes", command = function() tclvalue(done) <- 1)
#    no.but <- tkbutton(tt, text = "No", command = function() tclvalue(done) <- -1)
#    other.but <- tkbutton(tt, text = "Ask Again Later", command = function() tclvalue(done) <- 0)
    
#    tkgrid(yes.but, no.but, other.but)
    
#    tkbind(tt, "<Destroy>", function() tclvalue(done) <- 0)
    
#    tkfocus(tt)
#    tkwait.variable(done)
    
#    doneVal <- as.integer(tclvalue(done))
    
#    tkdestroy(tt)
    
#    return(doneVal)
}

learningIsFun <- function(liked, tagNameDF) {
    destFile <- "PATH/TO/DATA/FILES/tagLikes.rds"
    
#    if(liked == 0) liked <- -1
    
    if(!file.exists(destFile)) {
        tagDF <- data.frame(tagNameDF, tagLikes = rep(liked, length(tagNameDF$tagID)), stringsAsFactors = FALSE)
        
        saveRDS(tagDF, destFile)
        return()
    }
    
    tagDF <- readRDS(destFile)
    
    for(i in tagNameDF$tagID) {
        tagDF[i == tagDF$tagID, 3] <- tagDF[i == tagDF$tagID, 3] + liked
        
        if(!sum(i == tagDF$tagID)) tagDF <- rbind(tagDF, data.frame(tagID = i, tagNames = tagNameDF[tagNameDF$tagID == i, 2], tagLikes = liked, stringsAsFactors = FALSE))
    }
    
    saveRDS(tagDF, destFile)
}

getWallpaper()