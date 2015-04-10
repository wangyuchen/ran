
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output, session) {
  
#   output$text <- renderText({
#     if(input$submit == 0) return()
#     
#     res <- DHR(isolate(c(input$type, input$gender, input$age, input$site)), 
#                enroll, cov.type, max.imb, biased.p)
#     enroll <<- res[[2]]
#     write.csv(enroll, "log.csv", row.names=FALSE)
#     paste("Group assigned to this patient:", "Group", res[[1]])
#   })
  
  output$dt <- renderDataTable(options = list(bFilter = FALSE), {
    if(input$submit == 0) return()
    
    res <- DHR(isolate(c(input$type, input$gender, input$age, input$site)), 
               enroll, cov.type, max.imb, biased.p)
    enroll <<- res[[2]]
    id.list <<- c(id.list, isolate(input$id))
    
    hist <- cbind(id.list, enroll)
    names(hist) <- c("Patient #", "Surgery Type", "Gender", "Age", 
                       "Treatment Site", "Group")
    write.csv(hist, "log.csv", row.names=FALSE)
    return(hist)
  })
  
  
#   output$dl <- downloadHandler(
#     # This function returns a string which tells the client
#     # browser what name to use when saving the file.
#     filename = function() {
#       paste("randomization-history-", Sys.time(), ".csv", sep = "")
#     },
#     
#     # This function should write data to a file given to it by
#     # the argument 'file'.
#     content = function(file) {
#       write.csv(enroll, file, row.names = FALSE)
#     }
#   )
#   
})

cov.type <- c("factor", "factor", "numeric", "factor")
max.imb <- c(3, 4, 3, 2, 10)
biased.p <- c(0.8, 0.2)
enroll <- NULL
id.list <- NULL

#### DHR() - DHR algorithm, 2 arms, 1:1 (1 = ctrl, 2 = act)
#    new.subj = vector of characteristics of the new subject coming in 
#    enroll = matirx of characteristics of subjects already enrolled 
#    cov.type = covariate type, e.g. "factor" or "numeric"
#    max.imb = vector of imbalance threshold for each covariate
#    biased.p = biased coin probability, e.g. biased.p <- c(0.8, 0.2)

DHR <- function(new.subj, enroll=NULL, cov.type, max.imb, biased.p) {
  
  new.subj <- data.frame(t(new.subj))
  cont.id <- which(cov.type == "numeric")
  
  # Convert continous covariate type to "numeric"  
  if(sum(cov.type == "numeric") >= 1) { 
    for(k in 1:length(cont.id)) {
      kk <- cont.id[k]
      new.subj[, kk] <- as.numeric(levels(new.subj[, kk]))[new.subj[, kk]]     
    }
  }
  
  # Randomly assign the first subject
  if(is.null(enroll)) {      
    new.subj$trt <- which(rmultinom(1, 1, c(0.5, 0.5)) == 1)
    enroll <- new.subj
  }
  
  # If not the first subject, then implement DHR
  else { 
    # Hierarchy levels (X1, X2, X3, ...)
    n.hierarchy <- length(cov.type)    
    assigned <- FALSE    
    
    # Check imbalance at each hierarchy level                   
    for(j in 1:n.hierarchy) {      
      if(assigned == FALSE) {
        
        # Conver continuous covariate to categorial using quartile
        if(cov.type[j] == "numeric") {
          cont.cov <- rbind(enroll[, 1:n.hierarchy], new.subj)[, j]
          cate.cov <- cut(cont.cov, unique(quantile(cont.cov)), include.lowest=T)
          # New patient: stratum j level k (new.j)
          new.j <- cate.cov[length(cate.cov)]
          enroll.j <- cate.cov[1:(length(cate.cov) - 1)]
        } else {
          new.j <- as.vector(new.subj[, j])
          enroll.j <- as.vector(enroll[, j])
        }
        
        # Identify previously enrolled subjects in jk
        id.jk <- which(enroll.j == new.j)    
        N.jk <- NULL
        for(tt in 1:2){ N.jk[tt] <- sum(enroll$trt[id.jk] == tt) }
        imb.jk <- abs(N.jk[1] - N.jk[2])
        
        if(imb.jk >= max.imb[j]) {
          p <- biased.p[rank(N.jk)]
          new.subj$trt <- which(rmultinom(1, 1, p) == 1)
          enroll <- rbind(enroll, new.subj)
          assigned <- TRUE
        }
      }
    }
    # Check imbalance at overall trial level
    if(assigned == FALSE) {      
      for(tt in 1:2){ N.jk[tt] <- sum(enroll$trt == tt) }
      imb.j <- abs(N.jk[1] - N.jk[2])
      
      if(imb.j >= max.imb[length(max.imb)]) {
        p <- biased.p[rank(N.jk)]
        new.subj$trt <- which(rmultinom(1, 1, p) == 1)
        enroll <- rbind(enroll, new.subj)
        assigned <- TRUE
      }
      # If nothing imbalanced, then randomly assign the subject
      else {
        new.subj$trt <- which(rmultinom(1, 1, c(0.5, 0.5)) == 1)
        enroll <- rbind(enroll, new.subj)
      }
    }
  }
  # Return trt assignment for the new subject & updated enrollment matrix 
  return(list(new.subj$trt, enroll))
}