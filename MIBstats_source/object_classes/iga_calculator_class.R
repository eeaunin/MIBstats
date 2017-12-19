# _class for performing iterative group analysis (iGA) calculations
# _This class uses code written by Francesco Del Carratore

library(Rmpfr)

iga_calculator <- setClass("iga_calculator", 
                                    slots = c(metric = "numeric", group.membership = "matrix", var.names = "character", groups = "character", decreasing = "logical"),
                                    prototype=list(
                                      metric = NULL, group.membership = NULL, var.names = "", groups = "", decreasing = FALSE)
)

"hyper.geom_acc" <- function(z,n,t,x){
  if((x-z)>(n-t)){
    p<-0
  }else{
    p <- (chooseZ(t,z)*chooseZ(n-t, x-z))/chooseZ(n, x)
  }
  p
}

"iga_acc" <- function(z,n,t,x){
  if (z==0){
    p<-1
  } else {
    p <- iga_acc(z-1, n,t,x) - hyper.geom_acc(z-1,n,t,x)
  }
  p
}

setGeneric(name="set_iga_inputs",
           def=function(theObject, metric, group.membership, groups, var.names = NULL, decreasing=FALSE)
           {
             standardGeneric("set_iga_inputs")
           }
)

setMethod(f="set_iga_inputs",
          signature="iga_calculator",
          definition=function(theObject, metric, group.membership, groups, var.names = NULL, decreasing=FALSE)
            
            # _Method for adding iGA input data to the object
          {
            theObject@metric <- metric
            theObject@group.membership <- group.membership
            theObject@groups <- groups
            theObject@var.names <- var.names
            theObject@decreasing <- decreasing
            return(theObject)
          }
)

setGeneric(name="perform_iga",
           def=function(theObject)
           {
             standardGeneric("perform_iga")
           }
)

setMethod(f="perform_iga",
          signature="iga_calculator",
          definition=function(theObject)
            
            # _Method for iGA.
          {
            
            "iGA_acc" <- function(metric, group.membership, groups, var.names = NULL, decreasing=FALSE){
              
              
              
              
              if(length(metric) != dim(group.membership)[1]){
                cat('\n wrong inputs')
                stop()
              }
              if (!is.null(var.names) & (length(metric) != length(var.names))){
                cat('\n wrong var.names')
                stop()
              }
              
              N.groups <- dim(group.membership)[2]
              N <- length(metric)
              
              group.elements <- rep(NA, N.groups)
              for (g in 1:N.groups){
                group.elements[g] <- length(which(group.membership[,g]==1))
              }
              
              ord <- order(metric, decreasing= decreasing)
              metric <- metric[ord]
              group.membership <- group.membership[ord,]
              group.membership <- t(t(group.membership))
              if (!is.null(var.names)) var.names <- var.names[ord]
              rm(ord)
              
              PC.list <- list() #this list will contain a vector per each group,
              min.PCs <- rep(NA, N.groups) # this vector will contain the min PC-value for
              min.PCs.pos <- rep(NA,N.groups) # this vector will contain number of group
              
              for (g in 1:N.groups){
                x <- group.elements[g]  # number of elements in the gth group
                position <- 1:N         # vector from which we extract t
                PC.vec <- rep(NA, x)
                tmp <- group.membership[,g]
                store.t<-rep(NA, x)
                for (z in 1:x){
                  next.group.member <- which(tmp==1)[1]
                  t <- position[next.group.member]
                  p <- iga_acc(z,N,t,x)
                  PC.vec[z] <- as.numeric(p)
                  if (next.group.member < length(tmp)){
                    tmp <- tmp[(next.group.member+1):length(tmp)]
                    position <- position[(next.group.member+1):length(position)]
                  }
                  store.t[z] <-t
                }
                PC.list[[g]] <- PC.vec
                min.PCs[g] <- min(PC.vec)
                min.PCs.pos[g] <- store.t[which(PC.vec==min(PC.vec))[1]]        
              }
              names(PC.list) <- groups
              var.selected <- rep(NA,N.groups)
              if (!is.null(var.names)) var.selected.names <- list()
              for(g in 1:N.groups){
                ind<-which(group.membership[1:min.PCs.pos[g],g] == 1)
                var.selected[g] <- length(ind)
                if (!is.null(var.names)) var.selected.names[[g]] <- var.names[ind] 
              }
              summary <- matrix(NA, N.groups, 4)
              rownames(summary) <- groups
              summary[,1] <- min.PCs
              summary[,2] <- min.PCs.pos
              summary[,3] <- var.selected
              summary[,4] <- group.elements
              colnames(summary) <- c("minPC","list.position", "N.var.selected","N.var.group")
              if (is.null(var.names)){
                out <- list(PC.list = PC.list, minPCs = min.PCs, minPCs.pos = min.PCs.pos,
                            summary = summary)
              }else{
                out <- list(PC.list = PC.list, minPCs = min.PCs, minPCs.pos = min.PCs.pos,
                            summary = summary, var.sel.list = var.selected.names)    
              }
              out
            }
            
            
            iga_result <- iGA_acc(theObject@metric, theObject@group.membership, theObject@groups, theObject@var.names, theObject@decreasing)
            
            return(iga_result)
          }
)

setGeneric(name="perform_db_iga",
           def=function(theObject)
           {
             standardGeneric("perform_db_iga")
           }
)

setMethod(f="perform_db_iga",
          signature="iga_calculator",
          definition=function(theObject)
            
            # _Method for db-iGA.
          {
            
            "db.iGA_acc" <- function(metric, group.membership, groups, var.names = NULL, decreasing = FALSE){
              library(Rmpfr) 
              if(length(metric) != dim(group.membership)[1]){
                cat('\n wrong inputs')
                stop()
              }
              if (!is.null(var.names) & (length(metric) != length(var.names))){
                cat('\n wrong var.names')
                stop()
              }
              
              N.groups <- dim(group.membership)[2]
              N <- length(metric)
              
              group.elements <- rep(NA, N.groups)
              for (g in 1:N.groups){
                group.elements[g] <- length(which(group.membership[,g] == 1))
              }
              
              ord <- order(metric, decreasing = decreasing)
              metric <- metric[ord]
              group.membership <- group.membership[ord,]
              if (!is.null(var.names)) var.names <- var.names[ord]
              rm(ord)
              
              PC.list <- list() #this list will contain a vector per each group,
              min.PCs <- rep(NA, N.groups) # this vector will contain the min PC-value for
              min.PCs.pos <- matrix(NA, N.groups, 2)
              colnames(min.PCs.pos) <- c("start", "end")
              rownames(min.PCs.pos) <- groups
              for (g in 1:N.groups){
                x <- group.elements[g]
                PC.mat <- matrix(NA,x,N)
                position <- 1:N
                rows<-position[which(group.membership[,g] == 1)]
                for(start in 1:N){
                  position <- 1:N
                  tmp <- group.membership[,g]
                  TMP <-tmp
                  for(z in 1:x){
                    next.group.member <- which(tmp == 1)[1]
                    t <- position[next.group.member]
                    bef.start <- 0
                    if(start>1) bef.start <- length(which(TMP[1:(start-1)]== 1))
                    Z <- z-bef.start
                    if (t<start){
                      p <- NA
                    }else{
                      p <- iga_acc(Z, N, t-start+1, x)
                    }            
                    if (next.group.member < length(tmp)){
                      tmp <- tmp[(next.group.member+1):length(tmp)]
                      position <- position[(next.group.member+1):length(position)]
                    }
                    PC.mat[z,start]<- as.numeric(p)
                  }
                }
                row.names(PC.mat) <- rows
                PC.list[[g]] <- PC.mat
                min.PCs[g] <- min(PC.mat,na.rm=TRUE)
                pos <- which(PC.mat==min(PC.mat, na.rm = TRUE), arr.ind = TRUE) 
                if(is.matrix(pos)){
                  min.PCs.pos[g,1] <- as.numeric(pos[1,2])
                  min.PCs.pos[g,2] <- as.numeric(rownames(PC.mat)[pos[1,1]])
                }else{
                  min.PCs.pos[g,1] <- as.numeric(pos[2])
                  min.PCs.pos[g,2] <- as.numeric(rownames(PC.mat)[pos[1]])    
                }
              }
              var.selected <- rep(NA,N.groups)
              position <- 1:N
              if (!is.null(var.names)) var.selected.names <- list()
              for(g in 1:N.groups){
                ind<-which(group.membership[min.PCs.pos[g,1]:min.PCs.pos[g,2],g] == 1)
                ind<-position[min.PCs.pos[g,1]:min.PCs.pos[g,2]][ind]
                var.selected[g] <- length(ind)
                if (!is.null(var.names)) var.selected.names[[g]] <- var.names[ind] 
              }
              
              summary <- matrix(NA, N.groups, 5)
              rownames(summary) <- groups
              summary[,1] <- min.PCs
              summary[,2] <- min.PCs.pos[,1]
              summary[,3] <- min.PCs.pos[,2]
              summary[,4] <- var.selected
              summary[,5] <- group.elements
              colnames(summary) <- c("minPC","start", "end", "N.var.selected","N.var.group")
              
              if (is.null(var.names)){
                out <- list(PC.list = PC.list, minPCs = min.PCs, minPCs.pos = min.PCs.pos,
                            summary = summary)
              }else{
                out <- list(PC.list = PC.list, minPCs = min.PCs, minPCs.pos = min.PCs.pos,
                            summary = summary, var.sel.list = var.selected.names)    
              }
              out
              
            }

            iga_result <- db.iGA_acc(theObject@metric, theObject@group.membership, theObject@groups, theObject@var.names, theObject@decreasing)
            
            return(iga_result)
          }
          
)