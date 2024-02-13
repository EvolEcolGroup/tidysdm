## This code is based on functions from the usdm package by Babak Naimi
# Author: Babak Naimi, naimi.b@gmail.com
# Date :  Oct. 2019
# Last update: September 2023
# Version 1.7
# Licence GPL v3

.vif <- function(.dd) {
  z<-rep(NA,ncol(.dd))
  names(z) <- colnames(.dd)
  for (i in 1:ncol(.dd)) {
    z[i] <-  1 / (1 - summary(lm(.dd[,i]~.,data=.dd[-i]))$r.squared)
  }
  return(z)
}

.vif_fast <- function(data_df){
  ## Note: it would be better to work on matrices rather than on data.frames (as they are all numeric!!!)
  data_df <- cbind(1,as.matrix(data_df)) ## convert to a design matrix with an intercept
  vif_one_col <- function( i_col,data_mat){
    this_resid<-.lm.fit(data_mat[,-i_col], y=data_mat[,i_col])$residuals
    return(1/(sum(this_resid^2)/sum((data_mat[,i_col] - mean(data_mat[,i_col]))^2 )))
  }
  sapply(seq.int(2,ncol(data_df)),FUN = vif_one_col, data_mat = data_df)
}

.vif2 <- function(y,w) {
  z<-rep(NA,length(w))
  names(z) <- colnames(y)[w]
  for (i in 1:length(w)) {
    z[i] <-  1/(1-summary(lm(as.formula(paste(colnames(y)[w[i]],"~.",sep='')),data=y))$r.squared)
  }
  return(z)
}

.maxCor <- function(k){
  k <- abs(k)
  n <- nrow(k)
  for (i in 1:n) k[i:n,i] <- NA
  w <- which.max(k)
  c(rownames(k)[((w%/%nrow(k))+1)],colnames(k)[w%%nrow(k)])
}
#---
# it checks the correlation and if both of the variables in the pair with max-Cor are in keep, it 
# goes to next order of maximum correlation until a pair with at least one of them not in keep is selected
.maxCor2 <- function(k,keep){
  k <- abs(k)
  n <- nrow(k)
  for (i in 1:n) k[i:n,i] <- NA
  #w <- which.max(k)
  o <- order(k,decreasing = TRUE)
  w <- o[1]
  vn <- c(rownames(k)[((w%/%nrow(k))+1)],colnames(k)[w%%nrow(k)])
  if (all(vn %in% keep)) {
    LOOP <-TRUE
    j <- 2
    while(LOOP) {
      w <- o[j]
      vn <- c(rownames(k)[((w%/%nrow(k))+1)],colnames(k)[w%%nrow(k)])
      if (all(vn %in% keep) && j < length(o)) j <- j+1
      else LOOP <- FALSE
    }
    return (list(variables=vn,warning_level=j))
  } else {
    return(vn)
  }
  
}


.minCor <- function(k){
  k <- abs(k)
  rr<-c();cc<-c();co<-c()
  for (c in 1:(ncol(k)-1)) {
    for (r in (c+1):nrow(k)){
      rr<-c(rr,rownames(k)[r]);cc<-c(cc,colnames(k)[c])
      co <- c(co,k[r,c])
    }
  }
  w <- which.min(co)
  c(rr[w],cc[w])
}


function vif_cor_df (x, th=0.9, keep=NULL, size, method='pearson') {
#            if (ncol(x) == 1) stop("The data.frame should have at least two columns")
#            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            
#            x <- na.omit(x)
            
#            if (missing(size)) {
#              if (nrow(x) < 7000) size <- NULL
#              else size <- 5000
#            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            
#            if (!is.null(size)) {
#              if(nrow(x) > size) x <- x[sample(1:nrow(x),size),]
#            }
            #----
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and ncol(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            .warn <- FALSE
            
            LOOP <- TRUE
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                xcor <- abs(cor(x, method=method))
                mx <- .maxCor(xcor)
                if (xcor[mx[1],mx[2]] >= th) {
                  w1 <- which(colnames(xcor) == mx[1])
                  w2 <- which(rownames(xcor) == mx[2])
                  v <- .vif2(x,c(w1,w2))
                  ex <- mx[which.max(v[mx])]
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP <- FALSE
              }
            } else {
              
              while (LOOP) {
                xcor <- abs(cor(x, method=method))
                mx <- .maxCor2(xcor,keep=keep)
                if (is.list(mx)) {
                  .warn <- TRUE
                  mx <- mx[[1]]
                }
                if (xcor[mx[1],mx[2]] >= th) {
                  w1 <- which(colnames(xcor) == mx[1])
                  w2 <- which(rownames(xcor) == mx[2])
                  if (any(mx %in% keep)) {
                    ex <- mx[!mx %in% keep]
                  } else {
                    v <- .vif2(x,c(w1,w2))
                    ex <- mx[which.max(v[mx])]
                  }
                  
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP <- FALSE
              }
            }
            
            if (.warn) {
              if (length(keep) > 2) warning('At least two variables specified in "keep" are strongly correlated (i.e., are subjected to the collinearity issue)!')
              else warning('The two variables specified in "keep" are strongly correlated (i.e., are subjected to the collinearity issue)!')
            }
            #---
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)
#--------

setMethod('vifcor', signature(x='matrix'),
          function(x, th=0.9, keep=NULL, size, method='pearson') {
            if (ncol(x) == 1) stop("The matrix should have at least two columns")
            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            
            x <- as.data.frame(x)
            x <- na.omit(x)
            
            if (missing(size)) {
              if (nrow(x) < 7000) size <- NULL
              else size <- 5000
            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            
            if (!is.null(size)) {
              if(nrow(x) > size) x <- x[sample(1:nrow(x),size),]
            }
            #----
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and ncol(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            .warn <- FALSE
            
            LOOP <- TRUE
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                xcor <- abs(cor(x, method=method))
                mx <- .maxCor(xcor)
                if (xcor[mx[1],mx[2]] >= th) {
                  w1 <- which(colnames(xcor) == mx[1])
                  w2 <- which(rownames(xcor) == mx[2])
                  v <- .vif2(x,c(w1,w2))
                  ex <- mx[which.max(v[mx])]
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP <- FALSE
              }
            } else {
              
              while (LOOP) {
                xcor <- abs(cor(x, method=method))
                mx <- .maxCor2(xcor,keep=keep)
                if (is.list(mx)) {
                  .warn <- TRUE
                  mx <- mx[[1]]
                }
                if (xcor[mx[1],mx[2]] >= th) {
                  w1 <- which(colnames(xcor) == mx[1])
                  w2 <- which(rownames(xcor) == mx[2])
                  if (any(mx %in% keep)) {
                    ex <- mx[!mx %in% keep]
                  } else {
                    v <- .vif2(x,c(w1,w2))
                    ex <- mx[which.max(v[mx])]
                  }
                  
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP <- FALSE
              }
            }
            
            if (.warn) {
              if (length(keep) > 2) warning('At least two variables specified in "keep" are strongly correlated (i.e., are subjected to the collinearity issue)!')
              else warning('The two variables specified in "keep" are strongly correlated (i.e., are subjected to the collinearity issue)!')
            }
            #---
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)
########################################

if (!isGeneric("vifstep")) {
  setGeneric("vifstep", function(x, th= 10,keep=NULL, size, method='pearson',...)
    standardGeneric("vifstep"))
}

setMethod('vifstep', signature(x='RasterStackBrick'),
          function(x, th=10, keep=NULL, size, method='pearson') {
            if (nlayers(x) == 1) stop("The Raster object should have at least two layers!")
            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            
            if (missing(size)) {
              if (ncell(x) < 7000) size <- NULL
              else size <- 5000
            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            
            if (is.null(size)) x <- as.data.frame(x,na.rm=TRUE)
            else x <- sampleRandom(x,size,na.rm=TRUE)
            
            x <- na.omit(x)
            
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and nlyr(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x dataset!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            LOOP <- TRUE
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                v <- .vif(x)
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            } else {
              while (LOOP) {
                v <- .vif(x)
                v <- v[!names(v) %in% keep]
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            }
            #---
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)

setMethod('vifstep', signature(x='data.frame'),
          function(x, th=10, keep=NULL, size, method='pearson') {
            if (ncol(x) == 1) stop("The data.frame should have at least two variables!")
            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            
            x <- na.omit(x)
            
            if (missing(size)) {
              if (nrow(x) < 6000) size <- NULL
              else size <- 5000
            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            if (!is.null(size)) {
              if(nrow(x) > size) x <- x[sample(1:nrow(x),size),]
            }
            #----
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and ncol(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            
            LOOP <- TRUE
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                v <- .vif(x)
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            } else {
              while (LOOP) {
                v <- .vif(x)
                v <- v[!names(v) %in% keep]
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            }
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)

setMethod('vifstep', signature(x='matrix'),
          function(x, th=10, keep=NULL, size, method='pearson') {
            if (ncol(x) == 1) stop("The matrix should have at least two columns (variables)!")
            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            
            x <- as.data.frame(x)
            x <- na.omit(x)
            
            if (missing(size)) {
              if (nrow(x) < 6000) size <- NULL
              else size <- 5000
            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            
            if (!is.null(size)) {
              if(nrow(x) > size) x <- x[sample(1:nrow(x),size),]
            }
            #----
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and ncol(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            LOOP <- TRUE
            
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                v <- .vif(x)
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            } else {
              while (LOOP) {
                v <- .vif(x)
                v <- v[!names(v) %in% keep]
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            }
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)
#------------


setMethod('vifstep', signature(x='SpatRaster'),
          function(x, th=10, keep=NULL, size, method='pearson') {
            if (nlyr(x) == 1) stop("The Raster object should have at least two layers!")
            if (missing(method) || !method %in% c('pearson','kendall','spearman')) method <- 'pearson'
            if (missing(size)) {
              if (ncell(x) < 7000) size <- NULL
              else size <- 5000
            }
            
            if (missing(keep)) keep <- NULL
            #-----------
            
            if (is.null(size)) x <- as.data.frame(x,na.rm=TRUE)
            else x <- spatSample(x,size,na.rm=TRUE)
            
            x <- na.omit(x)
            
            vn <- colnames(x)
            
            if (!is.null(keep)) {
              if (is.numeric(keep)) {
                .w <- keep %in% c(1:length(vn))
                if (all(!.w)) stop(paste0('the values in keep are out of range; should be between 1 and ',ncol(x),' for your dataset...!'))
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of layer numbers in keep are out of range (should be between 1 and nlyr(x)), so they are ignored!')
                }
                keep <- vn[keep]
              } else if (is.character(keep)) {
                .w <- keep %in% colnames(x)
                if (all(!.w)) stop('None of the variable names in keep are available in x dataset!')
                if (any(!.w)) {
                  keep <- keep[.w]
                  warning('some of the variable names in keep are not available in x (check if it is typos), so they are ignored!')
                }
              }
            }
            #----------------
            LOOP <- TRUE
            n <- new("VIF")
            n@variables <- colnames(x)
            exc <- c()
            if (is.null(keep)) {
              while (LOOP) {
                v <- .vif(x)
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            } else {
              while (LOOP) {
                v <- .vif(x)
                v <- v[!names(v) %in% keep]
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  exc <- c(exc,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else LOOP=FALSE
              }
            }
            
            if (length(exc) > 0) n@excluded <- exc
            v <- .vif(x)
            n@corMatrix <- cor(x, method=method)
            n@results <- data.frame(Variables=names(v),VIF=as.vector(v))
            n
          }
)



