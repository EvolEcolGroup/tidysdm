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

.vif2 <- function(y,w) {
  z<-rep(NA,length(w))
  names(z) <- colnames(y)[w]
  for (i in 1:length(w)) {
    z[i] <-  1/(1-summary(lm(as.formula(paste(colnames(y)[w[i]],"~.",sep='')),data=y))$r.squared)
  }
  return(z)
}

# estimate the variance inflation factor
# vif_fast only works with continuous variables
# note that cols are indices of the columns of interest
vif_fast <- function(data_mat, cols = NULL){
  # in case we passed a data.frame, fix it and transform it into a matrix
  if (inherits(data_mat,"data.frame")){
    data_mat <- as.matrix(data_mat)
  }
  if (is.null(cols)){
    cols <- seq.int(1,ncol(data_mat))
  }
  data_mat <- cbind(data_mat,1) ## convert to a design matrix with an intercept
  vif_one_col <- function( i_col,data_mat){
    this_resid<-.lm.fit(data_mat[,-i_col], y=data_mat[,i_col])$residuals
    return(1/(sum(this_resid^2)/sum((data_mat[,i_col] - mean(data_mat[,i_col]))^2 )))
  }
  sapply(cols,FUN = vif_one_col, data_mat = data_mat)
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



setMethod('vifcor', signature(x='matrix'),
          function(x, th=0.9, keep=NULL, size, method='pearson') {
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

vif_step<-         function(x, th=10, keep=NULL, size, method='pearson') {

            var_names <- colnames(x)

            #----------------
            LOOP <- TRUE
            
            vars_to_remove <- c()
               while (LOOP) {
                v <- vif_fast(x)
                # remove the variables to keep
                if (!null(keep)){
                  v <- v[!names(v) %in% keep]
                }
                
                if (v[which.max(v)] >= th) {
                  ex <- names(v[which.max(v)])
                  vars_to_remove <- c(vars_to_remove,ex)
                  x <- x[,-which(colnames(x) == ex)]
                } else {
                  LOOP=FALSE
                }
              }
            vars_kept <- var_names[!var_names %in% vars_to_remove]
            if (verbose){
              print(vif_fast(x))
              print(cor(x,method))
            }
}

#------------
