
domain<-function(X, D, texts) {

   # X
   X <- as.matrix(X)
   n <- nrow(X)
   m <- ncol(X)
   if (any(is.na(X))) stop("'X' has unknown values")
   if (is.null(colnames(X))) stop(paste(texts, "have not defined the column names!"))
   
   # D
   D <- as.matrix(D)
   
   if (nrow(D) != n) stop("'D' and 'X' have different row count")
   if(is.null(colnames(D))) stop("The column names in the matrix 'D' has not defined")
  
   Dom0 <- unique(D)
   Dom_agg<-Dom0[do.call(order, lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
   Dom_agg<-as.matrix(Dom_agg)
   colnames(Dom_agg)<-colnames(D)
   L <- nrow(Dom_agg)
   n <- nrow(X)

   domen <- foreach(i = 1:ncol(X), .combine = cbind) %do% {
      X_dom <- foreach(k = 1:L, .combine = cbind) %do% 
        ifelse(rowSums(D == matrix(Dom_agg[k, ], n, ncol(D), T)) == ncol(D), X[,i], 0)}
 
     domen<-as.matrix(domen)
     namesD<-function(X,D) {
         d <- colnames(X)
         b <- colnames(D)
         Dom0 <- unique(D)
         Dom_agg <- Dom0[do.call(order, lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
         Dom_agg <- as.matrix(Dom_agg)
         colnames(Dom_agg) <- colnames(D)
         L <- nrow(Dom_agg)
         h <- c()
    
         for (i in 1:L) {
               c <- paste(b, Dom_agg[i,], sep=".")
               h[i] <- do.call(paste, as.list(c(c, sep="__")))}
    
         g <- foreach(i = 1:ncol(X), .combine = cbind) %do% {
                nsk1 <- paste(d[i], h, sep="__")}
    
         s <- paste(as.list(g))
         s }
     colnames(domen) <- namesD(X, D)
     domen
}


