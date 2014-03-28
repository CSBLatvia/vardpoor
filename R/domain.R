
domain <- function(Y, D, texts) {
   # Y
   Y <- data.table(Y, check.names=TRUE)
   n <- nrow(Y)
   if (any(is.na(Y))) stop("'Y' has unknown values")
   if (is.null(names(Y))) stop(paste(texts, "have not defined the column names!"))
   
   # D
   D <- data.table(D)
   if (any(duplicated(names(D)))) 
           stop("'D' are duplicate column names: ", 
                paste(names(D)[duplicated(names(D))], collapse = ","))
   if (nrow(D) != n) stop("'D' and 'Y' have different row count")
   if(is.null(names(D))) stop("The column names in the 'D' has not defined")
  
   Dom_agg <- data.table(unique(D))
   setkeyv(Dom_agg, names(Dom_agg))
   	
   k <- i <- NULL
   domen <- foreach(i = 1:ncol(Y), .combine = cbind) %do% {
         X_dom <- foreach(k = 1:nrow(Dom_agg), .combine = cbind) %do% 
            ifelse(rowSums(D==Dom_agg[k,][rep(1,n),]) == ncol(D),as.data.frame(Y)[,i],0)} 

   namesD <- function(Y,D) {
           d <- names(Y)
           b <- names(D)
           Dom_agg <- data.table(unique(D))
           setkeyv(Dom_agg, names(Dom_agg))
            
           h <- c()   
           for (i in 1:nrow(Dom_agg)) {
                 c <- paste(b, as.matrix(Dom_agg[i]), sep=".")
                 h[i] <- do.call(paste, as.list(c(c, sep="__")))}
  
           g <- foreach(i = 1:ncol(Y), .combine = cbind) %do% {
                  nsk1 <- paste(d[i], h, sep="__")}
    
           s <- paste(as.list(g))
           s }
    domen <- data.frame(domen)
    colnames(domen) <- namesD(Y, D)
    domen <- data.table(domen)
    return(domen)
 }

