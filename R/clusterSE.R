#' Create clustered standard errors for fixed effect regression. 
#' Note that the dataframe has to be sorted by the cluster.name to work. 
#' 
#' @param cluster.name is tje cluster identifier.
#' @param model is the lm object estimated on the demeanded data
#' @param df is the demead data.frame the model is fit on 
#' @return se
#' @export

clusterSE <- function(cluster.name, model, df){
    X <- model.matrix(model)
    df[, cluster.name] <- as.numeric(as.factor(df[,cluster.name]))
    clus <- cbind(X,df[,cluster.name],resid(model))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster.name,"resid")
    m <- dim(table(clus[,cluster.name])) # number of clusters
    n <- dim(X)[1]
    k <- dim(X)[2]
    dfc <- (m/(m-1))*((n-1)/(n-k)) # dof adjustment
    uclust <- matrix(NA, nrow = m, ncol = k) # uj matrix
    current.cluster <- 1
    row <- 1
    start <- 1
    end <- 1
    while (current.cluster < m){
        if ((end + 1) > nrow(df)) break
        while (df[end + 1, cluster.name] == current.cluster){
            end <- end + 1
        }
        uclust[current.cluster,] <- t(X[start:end,]) %*% resid(model)[start:end]
        start <- end + 1
        end <- start
        current.cluster <- current.cluster + 1 
    }
    uclust[current.cluster,] <- t(X[start:(end + 1),]) %*% resid(model)[start:(end + 1)]
    se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)
    se
}
