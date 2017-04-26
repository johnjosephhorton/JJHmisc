

HasChange <- function(x, k){
    if (k > 0) {
        shift.type <- "lag"
    } else {
        shift.type <- "lead"
    }
    cv <- as.numeric(I(abs(x - shift(x, abs(k), type = shift.type)) > epsilon))
    cv[is.na(cv)] <- 0
    cv
}
       
SumChanges <- function(x, k){
    if (k < -1){
        X <- sapply(-1:(k + 1), function(i) HasChange(x, i))
        v <- apply(X,1,sum)
    }
    if (k > 1){
        X <- sapply(1:(k-1), function(i) HasChange(x, i))
        v <- apply(X,1,sum)
    }
    if (k == 1 || k == -1) {
        v <- rep(FALSE, length(x))
    }
    v
}


CherryPickedOK <- function(x,k){
    sc <- SumChanges(x,k)
    ifelse(sc == 0 | is.na(sc), TRUE, FALSE)
}


AddIndicators  <- function(df, num.periods.post, num.periods.pre, var.name, index.name) {
    for(i in 1:num.periods.post){
        df[, (paste0("t",i)) := HasChange(eval(parse(text = var.name)), i) *
                 CherryPickedOK(eval(parse(text = var.name)),i),
           by = index.name]
        df[, (paste0("lag.",i)) := lag(p, i),
           by = index.name]
    }
    for(i in -num.periods.pre:-1){
        df[, (paste0("tn",abs(i))) := HasChange(eval(parse(text = var.name)), i) *
                 CherryPickedOK(eval(parse(text = var.name)),i),
           by = index.name]
    }
    df
}

LagTerm <- function(var.name, i){
    paste0("I(", var.name, " - lag(", var.name, ",", i, ")):t", i)
}


LeadTerm <- function(var.name, i){
    paste0("I(lead(", var.name, ",", i, ") -", var.name, "):tn", i)
}

GenFormula <- function(x, num.periods.pre, num.periods.post){
    if (num.periods.pre < 1){
        lead.terms <- ""
    } else {
        lead.terms <- paste0(sapply(1:num.periods.pre,  function(i) LeadTerm(x,i)), collapse = " + ")
    }
    lag.terms <-  paste0(sapply(1:num.periods.post, function(i) LagTerm(x,i)), collapse = " + ")
    formula <- paste0("~ ", x, " + ",lead.terms, " + ", lag.terms)
    formula
}


ExtractCoef <- function(m, num.periods.pre, num.periods.post){
    betas <- coef(m)
    #ses <- sqrt(diag(vcov(m)))
    ses <- sqrt(diag(vcovHC(m,type="HC0",cluster="group")))
    num.coef <- length(coef(m))
    lr <- betas[1]
    se.lr <- ses[1]
    if (num.periods.pre > 0){
        leads <- betas[2:(1 + num.periods.pre)]
        se.leads <- ses[2:(1 + num.periods.pre)]
    }
    lags <- betas[(1 + num.periods.pre + 1):num.coef]
    se.lags <- ses[(1 + num.periods.pre + 1):num.coef]
    new.beta <- c(leads, lags + lr, lr)
    new.ses <-  c(se.leads, sqrt(se.lags^2 + se.lr^2), se.lr)
    t <- c(-num.periods.pre:-1, 1:(num.periods.post + 1))
    data.frame(t, effect = new.beta, se = new.ses)
}


 




