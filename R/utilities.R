#' Writes an list of regression models generated via table to a file. 
#'
#' @param models is a list of models. 
#' @param renames is a list of renames.
#' @param out.file path to where table shoul be written
#' @return Nothing - writes to file out.file
#' @export


## Codebook.Entry <- function(var.name, label, note)
##     paste("\\item[", label, "](\\verb|", var.name, "|). ", note, sep = "")


## create.note <- function(l, include.row.end = TRUE, col.sep = "&&"){
##   n <- length(l)
##   s <- ""
##   i <- 1
##   for(note in l){
##     if(i < n){
##       cap <- col.sep
##     } else {
##       if(include.row.end){
##         cap <- "\\\\ \n "
##       } else {
##           cap <- " \n"
##       }
##     }
##     s <- paste(s, note, cap)
##     i <- i + 1
##   }
##   s
## }

## fix.sample.size.line <- function(x){
##   entries <- strsplit(gsub("\\s","", gsub("\\\\","",x)) , '&')[[1]]
##   new <- ""
##   for(e in entries){
##     if (e == "N"){
##      new <- paste(new,  e)
##     }
##     if (e == ""){
##       new <- paste(new, "&&")
##     }
##     if (e != "N" && e != ""){
##       new <- paste(new, "\\multicolumn{1}{c}{", formatC(as.numeric(e), big.mark=",", format="f", digits=0), "}")
##     }
##   }
##   new <- paste(new, "\\\\ \n ")
##   new
## }


## createCommand <- function(command.name, value) paste("\\newcommand{\\", command.name, "}{", value, "}", sep="")
    

## flattenList <- function(first.key, values){
##     new.list <- list()
##     for(second.key in names(values)){
##         new.list[[ paste(first.key, toupper(second.key), sep ="") ]] = values[[ second.key ]]
##     }
##     new.list
## }


## pp.big.number <- function(e) formatC(as.numeric(e), big.mark=",", format="f", digits=0)


## write.image <- function(filename, g, width = 5, height = 5,  format = "png"){
##   "Writes a passed ggplot, g, to the filename. The default format is png."
##   do.call(format, list(filename, width, height))
##    print(g)
##   dev.off()
## }




## latex.row.label <- function(width, label, rule.pts = 0, new.lines = 0, new.line.char = FALSE){
##   rule.block <- gsub("<rule.pts>", rule.pts, "\\rule{0pt}{<rule.pts>pt}")
##   new.line.block <- paste(do.call(paste, as.list(rep(" \\\\", new.lines))), ifelse(new.line.char,"\n",""), sep=" ")
##   string.literal <- "\\multirow{2}{*}{\\parbox{<width>cm}{<label>}}"
##   t1 <- gsub("<width>", width, string.literal)
##   t2 <- gsub("<label>", label, t1)
##   paste(rule.block, t2, new.line.block, sep = "")
## }

## spanning.latex.column.label <- function(num.columns, width, label){
##   string.literal <- "\\multicolumn{<num.columns>}{c}{\\parbox[t][][t]{<width>cm}{\\emph{DV = } <label>}}"
##   t1 <- gsub("<num.columns>", num.columns, string.literal)
##   t2 <- gsub("<width>", width, t1)
##   gsub("<label>", label, t2)
## }

## # - Need to figure out how to append in R. 
## ## create.parameter.entry <- function(parameter.name, parameter.value, out.file){
## ##   command <- paste("\\newcommand{\\", parameter.name, "}{", value, "}\n", sep = "")  
## ## }






## # get the opening URL

## get.job.post <- function(opening.id, con){
##   sql.query <- paste('select "ProfileKey" from "oDesk DB"."Openings" where "Record ID#" = ', opening.id, ' limit 1', sep = "")
##   result <- dbGetQuery(con, sql.query)
##   url <- paste("http://www.odesk.com/jobs/", result[,1], sep = "")
##   browseURL(url)
## }


## get.contractor <- function(contractor.id, con){
##   sql.query <- paste('select "ProfileKey - CfullFfull" from "oDesk DB"."Developers" where "Record ID#" = ',
##                      contractor.id, ' limit 1', sep = "")
##   result <- dbGetQuery(con, sql.query)
##   url <- paste("http://www.odesk.com/users/", result[,1], sep = "")
##   browseURL(url)
## }


## get.employer.odw <- function(employer.id, con){
##   sql.query <- paste('select * from agg.b_employer where employer = ', employer.id, 'limit 1',
##                      sep = "")
##   result <- dbGetQuery(con, sql.query)
##   str(result)
## }

## get.job.post.odw <- function(opening.id, con){
##   sql.query <- paste('select * from agg.b_opening where opening = ', opening.id,
##                      sep = "")
##   result <- dbGetQuery(con, sql.query)
##   str(result)
## }


## get.contractor.odw <- function(contractor.id, con){
##   sql.query <- paste('select * from agg.b_contractor where contractor = ',
##                      contractor.id, 'limit 1',
##                      sep = "")
##   result <- dbGetQuery(con, sql.query)
##   str(result)
## }

## get.qt.user <- function(username, con){
##   sql.query <- paste("select * from qt_users where username = '", username, "' limit 1", sep = "")
##   result <- dbGetQuery(con, sql.query)
##   str(result)
## }

## get.employers.jobs <- function(employer.id, con){
##   sql.query <- paste('select * from agg.b_opening where employer =', employer.id)
##   results <- dbGetQuery(con, sql.query)
##   results
## }



## is.Changed <- function(new.df, old.df){
##     !( digest(new.df)==digest(old.df) )
## }

## Get.Data <- function(schema, data.name, con, refresh = FALSE, data_dir = "../data"){
##     "Function that returns a data frame based on the name of a table in ODW.
##      It does not directly read from the database---it always reads from a
##      CSV file in the data_dir. If the CSV file does not exist and no 'refresh'
##      is asked for, it returns and error. If the CSV file does exist but a refresh
##      is called for, it executes the re-fresh and over-writes, but also notes
##      where there was a change."
##     table.name <- paste(schema, ".", data.name, sep="")
##     query <- paste("select * from ", table.name, sep="")
##     csv.name <- paste(data_dir, table.name, ".csv", sep = "")
##     old.file.exists <- file.exists(csv.name)
##     if ( !old.file.exists & !refresh ) stop("File does not exist. Need to re-run with a refresh")
##     if ( old.file.exists ) df.old <- read.csv(csv.name) 
##     if (refresh) {
##         schema.query <- paste("select tablename from pg_tables where schemaname='", schema, "'", sep="")
##         tables.on.server <- dbGetQuery(con, schema.query)[,"tablename"]
##         if( !(data.name %in% tables.on.server) ) {
##             stop("Table does not exist on server---check your SQL")
##         }
##         df.new <- dbGetQuery(con, query)
##         write.csv(df.new, file = csv.name)
##         df.new <- read.csv(csv.name)
##         if (old.file.exists) {
##             status <- ifelse(is.Changed(df.old, df.new), "_changed_", "_same_")
##             print(paste("The underlying dataset is: ", status, " from last refresh", sep=""))
##         }
##         df.new
##     } else {
##         df.old 
##     }
## }


## tableContinuous2 <- function (vars, weights = NA, subset = NA, group = NA, stats = c("n", 
##     "min", "q1", "median", "mean", "q3", "max", "s", "iqr", "na"), 
##     prec = 1, col.tit = NA, col.tit.font = c("bf", "", "sf", 
##         "it", "rm"), print.pval = c("none", "anova", "kruskal"), 
##     pval.bound = 10^-4, declare.zero = 10^-10, cap = "", lab = "", 
##     font.size = "footnotesize", longtable = TRUE, disp.cols = NA, 
##     nams = NA) 
## {
##     print.pval <- match.arg(print.pval)
##     if (identical(disp.cols, NA) == FALSE) {
##         stats <- disp.cols
##     }
##     if (is.data.frame(vars) == TRUE) {
##         tmp <- vars
##         vars <- list()
##         for (i in 1:ncol(tmp)) {
##             vars[[i]] <- tmp[, i]
##         }
##         nams <- colnames(tmp)
##     }
##     n.var <- length(nams)
##     if (identical(subset, NA) == FALSE) {
##         if (identical(group, NA) == FALSE) {
##             group <- group[subset]
##         }
##         if (identical(weights, NA) == FALSE) {
##             weights <- weights[subset]
##         }
##         for (i in 1:n.var) {
##             vars[[i]] <- vars[[i]][subset]
##         }
##     }
##     for (i in 1:length(nams)) {
##         nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
##     }
##     if (identical(col.tit, NA) == TRUE) {
##         col.tit.font <- match.arg(col.tit.font)
##         fonts <- getFonts(col.tit.font)
##         col.tit <- c(fonts$text("Variable"), fonts$text("Levels"), 
##             fonts$math("n"), fonts$text("Min"), fonts$math("q_1"), 
##             fonts$math("\\widetilde{x}"), fonts$math("\\bar{x}"), 
##             fonts$math("q_3"), fonts$text("Max"), fonts$math("s"), 
##             fonts$text("IQR"), fonts$text("\\#NA"))
##     }
##     if (identical(weights, NA) == TRUE) {
##         weights2 <- 1
##     }
##     if (identical(weights, NA) == FALSE) {
##         weights2 <- weights
##     }
##     n.levels <- 1
##     if (identical(group, NA) == FALSE) {
##         group <- factor(group, exclude = NULL)
##         group <- as.factor(group)
##         n.levels <- length(levels(group))
##         group <- rep(group, times = weights2)
##     }
##     for (i in 1:n.var) {
##         vars[[i]] <- rep(vars[[i]], times = weights2)
##     }
##     ncols <- length(stats)
##     s1 <- unlist(lapply(stats, is.character))
##     s1 <- (1:ncols)[s1]
##     s2 <- unlist(lapply(stats, is.function))
##     s2 <- (1:ncols)[s2]
##     out <- matrix(NA, ncol = 12, nrow = (n.levels + 1) * n.var)
##     out <- data.frame(out)
##     out.fct <- matrix(NA, ncol = length(s2), nrow = (n.levels + 
##         1) * n.var)
##     out.fct <- data.frame(out.fct)
##     for (i in 1:n.var) {
##         ind <- (i - 1) * (n.levels + 1) + 1:(n.levels + 1)
##         splits <- list(vars[[i]])
##         if (identical(group, NA) == FALSE) {
##             splits <- split(vars[[i]], group)
##         }
##         for (j in 1:n.levels) {
##             tmp <- as.vector(splits[[j]])
##             if (sum(is.na(tmp) == FALSE) != 0) {
##                 out[ind[j], 3] <- sum(is.na(tmp) == FALSE)
##                 out[ind[j], 4] <- min(tmp, na.rm = TRUE)
##                 out[ind[j], 5] <- quantile(tmp, 0.25, na.rm = TRUE)
##                 out[ind[j], 6] <- median(tmp, na.rm = TRUE)
##                 out[ind[j], 7] <- mean(tmp, na.rm = TRUE)
##                 out[ind[j], 8] <- quantile(tmp, 0.75, na.rm = TRUE)
##                 out[ind[j], 9] <- max(tmp, na.rm = TRUE)
##                 out[ind[j], 10] <- sd(tmp, na.rm = TRUE)
##                 out[ind[j], 11] <- out[ind[j], 8] - out[ind[j], 
##                   5]
##                 out[ind[j], 12] <- sum(is.na(tmp) == TRUE)
##                 if (length(s2) > 0) {
##                   for (f in 1:length(s2)) {
##                     out.fct[ind[j], f] <- stats[[s2[f]]](tmp[is.na(tmp) == 
##                       FALSE])
##                   }
##                 }
##             }
##         }
##         vi <- as.vector(vars[[i]])
##         out[max(ind), 3] <- sum(is.na(vi) == FALSE)
##         out[max(ind), 4] <- min(vi, na.rm = TRUE)
##         out[max(ind), 5] <- quantile(vi, 0.25, na.rm = TRUE)
##         out[max(ind), 6] <- median(vi, na.rm = TRUE)
##         out[max(ind), 7] <- mean(vi, na.rm = TRUE)
##         out[max(ind), 8] <- quantile(vi, 0.75, na.rm = TRUE)
##         out[max(ind), 9] <- max(vi, na.rm = TRUE)
##         out[max(ind), 10] <- sd(vi, na.rm = TRUE)
##         out[max(ind), 11] <- out[max(ind), 8] - out[max(ind), 
##             5]
##         out[max(ind), 12] <- sum(is.na(vi) == TRUE)
##         out[, 3:12][abs(out[, 3:12]) <= declare.zero] <- 0
##         if (length(s2) > 0) {
##             for (f in 1:length(s2)) {
##                 out.fct[max(ind), f] <- stats[[s2[f]]](vi[is.na(vi) == 
##                   FALSE])
##             }
##             out.fct[abs(out.fct) <= declare.zero] <- 0
##         }
##         v1 <- vars[[i]]
##         g1 <- as.character(group)
##         indNA <- (is.na(g1) == FALSE) & (g1 != "NA") & (is.na(v1) == 
##             FALSE) & (v1 != "NA")
##         v2 <- v1[indNA]
##         g2 <- g1[indNA]
##         ind1 <- length(unique(g2)) > 1
##         ind2 <- print.pval %in% c("anova", "kruskal")
##         ind3 <- 1
##         if (ind1 >= 1) {
##             splits2 <- split(v2, g2)
##             for (s in 1:length(splits2)) {
##                 if (sum(is.na(splits2[[1]]) == TRUE) == length(splits2[[1]])) {
##                   ind3 <- 0
##                 }
##             }
##         }
##         if (ind1 * ind2 * ind3 == 1) {
##             g2 <- as.factor(g2)
##             if (print.pval == "anova") {
##                 pval <- anova(lm(v2 ~ g2))$"Pr(>F)"[1]
##             }
##             if (print.pval == "kruskal") {
##                 pval <- kruskal.test(v2 ~ g2)$p.value
##             }
##             out[(i - 1) * (n.levels + 1) + n.levels + 1, 1] <- paste("p", 
##                 formatPval(pval, includeEquality = TRUE, eps = pval.bound))
##         }
##     }
##     dc <- c("n", "min", "q1", "median", "mean", "q3", "max", 
##         "s", "iqr", "na")
##     stats.num <- pmatch(stats[s1], dc)
##     align.stats <- ""
##     stats2 <- c(2 + stats.num)
##     out2 <- matrix(NA, ncol = 2 + length(s1) + length(s2), nrow = (n.levels + 
##         1) * n.var)
##     out2 <- data.frame(out2)
##     out2[, c(1, 2, 2 + s1)] <- out[, c(1, 2, stats2)]
##     out2[, 2 + s2] <- out.fct
##     out2[((1:n.var) - 1) * (n.levels + 1) + 1, 1] <- nams
##     dimnames(out2)[[2]][c(1:2, 2 + s1)] <- col.tit[c(1:2, stats2)]
##     if (length(s2) > 0) {
##         dimnames(out2)[[2]][2 + s2] <- names(stats)[names(stats) != 
##             ""]
##     }
##     for (i in 1:ncols) {
##         align.stats <- paste(align.stats, "r", sep = "")
##     }
##     if (n.levels == 1) {
##         prec <- c(rep(0, 1), rep(prec, ncols))
##         ali <- "ll"
##         out2 <- out2[, -2]
##     }
##     if (n.levels > 1) {
##         prec <- c(rep(0, 2), rep(prec, ncols))
##         ali <- "lll"
##     }
##     for (c in 2:ncol(out2)) {
##         if ((all(out2[, c] == round(out2[, c]), na.rm = TRUE) == 
##             TRUE) & (all(is.na(out2[, c])) == FALSE)) {
##             out2[, c] <- format(out2[, c], nsmall = 0)
##         }
##         else {
##             out2[, c] <- format(round(out2[, c], prec[c]), nsmall = prec[c])
##         }
##     }
##     tmp <- cumsum(rep(n.levels, n.var) + 1)
##     tab.env <- "longtable"
##     float <- FALSE
##     if (identical(longtable, FALSE)) {
##         tab.env <- "tabular"
##         float <- TRUE
##     }
##     if (n.levels == 1) {
##         out3 <- out2[(1:n.var - 1) * 2 + 1, ]
##         hlines <- 0
##         xtab3 <- xtable::xtable(out3, align = paste(ali, align.stats, 
##             sep = ""), caption = cap, label = lab)
##         ## xtab4 <- print(xtab3, include.rownames = FALSE, floating = float, only.contents = TRUE,
##         ##     type = "latex", hline.after = hlines, size = font.size, caption.placement = "top",
##         ##     sanitize.text.function = function(x) {
##         ##         x
##         ##     }, tabular.environment = tab.env)
##         xtab3
##     }
##     if (n.levels > 1) {
##         out2[, 2] <- rep(c(levels(group), "all"), times = n.var)
##         hlines <- sort(c(0, tmp - 1, tmp))
##         xtab1 <- xtable::xtable(out2, align = paste(ali, align.stats, 
##             sep = ""), caption = cap, label = lab)
##         ## xtab2 <- print(xtab1, include.rownames = FALSE, floating = float, only.contents = TRUE,
##         ##     type = "latex", hline.after = hlines, size = font.size, caption.placement = "top",
            
##         ##     sanitize.text.function = function(x) {
##         ##         x
##         ##     }, tabular.environment = tab.env)
##         xtab1
##     }
## }



## tableNominal2 <- function (vars, weights = NA, subset = NA, group = NA, miss.cat = NA, 
##     print.pval = c("none", "fisher", "chi2"), pval.bound = 10^-4, 
##     fisher.B = 2000, vertical = TRUE, cap = "", lab = "", col.tit.font = c("bf", 
##         "", "sf", "it", "rm"), font.size = "footnotesize", longtable = TRUE, 
##     nams = NA, cumsum = TRUE) 
## {
##     print.pval <- match.arg(print.pval)
##     if (is.data.frame(vars) == TRUE) {
##         tmp <- vars
##         vars <- list()
##         for (i in 1:ncol(tmp)) {
##             vars[[i]] <- tmp[, i]
##         }
##         nams <- colnames(tmp)
##     }
##     n.var <- length(nams)
##     if (identical(subset, NA) == FALSE) {
##         if (identical(group, NA) == FALSE) {
##             group <- group[subset]
##         }
##         if (identical(weights, NA) == FALSE) {
##             weights <- weights[subset]
##         }
##         for (i in 1:n.var) {
##             vars[[i]] <- vars[[i]][subset]
##         }
##     }
##     vert.lin <- "|"
##     if (vertical == FALSE) {
##         vert.lin <- ""
##     }
##     for (i in 1:length(nams)) {
##         nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
##     }
##     if (max(is.na(miss.cat)) == 0) {
##         for (i in miss.cat) {
##             vars[[i]] <- NAtoCategory(vars[[i]], label = "missing")
##         }
##     }
##     if (identical(group, NA) == TRUE) {
##         group <- rep(1, length(vars[[1]]))
##     }
##     if (identical(weights, NA) == TRUE) {
##         weights2 <- 1
##     }
##     if (identical(weights, NA) == FALSE) {
##         weights2 <- weights
##     }
##     for (i in 1:n.var) {
##         vars[[i]][vars[[i]] == "NA"] <- NA
##         vars[[i]] <- rep(vars[[i]], times = weights2)
##     }
##     group <- rep(group, times = weights2)
##     vars <- lapply(vars, as.factor)
##     group <- as.factor(group)
##     ns.level <- unlist(lapply(lapply(vars, levels), length))
##     n.group <- length(levels(group))
##     cumsum <- as.logical(cumsum)
##     stopifnot(identical(length(cumsum), 1L))
##     nColPerGroup <- 2L + as.integer(cumsum)
##     out <- matrix(NA, ncol = 2 + nColPerGroup * (n.group + 1), 
##         nrow = (sum(ns.level) + n.var))
##     out <- data.frame(out)
##     for (i in 1:n.var) {
##         ind <- max(cumsum(ns.level[1:i])) - ns.level[i] + 1:(ns.level[i] + 
##             1) + (i - 1)
##         splits <- split(vars[[i]], group)
##         for (g in 1:n.group) {
##             tmp <- splits[[g]]
##             tmp <- tmp[is.na(tmp) == FALSE]
##             if (sum(is.na(tmp)) > 0) {
##                 excl <- NULL
##             }
##             else {
##                 excl <- NA
##             }
##             tab <- table(tmp, exclude = excl)
##             tab.s <- round(100 * tab/sum(tab), 2)
##             out[ind, 2 + nColPerGroup * (g - 1) + 1] <- c(tab, 
##                 sum(tab))
##             out[ind, 2 + nColPerGroup * (g - 1) + 2] <- c(tab.s, 
##                 sum(tab.s))
##             if (cumsum) {
##                 out[ind, 2 + nColPerGroup * (g - 1) + 3] <- c(cumsum(tab.s), 
##                   NA)
##             }
##         }
##         out[ind[1], 1] <- nams[[i]]
##         out[ind, 2] <- c(levels(vars[[i]]), "all")
##         tab2 <- table(vars[[i]])
##         tab2.s <- round(100 * tab2/sum(tab2), 2)
##         out[ind, 2 + nColPerGroup * n.group + 1] <- c(tab2, sum(tab2))
##         out[ind, 2 + nColPerGroup * n.group + 2] <- c(tab2.s, 
##             sum(tab2.s))
##         if (cumsum) {
##             out[ind, 2 + nColPerGroup * n.group + 3] <- c(cumsum(tab2.s), 
##                 NA)
##         }
##         v1 <- vars[[i]]
##         g1 <- as.character(group)
##         indNA <- (is.na(g1) == FALSE) & (g1 != "NA") & (is.na(v1) == 
##             FALSE) & (v1 != "NA")
##         v2 <- as.character(v1[indNA])
##         g2 <- g1[indNA]
##         ind1 <- length(unique(g2)) > 1
##         ind2 <- print.pval %in% c("fisher", "chi2")
##         ind3 <- length(unique(v2)) > 1
##         splits2 <- split(v2, g2)
##         ind4 <- 1 - max(unlist(lapply(lapply(splits2, is.na), 
##             sum)) == unlist(lapply(lapply(splits2, is.na), length)))
##         if (ind1 * ind2 * ind3 * ind4 == 1) {
##             if (print.pval == "fisher") {
##                 pval <- if (fisher.B == Inf) 
##                   fisher.test(v2, g2, simulate.p.value = FALSE)$p.value
##                 else fisher.test(v2, g2, simulate.p.value = TRUE, 
##                   B = fisher.B)$p.value
##             }
##             if (print.pval == "chi2") {
##                 pval <- chisq.test(v2, g2, correct = TRUE)$p.value
##             }
##             out[max(ind), 1] <- paste("p", formatPval(pval, includeEquality = TRUE, 
##                 eps = pval.bound))
##         }
##     }
##     col.tit <- if (cumsum) {
##         c("n", "\\%", "\\sum \\%")
##     }
##     else {
##         c("n", "\\%")
##     }
##     col.tit.font <- match.arg(col.tit.font)
##     fonts <- getFonts(col.tit.font)
##     digits <- if (cumsum) {
##         c(0, 1, 1)
##     }
##     else {
##         c(0, 1)
##     }
##     groupAlign <- paste(rep("r", nColPerGroup), collapse = "")
##     al <- paste("lll", vert.lin, groupAlign, sep = "")
##     tmp <- cumsum(ns.level + 1)
##     hlines <- sort(c(0, tmp - 1, rep(tmp, each = 2)))
##     tab.env <- "longtable"
##     float <- FALSE
##     if (identical(longtable, FALSE)) {
##         tab.env <- "tabular"
##         float <- TRUE
##     }
##     if (n.group > 1) {
##         dimnames(out)[[2]] <- c(fonts$text("Variable"), fonts$text("Levels"), 
##             fonts$math(paste(col.tit, "_{\\mathrm{", rep(c(levels(group), 
##                 "all"), each = nColPerGroup), "}}", sep = "")))
##         for (i in 1:n.group) {
##             al <- paste(al, vert.lin, groupAlign, sep = "")
##         }
##         xtab1 <- xtable::xtable(out, digits = c(rep(0, 3), rep(digits, 
##             n.group + 1)), align = al, caption = cap, label = lab)
##         ## xtab2 <- print(xtab1,
##         ##                include.rownames = FALSE,
##         ##                floating = float, 
##         ##                type = "latex",
##         ##                hline.after = hlines,
##         ##                size = font.size,
##         ##                only.contents = TRUE, 
##         ##                sanitize.text.function = function(x) {
##         ##                    x
##         ##                },
##         ##                tabular.environment = tab.env)
##         xtab1
##     }
##     if (n.group == 1) {
##         out <- if (cumsum) {
##             out[, 1:5]
##         }
##         else {
##             out[, 1:4]
##         }
##         dimnames(out)[[2]] <- c(fonts$text("Variable"), fonts$text("Levels"), 
##             fonts$math(col.tit))
##         xtab1 <- xtable::xtable(out, digits = c(rep(0, 3), digits), 
##             align = al, caption = cap, label = lab)
##         ## xtab2 <- print(xtab1,
##         ##                include.rownames = FALSE,
##         ##                floating = float, 
##         ##                type = "latex",
##         ##                hline.after = hlines,
##         ##                size = font.size,
##         ##                only.contents = TRUE, 
##         ##                sanitize.text.function = function(x) {
##         ##                    x
##         ##                },
##         ##                tabular.environment = tab.env)
##         xtab1
##     }
##     xtab1
## }


## big.letter.theme <- theme(axis.text.y = element_text(size = 18),
##                           axis.text.x = element_text(size = 18),
##                           strip.text.x = element_text(size = 18),
##                           axis.title.x = element_text(size = 18),
##                           axis.title.y = element_text(size = 18)
##           )
