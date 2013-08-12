#' Creata line for a LaTeX codebook. 
#'
#' @param con connection to database 
#' @param tablename to check, without schema 
#' @return column of column names 
#' @export
get_field_names <- function(con, tablename){
    query <- paste("select column_name from information_schema.columns where table_name='",
                   tablename, "'", sep = "")
    dbGetQuery(con, query)[,c("column_name")]
}
