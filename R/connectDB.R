#' #' Creates a connection to the DB and returns connection as a global variable, con
#'
#' @param credentials - list containing connection information 
#' @return None - run for side-effects.  
#' @export

connectDB <- function(credentials){
    con <<-  with(credentials, dbConnect(dbDriver(dbdriver),
                                    dbname=dbname,
                                    host = host,
                                    user = user,
                                    password = password,
                                    port = port))
}
