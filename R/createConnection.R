#' Create a connection to the server
#' @param server server name for authentication (charcter string)
#' @param database database name
#' @param username username for authentication
#' @param password for authenitication (if required)
#' @export
#' @importFrom RODBC odbcDriverConnect
#' @seealso \code{\link[RODBC]{odbcConnect}}
#' @examples \dontrun{
#' # don't run this sript
#' con = createConnection(server = "",database = "",username="",password="")
#' }


createConnection <- function(server,database,username,password){

  if(missing(server))
    stop("missing argument 'server name'")
  if(missing(database))
    stop("missing argument 'database name'")
  if(missing(username))
    stop("missing argument 'username'")
  if(missing(password))
    stop("missing argument 'password'")

dbconnection<- odbcDriverConnect(paste0("Driver=ODBC Driver 13 for SQL Server;
                                  Server=",server,"; Database=",database,";
                                  Uid=",username,"; Pwd=",password,""))

  return(dbconnection)
}
