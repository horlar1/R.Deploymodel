#' Deploy Model to SQL Server, Spark And Hadoop
#'
#' @param connection RODBC connection object returned by createConnection
#' @param procedure_name name of the procedure as passed to create_procedure
#' @param model model object as returned by serialize_model
#' @param name name of the algorithm. example: Logistic Regression model, LightGBM model
#' @param type model type (classification or Regression, et.c)
#' @param description model description
#' @param version model version
#' @param author name of the author/creator
#'
#' @export
#'@seealso \code{\link{createConnection} \link{create_procedure} \link{serializeModel}}
#' @examples \dontrun{
#' # don't run this sript
#' }
#' con = create_connection(server = "",database = "",username="",password="")

deployModel <- function(connection,procedure_name,model,name,type,description,version,author){

  stopifnot(inherits(connection, c("RODBC")))

  if(!inherits(model, c("character")))
    stop("'model' is not is right format, serialize 'model")

  creatMod <- function(procedure_name,model,name,type,description,version,author){

    exec <- paste0("exec ","[dbo].[",procedure_name,"]")

    query <- paste(exec,"@m='", model, "'",  ",",
      "@modelname = '", name, "' ,",
      "@modeltype = '", type, "' ,",
      "@modeldescription = '", description, "' ,",
      "@modelversion = '", version, "' ,",
      "@author = '", author, "' ;",
      sep = ""
    )
    query
  }

  execstr <- creatMod(procedure_name,model,name,type,description,version,author)

  out <- sqlQuery(connection,execstr, errors = TRUE)

  if(is_empty(out)){
    return("Model Deployed Successfully!!")
  }else{
    return(list(out))
  }

}
