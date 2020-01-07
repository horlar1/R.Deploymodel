
#' Create Procedure to accept and store a model
#' @description Model Deployment
#'
#' @param connection a connection to the database from createConnection
#' @param procedure_name name of the procedure to create
#' @param table_name name of the table to accept/store the model
#'
#' @importFrom RODBC sqlQuery
#' @importFrom purrr is_empty
#' @seealso \code{\link{createConnection}}
#' @export
#' @examples \dontrun{
#' # don't run this sript
#' con = createConnection(server = "",database = "",username="",password="")
#' createProcedure(con,"model_procedure","model_table")
#' }

createProcedure <- function(connection,procedure_name,table_name){

  stopifnot(inherits(connection, c("RODBC")))

  if( !inherits(procedure_name, "character"))
    stop("procedure name is not in the right format/short")
  if( !inherits(table_name, "character"))
    stop("table name is not in the right format/short")

  run_query <- sqlQuery(connection,paste0('
create PROCEDURE [dbo].[',procedure_name,']','
( @m NVARCHAR(MAX),
  @modelname NVARCHAR(100),
  @modeltype NVARCHAR(100),
  @modeldescription NVARCHAR(MAX),
  @modelversion FLOAT,
  @author NVARCHAR(100)) as begin
IF OBJECT_ID(',paste0("'",'dbo.',table_name,"'"),') is null

BEGIN

CREATE TABLE dbo.',table_name,'
(
  model VARBINARY(MAX),
  modelname NVARCHAR(100),
  modeltype NVARCHAR(100),
  modeldescription NVARCHAR(MAX),
  modelversion FLOAT,
  author NVARCHAR(MAX),
  insertdate datetime
);

END
--

SET NOCOUNT ON;

INSERT INTO dbo.',table_name,' (model, modelname,modeltype,modeldescription, modelversion, author,insertdate)
VALUES(
  CONVERT(VARBINARY(MAX), @m ,2),
  @modelname,
  @modeltype,
  @modeldescription,
  @modelversion,
  @author,
  GETDATE()
)

END
'))

  if(is_empty(run_query)){
    return("Procedure Created Successfully!!")
  }else{
    return(list(run_query))
  }

}
