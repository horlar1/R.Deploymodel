#' Simple Model Serialization Interface
#'
#' @description A simple low-level interface for serializing models.
#' @param model model object
#'
#' @export
#' @return A binary output
#' @seealso  \code{\link{serialize} \link{unserialize}}
#' @examples \dontrun{
#' # don't run this sript
#' }
#' x <- serialize(list(1,2,3), NULL)
#' unserialize(x)

serializeModel <- function(model){

  if(!inherits(model, c("glm","lm","rpart","randomforest")))
    stop("'model' is not supported")

  mod <- serialize(model, connection = NULL)
  mod <- paste0(mod, collapse = "")

  return(mod)
}
