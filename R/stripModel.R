#' Strip and Remove unneccesary large files associated with GLM and LM models
#'
#' @description Strip and Remove unneccesary large files associated with models. The function helps reduces the size of the model.
#' @param model a GLM or LM model
#'
#' @export
#' @importFrom utils object.size
#' @examples \dontrun{
#' # don't run this sript
#' }

stripModel <- function(model){

  if(!inherits(model, c("glm","lm")))
    stop("'model' is not Supported, only 'glm' and 'lm' models are supported")

  print(paste("Before::",format(object.size(model),units = "Mb")))

  model$data <- NULL
  model$y <- NULL
  model$linear.predictors <- NULL
  model$weights <- NULL
  model$fitted.values <- NULL
  model$model <- NULL
  model$prior.weights <- NULL
  model$residuals <- NULL
  model$effects <- NULL
  model$qr$qr <- NULL

  print(paste("After::",format(object.size(model),units = "Mb")))

  return(model)
}
