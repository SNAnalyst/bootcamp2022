
#' Add standardized coefficients to lm output
#' 
#' Add standardized coefficients to lm summary
#' 
#' Using the output from an \code{lm} model, this functions adds the 
#' standardized coefficients to the output summary.
#' 
#' For a little more flexibility, consider the \code{lm.beta} package, which 
#' this function in based on.
#'
#' @param mod output from an \code{lm} model
#'
#' @return a \code{summary.lm} object
#' @export
#'
#' @examples
#' data(mtcars)
#' mm <- lm(mpg ~ ., data = mtcars)
#' summary(mm)
#' add_beta(mm)
add_beta <- function (mod) {
  if (!inherits(mod, "lm")) stop("'mod' has to be of class lm")
  
  intcp <- attr(attr(mod$model, "terms"), "intercept")
  model_matrix <- as.matrix(stats::model.matrix(mod))
  model_frame_1 <- as.matrix(stats::model.frame(mod)[, 1])

  numerator <- stats::coef(mod) * apply(model_matrix, 2, function(x) {
    sqrt(sum((x - mean(x, na.rm = T) * intcp)^2, na.rm = T))
    })
  denominator <- apply(model_frame_1, 2, function(x) {
    sqrt(sum((x - mean(x, na.rm = T) * intcp)^2, na.rm = T))
    })
  mod$standardized.coefficients <- numerator/denominator
  
  if (attr(attr(mod$model, "terms"), "intercept") == 1) {
    mod$standardized.coefficients[1] <- NA
  }
  
  # prepare output
  mod2 <- mod
  # mod2$standardized.coefficients <- NULL
  x.summary <- stats::summary.lm(mod2)
  x.summary$coefficients <- cbind(
    x.summary$coefficients[, 1, drop = F], 
    Standardized = mod$standardized.coefficients[rownames(x.summary$coefficients)], 
    x.summary$coefficients[, -1, drop = F]
    )
  
  x.summary
}
