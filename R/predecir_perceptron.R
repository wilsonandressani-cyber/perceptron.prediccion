
#' Predecir con un perceptron entrenado
#'
#' @param modelo Lista con pesos y sesgo del perceptron.
#' @param X Matriz o data frame con las variables predictoras.
#'
#' @return Vector de predicciones binarias (0 o 1).
#' @export
#'
#' @examples
#' modelo <- list(pesos = c(1, 1), sesgo = -1)
#' X <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1),
#'             ncol = 2, byrow = TRUE)
#' predecir_perceptron(modelo, X)

predecir_perceptron <- function(modelo, X) {

  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X debe ser una matriz o data frame.")
  }

  X <- as.matrix(X)

  if (!is.numeric(X) || any(!is.finite(X))) {
    stop("X debe contener valores numericos finitos.")
  }

  pesos <- modelo$pesos
  sesgo <- modelo$sesgo

  if (!is.numeric(pesos) ||
      any(!is.finite(pesos)) ||
      length(pesos) != ncol(X)) {
    stop("Las dimensiones de X y pesos no coinciden.")
  }

  if (!is.numeric(sesgo) ||
      length(sesgo) != 1 ||
      !is.finite(sesgo)) {
    stop("El sesgo debe ser un numero valido.")
  }

  activacion <- drop(X %*% pesos + sesgo)

  predicciones <- ifelse(activacion >= 0, 1L, 0L)

  return(as.integer(predicciones))
}
