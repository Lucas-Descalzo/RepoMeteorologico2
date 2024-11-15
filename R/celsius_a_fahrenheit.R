#' Convierte Temperatura de Celsius a Fahrenheit
#'
#' La función `celsius_to_fahrenheit` toma una temperatura en grados Celsius y la convierte en grados Fahrenheit.
#' @param temperatura_celsius Una temperatura en grado Celsius.
#'
#' @return
#' La temperatura ingresada pero en grados Fahrenheit.
#' @export
celsius_to_fahrenheit <- function(temperatura_celsius) {
  if (!is.numeric(temperatura_celsius)) {
    cli::cli_abort("La temperatura debe ser numerica.")
  }

  temperatura_fahrenheit <- (temperatura_celsius * 9/5) + 32

  return(temperatura_fahrenheit)
}
