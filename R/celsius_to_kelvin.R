#' Convierte Temperatura de Celsius a Kelvin
#'
#' La función celsius_to_Kelvin toma una temperatura en grados Celsius y la convierte en grados Kelvin.
#' @param temperatura_celsius Una temperatura en grado Celsius.
#'
#' @return
#' La temperatura ingresada pero en grados Kelvin.
#' @export
celsius_to_kelvin <- function(temperatura_celsius) {
  if (!is.numeric(temperatura_celsius)) {
    cli::cli_abort("La temperatura debe ser numerica.")
  }

  temperatura_kelvin <- (temperatura_celsius + 273.15)

  return(temperatura_kelvin)
}
