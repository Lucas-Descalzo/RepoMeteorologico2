utils::globalVariables("temperatura_abrigo_150cm")

#' Crea una Tabla Resumen de la Temperatura Abrigo
#'
#' La función `tabla_resumen_temperatura` crea una tabla de resumen de la variable
#' `temperatura_abrigo_150cm` para una o más estaciones.
#'
#' @param datos data.frame con los datos de las estaciones.
#' @param estaciones Vector de IDs de las estaciones a analizar (opcional).
#'
#' @return
#' Devuelve una tabla resumen con el mínimo, máximo, promedio, desviación estándar,
#' cantidad de observaciones y proporción de valores NA de la temperatura abrigo.
#'
#' @export
#' @import dplyr
#'
#' @examples
#' # Cargar los datos de ejemplo desde el paquete
#' data("estaciones_merged")
#'
#' # Llamar a la función usando los datos cargados
#' tabla_resumen_temperatura(estaciones_merged)
tabla_resumen_temperatura <- function(datos, estaciones = NULL) {
  # Verificar si datos es un dataframe
  if (!is.data.frame(datos)) {
    cli::cli_abort("Error: El objeto 'datos' debe ser un dataframe.")
  }

  # Si se especifican estaciones, filtrar los datos
  if (!is.null(estaciones)) {
    estaciones_existentes <- unique(datos$id)
    estaciones_no_existentes <- setdiff(estaciones, estaciones_existentes)

    if (length(estaciones_no_existentes) > 0) {
      cli::cli_abort(c("Las siguientes estaciones no existen:",
                       paste(estaciones_no_existentes, collapse = ", ")))
    }
    datos <- datos %>% dplyr::filter(id %in% estaciones)
  }

  # Calcular el resumen
  resumen <- datos %>%
    group_by(id) %>%
    summarise(
      observaciones_temperatura = sum(!is.na(temperatura_abrigo_150cm)),
      proporcion_NA = sum(is.na(temperatura_abrigo_150cm)) / n(),
      temperatura_minima = min(temperatura_abrigo_150cm, na.rm = TRUE),
      temperatura_maxima = max(temperatura_abrigo_150cm, na.rm = TRUE),
      temperatura_promedio = mean(temperatura_abrigo_150cm, na.rm = TRUE),
      .groups = 'drop'
    )

  return(resumen)
}


