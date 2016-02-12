#' configurator-class
#' 
#' A small set of tools for managing configuration files, formatted like INI files in Windows.  See \url{http://effbot.org/librarybook/configparser.htm} and \url{http://docs.python.org/release/2.6.8/library/configparser.html} for inspiration.
#' @name configurator-class
#' @name aliases configurator
#' @docType package
#' @family CONFIGURATOR
#' @examples \dontrun{
#'   Cfg <- Configurator(system.file("extdata", "example.cfg", package = "configurator")
#'   Cfg
#'   n_fleas <- Cfg$get("Bar", "fleas")
#' }
NULL
