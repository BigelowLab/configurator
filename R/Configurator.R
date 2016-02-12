
#' A Reference Class for configurations
#'
#' @field filename the name of the configuration file
#' @field comment the comment character
#' @field container a named list of the configuration sections
ConfiguratorRefClass <- setRefClass('ConfiguratorRefClass', 
   fields = list(
      filename = 'character',
      comment = 'character',
      container = 'list')
)

#' Parse one or more raw text lines of key=value or key:value form.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_parse_line
#' @param a character vector of key=value sets
#' @return a named character vector, names are keys data are the values
NULL
ConfiguratorRefClass$methods(
   parse_line = function(s){
      'Given one or more raw text lines, parse the key=value pairs into the 
      expected named character vector'
      pat <- '[=:]'
      ix <- regexpr(pat, s)
      r <- vector(mode = "character")
      for (i in seq_along(s)){
         if (ix[i] > 0 ){
            nm <- substring(s[i], 1, ix[i]-1)
            # strip leading spaces
            val <- sub("^ +", "", substring(s[i], ix[i] + 1, nchar(s[i]) ) )
            r[[nm]] <- val
         }
      }
      r
   }
)
#' Read a config file comprised of [sections] of key=value pairs.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_read_file
#' @param filename the fully qualified name of the configuration file
#' @return the configurations stored in a list
NULL
ConfiguratorRefClass$methods(
   read_file = function(filename = .self$filename){
      'read the specified configuration file'
      if (!file.exists(filename)) stop(paste("file must exist: ", filename))
      x <- scan(filename, what = character(), quiet = TRUE, sep = "\n", comment.char = .self$comment)
      ix <- grep("[", x, fixed = TRUE)
      if (length(ix) == 0) {
         stop("No [headers] found in config file - please check file")
      }
      len <- nchar(x[ix])
      nm <- substring(x[ix], rep(2, length(len)), len-1)
      iy <- c(ix, length(x)+1)
      L <- list()
      for (i in seq_along(nm)) 
         L[[nm[i]]] <- parse_line(x[(iy[i] + 1) : (iy[i+1]-1) ])      
      .self$container <- L
      invisible(L)
   }
)

#' Write a config file comprised of [sections] of key=value pairs.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_write_file
#' @param filename the fully qualified name of the configuration file
#' @param append logical, if TRUE append to the specified file otherwise truncate
#' @return logical named result form file.exists
NULL
ConfiguratorRefClass$methods(
   write_file = function(filename = .self$filename, append = FALSE){
      'write the specified configuration file'
      if (is.null(.self$container)) stop("There is nothing to write")
      if (length(.self$container) == 0) stop("There is nothing to write")
      open_mode <- ifelse(append, 'at', 'wt')
      ff <- file(filename, open = open_mode)
      L <- .self$container
      nmL <- names(L)
      for (nm in nmL){
         cat(paste0("[", nm, "]\n"), file = ff)
         keys <- names(L[[nm]])
         vals <- L[[nm]]
         for (k in keys) cat(paste0(k, '=', vals[[k]], "\n"), file = ff)
      }
      close(ff)     
      sapply(filename, file.exists)
   }
)

#' Retrieve one keyword/value pair by section or an entire section
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_get
#' @param section the name of the [section] to search
#' @param name the keyword name of the value to retrieve, if missing then the contents of the section are returned
#' @param default the value to return if the key or section is not found (by default NULL)
#' @return character value(s) or \code{default} if not found
#' @examples \dontrun{
#'    X$get("mySection", "myName")
#' }
NULL
ConfiguratorRefClass$methods(
   get =  function(section, name, default = NULL){
      'Retrieve the specified item in the sectioned named or NULL of not found'
      x <- .self$container[[section[1]]]
      if (!is.null(x) && !missing(name)) {
         if (name[1] %in% names(x)) {
            x <-x[[name[1]]]
         } else { 
            x <- default
         }
      }
      x
   } # get
)

#' Set one keyword/value pair by section
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_set
#' @param section the name of the [section] to search
#' @param name the keyword name of the value to set
#' @param value the value, must survive \code{\link{as.character}} conversion
#' @return logical TRUE if successful 
#' @examples \dontrun{
#'    X$set("mySection", "myName", someValue)
#' }
NULL
ConfiguratorRefClass$methods(
   set =  function(section, name, value){
      'Set the value of the specified item in the sectioned named'
      value <- try(as.character(value))
      OK <- !inherits(value, 'try-error')
      if (!OK) {
         cat(value, "\n")
         return(OK)
      }
      .self$container[[section[1]]][[name[1]]] <- value  
      invisible(TRUE)
   } # get
)


#' Print the contents of the Configurator
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_show
#' @param ... further arguments for \code{cat}
#' @return NULL invisibly
NULL 
ConfiguratorRefClass$methods(
   show = function(...){
      'Show the contents in a pretty format'
      L <- .self$container
      nmL <- names(L)
      for (nm in nmL){
         cat(paste0("[", nm, "]\n"), ...)
         keys <- names(L[[nm]])
         vals <- L[[nm]]
         for (k in keys) cat(paste0(k, '=', vals[[k]], "\n"), ...)
      }
      invisible(NULL)
   }
) #show

#' Create a new ConfiguratorRefClass and possibly read the contents of a 
#' config file.
#'
#' @family CONFIGURATOR
#' @export
#' @param filename a fully qualified path to a config file
#' @param comment character signals a comment line if it starts with this
#' @return an instance of ConfiguratorRefClass
Configurator <- function(filename, comment = "#"){
   x <- ConfiguratorRefClass$new()
   x$comment <- comment
   if (!missing(filename)) {
      x$filename <- filename
      x$read_file(filename)
   }
   invisible(x)
}
