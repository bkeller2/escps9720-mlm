# Functions for helping loading files in R

# Gets file director
#' @export
get_filedir <- function(file = NULL, sep = '/') {
    if (is.null(file)) return(dirname(rstudioapi::getSourceEditorContext()$path))
    return(paste0(get_filedir(), sep, file))
}

# Sets the working directory to file directory
#' @export
set_filedir <- function() {
    setwd(get_filedir())
}


# Sets the working directory to file directory
#' @export
set_filedir <- function() {
    setwd(get_filedir())
}

