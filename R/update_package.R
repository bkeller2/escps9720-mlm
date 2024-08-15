## Update Package using remotes
# Copyright Brian Keller 2021, all rights reserved

# Updates itself
#' @export
update_package <- function(...) {
    remotes::update_packages('hlm', upgrade = 'always',...)
}
