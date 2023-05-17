#' Run some R code as a local job
#'
#' This lets you execute long-running code in a new session, leaving you free
#' to continue working while your code runs. See 
#' [the RStudio documentation](https://solutions.rstudio.com/r/jobs/) for more
#' information. 
#'
#' This function works by copying the supplied code into a temporary R script
#' and then using `rstudioapi::jobRunScript()` to run it as a local job.
#'
#' @param code Code to run in a new session
#' @param name,workingDir,importEnv,... Passed to `rstudioapi::jobRunScript()`
#'
#' @return The location of the temporary R script created for the job
as_local_job <- function(code, name = "Local job", workingDir = ".", importEnv = TRUE, ...) {
  
  if (!(is_installed("rstudioapi") && rstudioapi::isAvailable())) {
    cli_warn("Couldn't run local job as RStudio not available")
    return(eval(code, envir = parent.frame()))
  }
  
  code <- expr_deparse(enexpr(code))
  code <- c('devtools::load_all(".")', code)
  file <- tempfile("local-job", fileext = ".R")
  writeLines(code, file)
  
  if (importEnv) {
    cli_alert("Copying global environment into new session for {.emph {name}}")
  }
  
  rstudioapi::jobRunScript(
    path = file, 
    name = name,
    workingDir = workingDir,
    importEnv = importEnv,
    ...
  )
  
  cli_alert("Running {.emph {name}} in the background")
  
  # Need to wait for 1 second before re-activating the console because of a bug
  # in RStudio/ rstudioapi`
  Sys.sleep(1)
  rstudioapi::executeCommand("activateConsole", quiet = TRUE)
  
  invisible(file)
  
}
