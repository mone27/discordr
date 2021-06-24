logging::basicConfig(level=10)
logging::removeHandler("basic.stdout") # remove default handler
# bot user can enable logging

#' Enable logging for the bot in console
#' 
#' @param level logging level
#' 
#' @export
enable_console_logging <- function(level=20){
  logging::addHandler(logging::writeToConsole, level=level)
}

#' Enable logging for the bot in a file
#' 
#' @param level logging level
#' 
#' @param file file where logs should be saved   
#' 
#' @export
enable_file_logging <- function(level=20, file="discordr-logs.log"){
  logging::addHandler(logging::writeToFile, level=level, file=file)
}
