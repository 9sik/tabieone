#' 여러 줄 입력을 받아 ask로 전달하는 함수
#'
#' @export
ask1 <- function() {
  lines <- character()
  repeat {
    line <- readline("> ")
    if (line == "") break
    lines <- c(lines, line)
  }
  input <- paste(lines, collapse = "\n")
  input <- paste0('"', input, '"')
  ask(input, history = 0, data = 0)
}
