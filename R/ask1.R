ask1 <- function() {
  cat("여러 줄 입력하세요. (빈 줄만 입력하면 끝)\n")
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
