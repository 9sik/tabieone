#'
#' @export
ask <- function() {
  input <- readline(prompt = "> ")
  input <- paste0('"', input, '"')   # 입력 자동 따옴표 감싸기
  gpt(input, history = 0, data = 0)
}
