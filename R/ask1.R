#' 간단 질문 함수
#'
#' @export
ask1 <- function() {
  input <- readline(prompt = "> ")
  input <- paste0('"', input, '"')   # 입력을 자동으로 따옴표 감싸기
  heip(input, history = 0, data = 0)
}
