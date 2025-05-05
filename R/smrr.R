#' 콘솔 최근 출력 + 질문을 GPT로 전달하는 함수
#'
#' @param n 최근 콘솔 출력 줄 수 (기본값: 20)
#' @export
smrr <- function(n = 20) {
  # 1. 최근 콘솔 입력 기록 불러오기
  console_text <- ""
  history_file <- tempfile()
  savehistory(history_file)
  console_lines <- tryCatch(
    tail(readLines(history_file, warn = FALSE), n),
    error = function(e) character(0)
  )
  if (length(console_lines) > 0) {
    console_text <- paste0("🧾 [최근 콘솔 입력 ", n, "줄]\n", paste(console_lines, collapse = "\n"))
  }

  # 2. 질문 입력 (줄바꿈으로 계속, 빈 줄 입력 시 종료)
  cat("❓ GPT에게 보낼 질문을 입력하세요. (빈 줄 입력 시 종료)\n")
  lines <- character()
  repeat {
    line <- readline("> ")
    if (line == "") break
    lines <- c(lines, line)
  }
  question <- paste(lines, collapse = "\n")

  # 3. 최종 프롬프트 구성
  final_prompt <- paste0(console_text, "\n\n[❓ 질문]\n", question)

  # 4. ask() 호출 (기록, 환경정보는 중복하지 않음)
  ask(question = final_prompt, history = 0, data = 0)
}
