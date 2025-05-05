#' 데이터 개괄요약 + 질문을 GPT로 전달하는 함수
#'
#' @param data 하나 또는 여러 개의 데이터프레임 이름 (벡터 형식 가능)
#' @export
smr <- function(data) {
  # 여러 객체 이름을 문자열로 받았을 경우 처리
  data_names <- as.character(substitute(data))
  if (length(data_names) == 1 && grepl("^c\\(", data_names)) {
    data_names <- gsub("^c\\(|\\)$", "", data_names)
    data_names <- trimws(unlist(strsplit(data_names, ",")))
  }

  # 요약정보 저장용
  summaries <- character()

  # 각 데이터프레임에 대해 요약 생성
  for (dname in data_names) {
    if (!exists(dname, envir = .GlobalEnv)) {
      summaries <- c(summaries, paste0("[", dname, " 은 존재하지 않습니다]\n"))
      next
    }
    obj <- get(dname, envir = .GlobalEnv)
    if (!is.data.frame(obj)) {
      summaries <- c(summaries, paste0("[", dname, " 은 데이터프레임이 아닙니다]\n"))
      next
    }

    head_text <- capture.output(head(obj, 5))
    str_text <- capture.output(str(obj))
    summary_text <- capture.output(summary(obj))

    summaries <- c(summaries,
                   paste0("🧾 [", dname, " - head()]\n", paste(head_text, collapse = "\n")),
                   paste0("📦 [", dname, " - str()]\n", paste(str_text, collapse = "\n")),
                   paste0("📊 [", dname, " - summary()]\n", paste(summary_text, collapse = "\n")))
  }

  # 질문 받기
  lines <- character()
  repeat {
    line <- readline("> ")
    if (line == "") break
    lines <- c(lines, line)
  }
  question <- paste(lines, collapse = "\n")

  # ask()로 전달
  full_input <- paste(summaries, collapse = "\n\n")
  final_prompt <- paste0(full_input, "\n\n[❓ 질문]\n", question)
  ask(final_prompt, history = 0, data = 0)
}
