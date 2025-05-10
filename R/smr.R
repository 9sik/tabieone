#' 데이터 개괄요약 + 질문을 GPT로 전달하는 함수
#'
#' @param data 하나 또는 여러 개의 데이터프레임 이름 (벡터 형식 가능)
#' @param max_lines 각 블록별 최대 출력 줄 수 (기본값: 50줄, 초과 시 생략)
#' @export
smr <- function(data, max_lines = 50) {
  data_names <- as.character(substitute(data))
  if (length(data_names) == 1 && grepl("^c\\(", data_names)) {
    data_names <- gsub("^c\\(|\\)$", "", data_names)
    data_names <- trimws(unlist(strsplit(data_names, ",")))
  }

  summaries <- character()

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

    # 각 요약 출력
    head_text    <- capture.output(head(obj, 5))
    str_text     <- capture.output(str(obj))
    summary_text <- capture.output(summary(obj))
    class_text   <- capture.output(sapply(obj, class))
    na_text      <- capture.output(colSums(is.na(obj)))

    # 최대 줄 제한 적용 함수
    trim_lines <- function(x) {
      if (length(x) > max_lines) {
        c(x[1:max_lines], "...(생략됨)")
      } else {
        x
      }
    }

    summaries <- c(
      summaries,
      paste0("🧾 [", dname, " - head()]\n", paste(trim_lines(head_text), collapse = "\n")),
      paste0("📦 [", dname, " - str()]\n", paste(trim_lines(str_text), collapse = "\n")),
      paste0("📊 [", dname, " - summary()]\n", paste(trim_lines(summary_text), collapse = "\n")),
      paste0("📐 [", dname, " - 변수 클래스]\n", paste(trim_lines(class_text), collapse = "\n")),
      paste0("❗ [", dname, " - 결측치 개수]\n", paste(trim_lines(na_text), collapse = "\n"))
    )
  }

  # 사용자 질문 입력
  cat("")
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
#' 데이터 개괄요약 + 질문을 GPT로 전달하는 함수
#'
#' @param data 하나 또는 여러 개의 데이터프레임 이름 (벡터 형식 가능)
#' @param max_lines 각 블록별 최대 출력 줄 수 (기본값: 50줄, 초과 시 생략)
#' @export
smr <- function(data, max_lines = 50) {
  data_names <- as.character(substitute(data))
  if (length(data_names) == 1 && grepl("^c\\(", data_names)) {
    data_names <- gsub("^c\\(|\\)$", "", data_names)
    data_names <- trimws(unlist(strsplit(data_names, ",")))
  }

  summaries <- character()

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

    # 각 요약 출력
    head_text    <- capture.output(head(obj, 5))
    str_text     <- capture.output(str(obj))
    summary_text <- capture.output(summary(obj))
    class_text   <- capture.output(sapply(obj, class))
    na_text      <- capture.output(colSums(is.na(obj)))

    # 최대 줄 제한 적용 함수
    trim_lines <- function(x) {
      if (length(x) > max_lines) {
        c(x[1:max_lines], "...(생략됨)")
      } else {
        x
      }
    }

    summaries <- c(
      summaries,
      paste0("🧾 [", dname, " - head()]\n", paste(trim_lines(head_text), collapse = "\n")),
      paste0("📦 [", dname, " - str()]\n", paste(trim_lines(str_text), collapse = "\n")),
      paste0("📊 [", dname, " - summary()]\n", paste(trim_lines(summary_text), collapse = "\n")),
      paste0("📐 [", dname, " - 변수 클래스]\n", paste(trim_lines(class_text), collapse = "\n")),
      paste0("❗ [", dname, " - 결측치 개수]\n", paste(trim_lines(na_text), collapse = "\n"))
    )
  }

  # 사용자 질문 입력
  cat("")
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
