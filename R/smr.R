#' 요약 출력 없이 GPT에 데이터 요약과 질문만 전송하는 smr()
#'
#' @param data c("x", "y") 형식으로 데이터 객체 이름 전달
#' @param max_lines 각 블록별 최대 출력 줄 수 제한
#' @export
smr <- function(data, max_lines = 50) {
  data_names <- as.character(substitute(data))
  if (length(data_names) == 1 && grepl("^c\\(", data_names)) {
    data_names <- gsub("^c\\(|\\)$", "", data_names)
    data_names <- trimws(unlist(strsplit(data_names, ",")))
  }

  summaries <- character()
  error_msgs <- character()

  trim_lines <- function(x) {
    if (length(x) > max_lines) c(x[1:max_lines], "...(생략됨)") else x
  }

  for (dname in data_names) {
    if (!exists(dname, envir = .GlobalEnv)) {
      summaries <- c(summaries, paste0("[", dname, " 은 존재하지 않습니다]\n"))
      error_msgs <- c(error_msgs, paste0("❌ 오류: 객체 '", dname, "' 가 존재하지 않음"))
      next
    }

    obj <- get(dname, envir = .GlobalEnv)
    obj_class <- class(obj)[1]

    block <- character()
    block <- c(block, paste0("📐 [", dname, " - 클래스] ", obj_class))

    if (is.data.frame(obj)) {
      block <- c(
        block,
        paste0("🧾 [", dname, " - head()]\n", paste(trim_lines(capture.output(head(obj, 5))), collapse = "\n")),
        paste0("📦 [", dname, " - str()]\n", paste(trim_lines(capture.output(str(obj))), collapse = "\n")),
        paste0("📊 [", dname, " - summary()]\n", paste(trim_lines(capture.output(summary(obj))), collapse = "\n")),
        paste0("🔤 [", dname, " - 변수 클래스]\n", paste(trim_lines(capture.output(sapply(obj, class))), collapse = "\n")),
        paste0("❗ [", dname, " - 결측치 개수]\n", paste(trim_lines(capture.output(colSums(is.na(obj)))), collapse = "\n"))
      )

    } else if (is.matrix(obj)) {
      block <- c(
        block,
        paste0("📏 [", dname, " - 차원] ", paste(dim(obj), collapse = " × ")),
        paste0("🧾 [", dname, " - 일부 값]\n", paste(trim_lines(capture.output(head(obj))), collapse = "\n")),
        paste0("📊 [", dname, " - summary()]\n", paste(trim_lines(capture.output(summary(obj))), collapse = "\n"))
      )

    } else if (is.array(obj)) {
      block <- c(
        block,
        paste0("📏 [", dname, " - 차원] ", paste(dim(obj), collapse = " × ")),
        paste0("📦 [", dname, " - 구조 보기]\n", paste(trim_lines(capture.output(str(obj))), collapse = "\n"))
      )

    } else if (is.list(obj)) {
      block <- c(
        block,
        paste0("🔢 [", dname, " - 길이] ", length(obj)),
        paste0("📦 [", dname, " - 구조 보기]\n", paste(trim_lines(capture.output(str(obj))), collapse = "\n")),
        paste0("🔤 [", dname, " - 구성 타입]\n", paste(trim_lines(capture.output(sapply(obj, class))), collapse = "\n"))
      )

    } else {
      block <- c(
        block,
        paste0("⚠️ [", dname, " - 비표준 객체]\n", paste(capture.output(str(obj)), collapse = "\n")),
        paste0("❌ 분석 불가: 지원되지 않는 클래스 (", obj_class, ")")
      )
      error_msgs <- c(error_msgs, paste0("❌ 오류: 객체 '", dname, "' 는 지원되지 않는 클래스 ", obj_class))
    }

    summaries <- c(summaries, paste(block, collapse = "\n"))
  }

  # 질문 입력 (출력 없이)
  lines <- character()
  repeat {
    line <- readline()
    if (line == "") break
    lines <- c(lines, line)
  }
  user_question <- paste(lines, collapse = "\n")

  auto_fix_prompt <- if (length(error_msgs) > 0) {
    paste0(
      "\n\n[🔧 자동 진단된 오류 및 개선 요청]\n",
      paste(error_msgs, collapse = "\n"),
      "\n위 오류를 바탕으로 의도한 사용을 추론하여, 적절한 R 코드 또는 수정 방안을 제시해주세요."
    )
  } else ""

  # ask() 전송
  full_input <- paste(summaries, collapse = "\n\n")
  final_prompt <- paste0(full_input, auto_fix_prompt, "\n\n[❓ 질문]\n", user_question)
  ask(final_prompt, history = 0, data = 0)
}
