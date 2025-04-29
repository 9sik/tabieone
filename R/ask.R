#' GPT 질문 함수 (완전체 버전)
#'
#' @param question 질문 문자열
#' @param history 콘솔 기록 첨부 여부 (0/1)
#' @param data Environment 개괄정보 첨부 여부 (0/1)
#' @export
ask <- function(question, history = 1, data = 1) {
  library(httr)
  library(jsonlite)

  # 최근 콘솔 입력 저장
  console_text <- ""
  if (history == 1) {
    history_file <- tempfile()
    savehistory(history_file)
    console_lines <- tryCatch(
      tail(readLines(history_file, warn = FALSE), 10),
      error = function(e) character(0)
    )
    console_text <- if (length(console_lines) > 0) {
      paste("[🧾 최근 콘솔 입력 10줄]\n", paste(console_lines, collapse = "\n"))
    } else {
      ""
    }
  }

  # Environment 개괄 정보 저장
  env_text <- ""
  if (data == 1) {
    env_summary <- character()
    for (obj_name in ls(envir = .GlobalEnv)) {
      obj <- get(obj_name, envir = .GlobalEnv)
      summary_text <- tryCatch({
        if (is.data.frame(obj)) {
          paste0("data.frame, dim=", paste(dim(obj), collapse = "x"))
        } else if (is.matrix(obj)) {
          paste0("matrix, dim=", paste(dim(obj), collapse = "x"))
        } else if (is.list(obj)) {
          paste0("list, length=", length(obj))
        } else if (is.vector(obj) && !is.list(obj) && !is.matrix(obj)) {
          paste0("vector, length=", length(obj))
        } else if (is.numeric(obj) || is.character(obj) || is.logical(obj)) {
          paste0("scalar, value=", toString(obj))
        } else if (is.function(obj)) {
          "function"
        } else if (is.environment(obj)) {
          "environment"
        } else {
          paste("other type:", class(obj)[1])
        }
      }, error = function(e) {
        "unreadable object"
      })

      env_summary <- c(env_summary, paste0("▶ ", obj_name, ": ", summary_text))
    }
    if (length(env_summary) > 0) {
      env_text <- paste("\n\n[📦 Environment 요약정보]\n", paste(env_summary, collapse = "\n"))
    }
  }

  # 최종 프롬프트 만들기
  full_prompt <- paste0(
    console_text,
    env_text,
    "\n\n[🧠 질문]\n",
    question
  )

  # GPT API 호출
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o",
      messages = list(
        list(role = "system", content = "You are helping with R coding.

When users ask about R code:
- Answer step-by-step from the top down.
- Use short, simple English.
- Write clean and concise R code.
- Assume that `ask()`, `ask1()`, and `input()` are normal functions already created by the user.

When users ask natural language questions:
- Answer briefly with simple English.
- Use short bullet points when possible.

Do not question or correct the existence of ask(), ask1(), or input() functions.
Treat them like normal R functions.
"),
        list(role = "user", content = full_prompt)
      )
    )
  )

  # 응답 출력
  result <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  cat(result$choices[[1]]$message$content, "\n")
}
