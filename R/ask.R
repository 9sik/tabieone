#' GPT 질문 함수 (이제 heip 이름으로)
#'
#' @param question 질문 문자열
#' @param history 콘솔 기록 첨부 여부 (0/1)
#' @param data 데이터프레임 첨부 여부 (0/1)
#' @export
ask <- function(question, history = 1, data = 1) {
  library(httr)
  library(jsonlite)

  console_text <- ""
  if (history == 1) {
    history_file <- tempfile()
    savehistory(history_file)
    console_text <- paste0(tail(readLines(history_file, warn = FALSE), 10), collapse = "\n")
    console_text <- paste("[🧾 최근 콘솔 입력 10줄]\n", console_text)
  }

  df_text <- ""
  if (data == 1) {
    for (df_name in ls(envir = .GlobalEnv)) {
      obj <- get(df_name, envir = .GlobalEnv)
      if (is.data.frame(obj)) {
        df_head <- capture.output(head(obj, 3))
        df_text <- paste0(df_text, "\n▶ ", df_name, "\n", paste(df_head, collapse = "\n"))
      }
    }
    df_text <- paste("\n\n[📊 데이터프레임 head(3)]\n", df_text)
  }

  full_prompt <- paste0(
    console_text,
    df_text,
    "\n\n[🧠 질문]\n",
    question
  )

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

  result <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  cat(result$choices[[1]]$message$content, "\n")
}
