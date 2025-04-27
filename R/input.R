#' API 키 복호화 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
decrypt_api_key <- function(password) {
  library(sodium)

  # 설치된 경로에서 파일 읽기 시도
  encrypted_path <- system.file("extdata", "encrypted_key.bin", package = "tabieone")
  if (encrypted_path == "") {
    # 설치된 패키지 경로에 없으면 개발 중 경로 사용
    encrypted_path <- file.path("inst", "extdata", "encrypted_key.bin")
  }

  encrypted_data <- readRDS(encrypted_path)

  encrypted_key <- encrypted_data$encrypted_key
  salt <- encrypted_data$salt
  nonce <- encrypted_data$nonce

  password_raw <- charToRaw(password)

  decrypted_key <- simple_decrypt(encrypted_key, password_raw, salt, nonce)

  return(rawToChar(decrypted_key))
}

#' 복호화 후 환경변수 등록 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
input <- function(password) {
  api_key <- decrypt_api_key(password)
  Sys.setenv(OPENAI_API_KEY = api_key)
  message("✅ API 키 복호화 완료 및 OPENAI_API_KEY 환경변수에 설정 완료!")
}
