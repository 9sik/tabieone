#' API 키 복호화 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
decrypt_api_key <- function(password) {
  library(sodium)

  encrypted_path <- system.file("extdata", "encrypted_key.bin", package = "tabieone")
  if (encrypted_path == "") {        # 개발 중 경로 fallback
    encrypted_path <- file.path("inst", "extdata", "encrypted_key.bin")
  }

  cipher <- readRDS(encrypted_path)         # 암호화된 바이너리 읽기
  key    <- sha256(charToRaw(password))      # 비밀번호로 키 생성
  plain  <- data_decrypt(cipher, key)        # 복호화
  rawToChar(plain)                           # 문자로 변환 후 반환
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
