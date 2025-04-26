# R/input.R

#' API 키 복호화 및 환경변수 설정 함수
#'
#' @param password 비밀번호 문자열
#' @export
input <- function(password) {
  # sodium 패키지 로드
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("패키지 'sodium'이 설치되어 있지 않습니다. install.packages('sodium') 해주세요.")
  }
  # 설치된 라이브러리 경로에서 encrypted_key.bin 찾기
  encrypted_path <- system.file("extdata", "encrypted_key.bin", package = "tabieone")
  if (encrypted_path == "") {
    stop("❌ encrypted_key.bin 파일을 찾을 수 없습니다. encrypt_api_key()를 먼저 실행하세요.")
  }
  # 파일 읽고 복호화
  encrypted <- readRDS(encrypted_path)
  key_raw    <- sodium::sha256(charToRaw(password))
  decrypted  <- sodium::data_decrypt(encrypted, key_raw)
  api_key    <- rawToChar(decrypted)

  # 환경변수에 저장
  Sys.setenv(OPENAI_API_KEY = api_key)
  message("✅ API 키 복호화 및 환경변수 설정 완료")
}
