#' API 키 암호화 함수
#'
#' @param key 암호화할 API 키 문자열
#' @param password 암호화에 사용할 비밀번호 문자열
#' @export
encrypt_api_key <- function(key, password) {
  library(sodium)

  key_raw <- charToRaw(key)
  password_raw <- charToRaw(password)

  salt <- randombytes(16)
  nonce <- randombytes(24)

  encrypted_key <- simple_encrypt(key_raw, password_raw, salt, nonce)

  # 개발환경(inst/extdata) 경로 지정
  inst_path <- file.path("inst", "extdata")
  if (!dir.exists(inst_path)) {
    dir.create(inst_path, recursive = TRUE)
  }

  saveRDS(list(data = encrypted_key, salt = salt, nonce = nonce),
          file = file.path(inst_path, "encrypted_key.bin"))

  message("✅ 암호화 완료: inst/extdata/encrypted_key.bin 에 저장되었습니다.")
}

#' API 키 복호화 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
decrypt_api_key <- function(password) {
  library(sodium)

  # 설치된 경로 확인
  encrypted_path <- system.file("extdata", "encrypted_key.bin", package = "tabieone")
  if (encrypted_path == "") {
    # 개발 중일 때 fallback
    encrypted_path <- file.path(getwd(), "inst", "extdata", "encrypted_key.bin")
  }

  if (!file.exists(encrypted_path)) {
    stop("❗ 암호화된 키 파일(encrypted_key.bin)을 찾을 수 없습니다.")
  }

  encrypted_data <- readRDS(encrypted_path)

  decrypted_key <- simple_decrypt(encrypted_data$data, charToRaw(password))

  return(rawToChar(decrypted_key))
}

#' 복호화 후 환경변수 등록 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
input <- function(password) {
  api_key <- decrypt_api_key(password)
  Sys.setenv(OPENAI_API_KEY = api_key)
  message("✅ API 키 복호화 및 환경변수 OPENAI_API_KEY에 설정 완료!")
}
