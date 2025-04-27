#' API 키 암호화 함수
#'
#' @param key 암호화할 API 키 문자열
#' @param password 암호화에 사용할 비밀번호 문자열
#' @export
encrypt_api_key <- function(key, password) {
  library(sodium)

  key_raw <- charToRaw(key)
  password_raw <- charToRaw(password)

  # salt와 nonce 생성
  salt <- randombytes(16)
  nonce <- randombytes(24)

  # 키 암호화
  encrypted_key <- simple_encrypt(key_raw, password_raw, salt, nonce)

  # inst/extdata 폴더가 없으면 생성
  if (!dir.exists(file.path("inst", "extdata"))) {
    dir.create(file.path("inst", "extdata"), recursive = TRUE)
  }

  # salt, nonce도 함께 저장 (복호화에 필요)
  saveRDS(list(
    encrypted_key = encrypted_key,
    salt = salt,
    nonce = nonce
  ), file = file.path("inst", "extdata", "encrypted_key.bin"))

  message("✅ API 키가 성공적으로 암호화되어 저장되었습니다.")
}

#' API 키 복호화 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
decrypt_api_key <- function(password) {
  library(sodium)

  # 저장된 데이터 읽기
  encrypted_data <- readRDS(file.path("inst", "extdata", "encrypted_key.bin"))

  encrypted_key <- encrypted_data$encrypted_key
  salt <- encrypted_data$salt
  nonce <- encrypted_data$nonce

  password_raw <- charToRaw(password)

  # 복호화
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
