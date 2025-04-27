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

  if (!dir.exists(file.path("inst", "extdata"))) {
    dir.create(file.path("inst", "extdata"), recursive = TRUE)
  }

  saveRDS(encrypted_key, file = file.path("inst", "extdata", "encrypted_key.bin"))

  return(encrypted_key)
}

#' API 키 복호화 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
decrypt_api_key <- function(password) {
  library(sodium)

  encrypted_key <- readRDS(file.path("inst", "extdata", "encrypted_key.bin"))

  decrypted_key <- simple_decrypt(encrypted_key, charToRaw(password))

  return(rawToChar(decrypted_key))
}

#' 복호화 후 환경변수 등록 함수
#'
#' @param password 복호화에 사용할 비밀번호 문자열
#' @export
input <- function(password) {
  library(sodium)

  encrypted_key <- readRDS(file.path("inst", "extdata", "encrypted_key.bin"))

  decrypted_key <- simple_decrypt(encrypted_key, charToRaw(password))

  Sys.setenv(OPENAI_API_KEY = rawToChar(decrypted_key))

  message("✅ API 키 복호화 완료 및 OPENAI_API_KEY 환경변수에 설정 완료!")
}
