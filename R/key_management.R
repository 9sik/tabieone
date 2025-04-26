#' API 키 암호화 함수
#'
#' @param key API 키 문자열
#' @param password 비밀번호 문자열
#' @export
encrypt_api_key <- function(key, password) {
  library(sodium)
  key_raw <- charToRaw(key)
  password_raw <- sha256(charToRaw(password))
  encrypted <- data_encrypt(key_raw, password_raw)

  # inst/extdata 폴더에 encrypted_key.bin 저장
  if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

  saveRDS(
    encrypted,
    file = file.path("inst/extdata", "encrypted_key.bin")
  )

  message("✅ API 키 암호화 및 저장 완료: inst/extdata/encrypted_key.bin")
}

#' API 키 복호화 및 환경변수 설정 함수
#'
#' @param password 비밀번호 문자열
#' @export
