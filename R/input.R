input <- function(password) {
  library(sodium)
  encrypted <- readRDS(file.path("inst/extdata", "encrypted_key.bin"))
  password_raw <- sha256(charToRaw(password))
  decrypted_raw <- data_decrypt(encrypted, password_raw)
  api_key <- rawToChar(decrypted_raw)

  Sys.setenv(OPENAI_API_KEY = api_key)
  message("✅ API 키 복호화 및 환경변수 설정 완료 (Sys.getenv('OPENAI_API_KEY'))")
}
