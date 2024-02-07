target "docker-metadata-action" {}

target "build" {
  inherits = ["docker-metadata-action"]
  context = "./"
  dockerfile = "Dockerfile"
  platforms = [
    "linux/amd64",
    "linux/arm64/v8"

  ]
  args = {
    JULIA_VERSION = "1.9.3"
    R_VERSION = "4.3.2"
  }
}