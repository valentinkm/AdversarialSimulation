library(knitr)

qmd_files <- c("LK/simulation1.qmd", "LK/simulation1b.qmd", "LK/simulation2.qmd", "LK/simulation3.qmd", "LK/simulation4.qmd", "LK/simulation4a.qmd")

for (qmd in qmd_files) {
  purl(input = qmd, output = sub(".qmd", ".R", qmd), documentation = 2)
}
