s <- readLines("./src/js/class-shidashi.js")
idx <- which(startsWith(trimws(s), "// Insert build version here")) + 1
s[idx] <- sprintf("    this.build = { version: '%.1f', date: '%s' };", 1.0, strftime(Sys.time(), usetz = TRUE))
writeLines(s, "./src/js/class-shidashi.js")
system("npx webpack")
