print.bart_spher <- function(x, ...){
  if(interactive()) writeLines("")
  writeLines("\tBartlett's Test of Sphericity\n")
  writeLines(paste0("Call: ", deparse(x$call), "\n"))
  colns <- format(c("X2","df","p-value"), justify = "right")
  writeLines(paste0(colns[1L], " = ", round(x$X2, 3L)))
  writeLines(paste0(colns[2L], " = ", round(x$df, 0L)))
  pv <- round(x$p.value, 5L)
  if(pv == 0.0){
    pv <- "< .00001"
  } else {
    pv <- gsub("0\\.", "\\= \\.", formatC(pv, format="f", digits=5))
  }
  writeLines(paste0(colns[3L], " ", pv))
  if(interactive()) writeLines("")
  if(x$warn){
    warning(paste0("Used n = ", round(x$n,2L), "."), call. = FALSE, immediate. = TRUE)
  }
}
