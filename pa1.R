p_cases<-colMeans(SNP_Status[which(SNP_Status$Status == "Case"),-which(names(SNP_Status) == "Status")])/2
p_controls<-colMeans(SNP_Status[which(SNP_Status$Status == "Control"),-which(names(SNP_Status) == "Status")])/2
p_avg <- (p_cases + p_controls) / 2

assocStat <- function(p_plus, p_minus, p_average, N) {
  s1 = p_plus - p_minus
  s2 = sqrt(2 / N)
  s3 = sqrt(p_average * (1 - p_average))
  return(s1 / (s2 * s3))
}

computePValues <- function(p_plus, p_minus, p_average, N) {
  aStat = assocStat(p_plus, p_minus, p_average, N)
  pValue = pnorm(-abs(aStat))
  return (2*pValue)
}

bonferroni <- function(p_plus, p_minus, p_average, N, alpha, m) {
  pValues = computePValues(p_plus, p_minus, p_average, N)
  threshold = alpha / m / 2
  answer = ""
  
  for (i in 1:100000) {
    if(is.na(pValues[[i]] < threshold)) {
      next
    } else if(pValues[[i]] < threshold) {
      if(answer == "") {
        answer = colnames(SNP_Status)[i]
      } else {
        answer <- c(answer, colnames(SNP_Status)[i])
      }
    }
  }
  return (answer)
}

output <- function() {
  answerFile = "~/Desktop/pa1.txt"
  cat("UID:504184347",file = answerFile, append = FALSE, sep = "\n")
  cat("email:killianjackson@yahoo.com",file = answerFile, append = TRUE, sep = "\n")
  cat("Undergrad or Grad:Undergrad",file = answerFile, append = TRUE, sep = "\n")
  cat("<A>",file = answerFile, append = TRUE, sep = "\n")
  pValues = computePValues(p_cases,p_controls,p_avg,2000)
  for (i in 1:100000) {
    if(is.na(pValues[[i]])){
      line = paste(colnames(SNP_Status)[i],":","1",sep = "")
    } else {
      line = paste(colnames(SNP_Status)[i],":",pValues[[i]], sep = "")
    }
    cat(line,file = answerFile, append = TRUE, sep="\n")
  }
  cat("</A>",file = answerFile, append = TRUE, sep = "\n")
  cat("<B>",file = answerFile, append = TRUE, sep = "\n")
  bonferroniResults = bonferroni(p_cases,p_controls,p_avg,2000,0.05,100000)
  for (i in bonferroniResults) {
    cat(i,file = answerFile, append = TRUE, sep = "\n")
  }
  cat("</B>",file = answerFile, append = TRUE, sep = "\n")
}

output()