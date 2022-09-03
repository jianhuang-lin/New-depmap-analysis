#sort the proteomics and skewness based on cell lines order before cbind the two datasets
# read each dataset
pro <- read.csv("expression_modified.csv", sep = ",", header = TRUE)
skew <- read.csv("newSkewnessfitness.csv",sep = ",", header = TRUE)

#combine the two datasets
pro_skew <- cbind(skew, pro)

# replace na cells into 0
pro_skew[is.na(pro_skew)] <- 0

#create dataframe for peason and pvalue
rresult <- as.data.frame(matrix(nrow=19160, ncol =1993))
presult <- as.data.frame(matrix(nrow=19160, ncol =1993))

#create a progress bar starting, max represents the parameter you wanna based on
pb <- txtProgressBar(min = 0, max = 1993, style = 3)

for (col1 in seq_len(1993)) {
  #update the progress bar based on col1
  setTxtProgressBar(pb,col1)
  
  for (col2 in 1994:21153) {
    r1 <- cor.test(pro_skew[,col1], pro_skew[,col2])
    rresult[(col2-1993),col1]=r1$estimate
    presult[(col2-1993),col1]=r1$p.value
  }
  
}
close(pb)



#order pearson dataset
r_order <- order(rresult)
#save the top and bottom 5000 indexes for further annotation
r5000index <- c(r_order[1:5000],tail(r_order,5000))

#convert dataframe(matrix) into list, and further unlist the columns
r1 <- unlist(as.list(rresult))
p1 <- unlist(as.list(presult))

#loop for top/bot 5000 pearson and corresponing pvalue

p5000 <- vector("list",10000)
r5000 <- vector("list", 10000)
n=0
for (index in r5000index) {
  n=n+1
  p5000[n] <- p1[index]
  r5000[n] <- r1[index]
  
}
# combine index, r and pvalue as a matrix and export as csv
rp_mat <- matrix(unlist(cbind(r5000index,r5000,p5000)), nrow=10000, ncol = 3)

#export to csv
write.table(rp_mat, "rp5000.csv", sep = ",", row.names = FALSE)
