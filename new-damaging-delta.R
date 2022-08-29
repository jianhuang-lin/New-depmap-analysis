#library
library("dplyr")

#input data
hpdf <- read.csv("hpdf.csv", sep = ",", header = TRUE)

# replace na cells into 0
hpdf[is.na(hpdf)] <- 0

#create dataframe for delta, pvalue, mean
delta_result <- as.data.frame(matrix(nrow=1798, ncol =1993))
p_result <- as.data.frame(matrix(nrow=1798, ncol =1993))
mean1_result <- as.data.frame(matrix(nrow=1798, ncol =1993))
mean0_result <- as.data.frame(matrix(nrow=1798, ncol =1993))

#create a progress bar starting, max represents the parameter you wanna based on
pb <- txtProgressBar(min = 0, max = 1993, style = 3)

for (col1 in seq_len(1993)) {
  #update the progress bar based on col1
  setTxtProgressBar(pb,col1)
  
  for (col2 in 1994:3791) {
    a0 <- hpdf %>% select(colnames(hpdf[,c(col1,col2)])) %>% filter(hpdf[,col2]==0)
    a1 <- hpdf %>% select(colnames(hpdf[,c(col1,col2)])) %>% filter(hpdf[,col2]==1)
    mean0 <- mean(a0[,1])
    mean1 <- mean(a1[,1])
    mean0_result[(col2-1993),col1] = mean0
    mean1_result[(col2-1993),col1] = mean1
    delta_result[(col2-1993),col1]= mean1 - mean0
    p_result[(col2-1993),col1]=t.test(a0[,1], a1[,1], var.equal =TRUE)$p.value
  }
  
}
close(pb)
#-------------------------------------------------------------------------------


#order pearson dataset
delta_order <- order(delta_result)
#save the top and bottom 5000 indexes for further annotation
d5000index <- c(delta_order[1:5000],tail(delta_order,5000))

#convert dataframe(matrix) into list, and further unlist the columns
d1 <- unlist(as.list(delta_result))
p1 <- unlist(as.list(p_result))
mean_0 <- unlist(as.list(mean0_result))
mean_1 <- unlist(as.list(mean1_result))

#loop for top/bot 5000 pearson and corresponing pvalue

del5000 <- vector("list",10000)
p5000 <- vector("list", 10000)
mean0_5000 <- vector("list", 10000)
mean1_5000 <- vector("list", 10000)

del5000 <- d1[d5000index]
p5000 <- p1[d5000index]
mean0_5000 <- mean_0[d5000index]
mean1_5000 <- mean_1[d5000index]



# combine index, r and pvalue as a matrix and export as csv
rp_mat <- matrix(unlist(cbind(d5000index,del5000,p5000, mean0_5000, mean1_5000)), nrow=10000, ncol = 5)

#export to csv
write.table(rp_mat, "hp5000.csv", sep = ",", row.names = FALSE)
