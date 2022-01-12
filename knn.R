rm(list = ls())
# ===========================================================================
library(dplyr)
library(class)
library(reshape2)
library(ggplot2)
library(amap)
library(caret)
library(gridExtra)
# ===========================================================================
file_wd = "/Users/jerrychien/Desktop/OneDrive - University Of Houston/6350 - Statistical Learning and Data Mining/HW/HW 2/fonts"
selected_file = c("GILL.csv", "LEELAWADEE.csv", "ROMAN.csv", "TECHNIC.csv") # Each class around 1300. Report now base on this case.
file_class = data.frame()
for (i in c(1:4)){
  file_class[paste("Class", i), "File"] = selected_file[i]
}

# Read the 4 csv files and assign to 4 different data frame and delete the unnecessary columns. 
# Filter all the 4 data frames with strength = 0.4 and italic = 0 and assign them to 4 different class.
col_to_be_skipped = c("fontVariant", "m_label", "orientation", "m_top", "m_left", "originalH", "originalW", "h", "w")
CL1 = filter(select(read.csv(paste(file_wd, "/", selected_file[1], sep = ""), skipNul = T), -col_to_be_skipped), strength == 0.4 & italic == 0)[, -c(1, 2, 3)]
CL2 = filter(select(read.csv(paste(file_wd, "/", selected_file[2], sep = ""), skipNul = T), -col_to_be_skipped), strength == 0.4 & italic == 0)[, -c(1, 2, 3)]
CL3 = filter(select(read.csv(paste(file_wd, "/", selected_file[3], sep = ""), skipNul = T), -col_to_be_skipped), strength == 0.4 & italic == 0)[, -c(1, 2, 3)]
CL4 = filter(select(read.csv(paste(file_wd, "/", selected_file[4], sep = ""), skipNul = T), -col_to_be_skipped), strength == 0.4 & italic == 0)[, -c(1, 2, 3)]
CL_list = c("CL1", "CL2", "CL3", "CL4")
# Print out the size of each class and the total size of 4 classes.
class_size = data.frame()
N = 0
for (i in CL_list){
  class_size[i, "Size"] = dim(get(i))[1]
  N = N + dim(get(i))[1]
}
class_size["Total", "Size"] = N
DATA = rbind(CL1, CL2, CL3, CL4) # Combine all the 4 classes and assign to a data frame.

# ===========================================================================
# Problem 1
# 1.1
X210_mean = data.frame() # Calculate the mean value of feature X210 for each class.
for (i in CL_list){
  X210_mean[i, "Mean"] = round(mean(get(i)[, 210]), 1)
}

t_test = data.frame() # Conduct t-test for 4 classes.
paired_list = combn(CL_list, 2)
for (i in 1:ncol(paired_list)){
  t_test[i, "1st Class"] = paired_list[, i][1]
  t_test[i, "2nd Class"] = paired_list[, i][2]
  t_test[i, "p-value"] = formatC(t.test(get(paired_list[, i][1])[, 210], get(paired_list[, i][2])[, 210])[["p.value"]], format = "e", digits = 2)
  t_test[i, "p-value < 0.1"] = t.test(get(paired_list[, i][1])[, 210], get(paired_list[, i][2])[, 210])[["p.value"]] < 0.1
}

for (i in c(1:4)){ # Plot the histogram of Feature X210 of each class.
  assign(paste("H", i, sep = ""), 
         ggplot(data = get(paste("CL", i, sep = ""))) +
           geom_histogram(aes(x = r10c9, y = ..density..), bins = 20) +
           ggtitle(paste("Histogram of CL", i, "(H", i, ")", sep = "")) +
           theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)) +
           ylab("Density") + xlab("Feature X210") + ylim(0, 0.06)
  )
}
grid.arrange(H1, H2, H3, H4, ncol = 2)

ks_test = data.frame() # Conduct KS-test for 4 classes.
for (i in 1:ncol(paired_list)){
  ks_test[i, "1st Class"] = paired_list[, i][1]
  ks_test[i, "2nd Class"] = paired_list[, i][2]
  ks_test[i, "p-value"] = formatC(ks.test(get(paired_list[, i][1])[, 210], get(paired_list[, i][2])[, 210])[["p.value"]], format = "e", digits = 2)
  ks_test[i, "p-value < 0.1"] = ks.test(get(paired_list[, i][1])[, 210], get(paired_list[, i][2])[, 210])[["p.value"]] < 0.1
}
# 1.2
CORR = cor(DATA[, 1:400]) # Correlation Values
upper_CORR = CORR * upper.tri(CORR)
melted_uCORR = melt(abs(upper_CORR))
melted_ordered_uCORR = melted_uCORR[order(melted_uCORR$value, decreasing = T), ]
top_10_corr_value = round(melted_ordered_uCORR[c(1:10), "value"], 2)
top_10_corr_index = melted_ordered_uCORR[c(1:10), c("Var1", "Var2")]
for (i in 1:10){
  top_10_corr_index[i, "Pixel1"] = gsub("r", "(", top_10_corr_index[i, "Var1"])
  top_10_corr_index[i, "Pixel2"] = gsub("r", "(", top_10_corr_index[i, "Var2"])
  top_10_corr_index[i, "Pixel1"] = gsub("c", ",", top_10_corr_index[i, "Pixel1"])
  top_10_corr_index[i, "Pixel2"] = gsub("c", ",", top_10_corr_index[i, "Pixel2"])
  top_10_corr_index[i, "Pixel1"] = paste(top_10_corr_index[i, "Pixel1"], ")", sep = "")
  top_10_corr_index[i, "Pixel2"] = paste(top_10_corr_index[i, "Pixel2"], ")", sep = "")
}

top_10_corr_index[, c("Var1", "Var2")] = NULL
top_10_corr = cbind(top_10_corr_index, "Correlation" = top_10_corr_value)
rownames(top_10_corr) = c(1:10)

corr_compare = data.frame("Pixel1" = c("(6,13)", "(6,1)"), "Pixel2" = c("(7,13)", "(6,18)"), "Correlation" = c(round(CORR["r6c13", "r7c13"], 2), round(CORR["r6c1", "r6c18"], 2)))
rm(CORR)

# 1.3
# Calculate the mean and standard deviation of each feature. Then, standardize each feature.
mean_table = data.frame()
sd_table = data.frame()
SDATA = DATA
for (j in colnames(DATA)[1:400]){
  mean_table["Mean", j] = mean(DATA[, j])
  sd_table["Standard Deviation of", j] = sd(DATA[, j])
  SDATA[, j] = (DATA[, j] - mean(DATA[, j])) / sd(DATA[, j])
}

# 1.4
# Assign class number to each case respectively.
TRUC = data.frame()
for (i in CL_list){
  temp = as.data.frame(rep(substring(i, 3), dim(get(i))[1]))
  colnames(temp) = "TRUC"
  TRUC = rbind(TRUC, temp)
}

SDATA_with_TRUC = cbind(TRUC, SDATA)

case = data.frame("Case_Number" = c(1: dim(SDATA)[1]))
final_SDATA = cbind(case, TRUC, SDATA)

# ===========================================================================
# Problem 2
# 2.1
# Split the data set into training set and test set
test_set = data.frame()
training_set = data.frame()
size_test_train = data.frame()
set.seed(20211004)
for (i in 1:4){
  temp = filter(final_SDATA, TRUC == i)
  sampled_number = sample(dim(temp)[1], dim(temp)[1] * 0.2)
  assign(paste("sampled_number_for_CL", i, sep = ""), sampled_number)
  
  temp_test = temp[sampled_number, ]
  temp_train = temp[-sampled_number, ]
  assign(paste("testCL", i, sep = ""), temp_test)
  test_set = rbind(test_set, temp_test)
  assign(paste("trainCL", i, sep = ""), temp_train)
  training_set = rbind(training_set, temp_train)
  
  size_test_train[paste("CL", i, sep = ""), "Test"] = dim(temp_test)[1]
  size_test_train[paste("CL", i, sep = ""), "Train"] = dim(temp_train)[1]
}

size_test_train["Total", "Test"] = dim(test_set)[1]
size_test_train["Total", "Train"] = dim(training_set)[1]

# 2.2
# Preliminary kNN
k = c(5, 10, 15, 20, 30, 40, 50)
preliminary_perf_table = data.frame(k , "Training_Perf" = 0, "Test_Perf" = 0)
for (i in 1:length(k)){
  temp_starting_time_training = Sys.time()
  preliminary_training_result = knn(train = training_set[, -c(1,2)], test = training_set[, -c(1,2)], cl = training_set[, 2], k = k[i])
  temp_end_time_training = Sys.time()
  temp_starting_time_test = Sys.time()
  preliminary_test_result = knn(train = training_set[, -c(1,2)], test = test_set[, -c(1,2)], cl = training_set[, 2], k = k[i])
  temp_end_time_test = Sys.time()
  preliminary_perf_table[i, "Training_Perf"] = round(sum(preliminary_training_result == training_set[, 2]) / length(preliminary_training_result), 3) * 100
  preliminary_perf_table[i, "Test_Perf"] = round(sum(preliminary_test_result == test_set[, 2]) / length(preliminary_test_result), 3) * 100
  preliminary_perf_table[i, "Training_Set_Computing_Time"] = round(temp_end_time_training - temp_starting_time_training, 1)
  preliminary_perf_table[i, "Test_Set_Computing_Time"] = round(temp_end_time_test - temp_starting_time_test, 1)
  
  temp_standard_error = sqrt((preliminary_perf_table[i, "Test_Perf"] / 100) * (1 - preliminary_perf_table[i, "Test_Perf"] / 100) / dim(test_set)[1])
  preliminary_perf_table[i, "UL_90_Test"] = round((preliminary_perf_table[i, "Test_Perf"] / 100 + 1.6 * temp_standard_error) * 100, 1)
  preliminary_perf_table[i, "LL_90_Test"] = round((preliminary_perf_table[i, "Test_Perf"] / 100 - 1.6 * temp_standard_error) * 100, 1)
  temp_standard_error = sqrt((preliminary_perf_table[i, "Training_Perf"] / 100) * (1 - preliminary_perf_table[i, "Training_Perf"] / 100) / dim(training_set)[1])
  preliminary_perf_table[i, "UL_90_Training"] = round((preliminary_perf_table[i, "Training_Perf"] / 100 + 1.6 * temp_standard_error) * 100, 1)
  preliminary_perf_table[i, "LL_90_Training"] = round((preliminary_perf_table[i, "Training_Perf"] / 100 - 1.6 * temp_standard_error) * 100, 1)
}

# Plot the k value against prediction performance.
ggplot(data = preliminary_perf_table) +
  geom_ribbon(aes(x = k, ymax = UL_90_Test, ymin = LL_90_Test, fill = "90% C.I. of Test"), alpha = 1) +
  geom_ribbon(aes(x = k, ymax = UL_90_Training, ymin = LL_90_Training, fill = "90% C.I. of Training"), alpha = 1) +
  geom_line(aes(k, Test_Perf, color = "Test")) + 
  geom_point(aes(k, Test_Perf)) +
  geom_line(aes(k, Training_Perf, color = "Training")) + 
  geom_point(aes(k, Training_Perf), color = "cyan") +
  theme(text = element_text(size = 25)) +
  geom_text(aes(k, Test_Perf, label = round(Test_Perf, 2)), hjust = 0.5, vjust = 1.5, size = 8) +
  geom_text(aes(k, Training_Perf, label = round(Training_Perf, 2)), hjust = 0.5, vjust = -1, size = 8) +
  scale_color_manual(name = "Performance", breaks = c("Test", "Training"), values = c("black", "cyan")) + 
  scale_fill_manual(name = "C.I.", values = c("90% C.I. of Test" = "yellow", "90% C.I. of Training" = "red")) +
  scale_x_discrete(limits = c(5, 10, 15, 20, 30, 40, 50)) +
  ylab("Performance (%)") + expand_limits(x = c(4,52), y = c(55, 100)) + 
  theme(legend.position = c(0.78, 0.90), legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.box = "horizontal") +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))

# 2.3
# Test k between 3 and 10
k = c(3, 4, 5, 6, 7, 8, 9, 10)
secondary_perf_table = data.frame(k , "Training_Perf" = 0, "Test_Perf" = 0)
for (i in 1:length(k)){
  temp_starting_time_training = Sys.time()
  secondary_training_result = knn(train = training_set[, -c(1,2)], test = training_set[, -c(1,2)], cl = training_set[, 2], k = k[i])
  temp_end_time_training = Sys.time()
  temp_starting_time_test = Sys.time()
  secondary_test_result = knn(train = training_set[, -c(1,2)], test = test_set[, -c(1,2)], cl = training_set[, 2], k = k[i])
  temp_end_time_test = Sys.time()
  secondary_perf_table[i, "Training_Perf"] = round(sum(secondary_training_result == training_set[, 2]) / length(secondary_training_result), 3) * 100
  secondary_perf_table[i, "Test_Perf"] = round(sum(secondary_test_result == test_set[, 2]) / length(secondary_test_result), 3) * 100
  secondary_perf_table[i, "Training_Set_Computing_Time"] = round(temp_end_time_training - temp_starting_time_training, 1)
  secondary_perf_table[i, "Test_Set_Computing_Time"] = round(temp_end_time_test - temp_starting_time_test, 1)
  
  temp_standard_error = sqrt((secondary_perf_table[i, "Test_Perf"] / 100) * (1 - secondary_perf_table[i, "Test_Perf"] / 100) / dim(test_set)[1])
  secondary_perf_table[i, "UL_90_Test"] = round((secondary_perf_table[i, "Test_Perf"] / 100 + 1.6 * temp_standard_error) * 100, 1)
  secondary_perf_table[i, "LL_90_Test"] = round((secondary_perf_table[i, "Test_Perf"] / 100 - 1.6 * temp_standard_error) * 100, 1)
  
  temp_standard_error = sqrt((secondary_perf_table[i, "Training_Perf"] / 100) * (1 - secondary_perf_table[i, "Training_Perf"] / 100) / dim(training_set)[1])
  secondary_perf_table[i, "UL_90_Training"] = round((secondary_perf_table[i, "Training_Perf"] / 100 + 1.6 * temp_standard_error) * 100, 1)
  secondary_perf_table[i, "LL_90_Training"] = round((secondary_perf_table[i, "Training_Perf"] / 100 - 1.6 * temp_standard_error) * 100, 1)
}

# Plot the k value against prediction performance.
ggplot(data = secondary_perf_table) +
  geom_ribbon(aes(x = k, ymax = UL_90_Test, ymin = LL_90_Test, fill = "90% C.I. of Test"), alpha = 1) +
  geom_ribbon(aes(x = k, ymax = UL_90_Training, ymin = LL_90_Training, fill = "90% C.I. of Training"), alpha = 1) +
  geom_line(aes(k, Test_Perf, color = "Test")) + 
  geom_point(aes(k, Test_Perf)) +
  geom_line(aes(k, Training_Perf, color = "Training")) + 
  geom_point(aes(k, Training_Perf), color = "cyan") +
  theme(text = element_text(size = 25)) +
  geom_text(aes(k, Test_Perf, label = round(Test_Perf, 2)), hjust = 0.5, vjust = 1.5, size = 8) +
  geom_text(aes(k, Training_Perf, label = round(Training_Perf, 2)), hjust = 0.5, vjust = -1, size = 8) +
  scale_color_manual(name = "Performance", breaks = c("Test", "Training"), values = c("black", "cyan")) + 
  scale_fill_manual(name = "C.I.", values = c("90% C.I. of Test" = "yellow", "90% C.I. of Training" = "red")) +
  scale_x_discrete(limits = c(3, 4, 5, 6, 7, 8, 9, 10)) +
  ylab("Performance (%)") + expand_limits(x = c(3,10), y = c(55, 100)) + 
  theme(legend.position = c(0.78, 0.90), legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.box = "horizontal") +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))
best_k = filter(secondary_perf_table, Test_Perf == max(secondary_perf_table$Test_Perf))[, "k"]

# Test on best k.
training_result = knn(train = training_set[, -c(1,2)], test = training_set[, -c(1,2)], cl = training_set[, 2], k = best_k)
start_time = Sys.time()
test_result = knn(train = training_set[, -c(1,2)], test = test_set[, -c(1,2)], cl = training_set[, 2], k = best_k)
end_time = Sys.time()
computing_time = end_time - start_time

# Compute the 90% Confidence Interval of Test Performance
test_perf = sum(test_result == test_set[, 2]) / length(test_result)
best_k_Standard_Error = sqrt(test_perf * (1 - test_perf) / length(test_result))
best_k_CI_UL = test_perf + 1.6 * best_k_Standard_Error
best_k_CI_LL = test_perf - 1.6 * best_k_Standard_Error

best_k_test_perf = data.frame()
best_k_test_perf["Best k", "Computing_Time"] = round(computing_time, 1)
best_k_test_perf["Best k", "Test_Perform (%)"] = round(test_perf, 3) * 100
best_k_test_perf["Best k", "90% C.I."] = paste("[", round(best_k_CI_LL, 3) * 100, ",", round(best_k_CI_UL, 3) * 100, "]")

# 2.4
# Compute the confusion matrix
confusion_matrix_test = round(prop.table(confusionMatrix(test_result, as.factor(test_set[, 2]))$table, margin = 2)* 100, 1)
colnames(confusion_matrix_test) = c("True CL1", "True CL2", "True CL3", "True CL4")
rownames(confusion_matrix_test) = c("Pred CL1", "Pred CL2", "Pred CL3", "Pred CL4")
CI_for_each_case = data.frame()
for (i in 1:4){
  Standard_Error = sqrt(confusion_matrix_test[i, i] / 100 * (1 - confusion_matrix_test[i, i] / 100) / dim(filter(test_set, TRUC == i))[1])
  CI_for_each_case[i, "CL"] = i
  CI_for_each_case[i, "CI_LL"] = round(confusion_matrix_test[i, i] / 100 - 1.6 * Standard_Error, 3) * 100
  CI_for_each_case[i, "Test_Performance"] = round(confusion_matrix_test[i, i] / 100, 3) * 100
  CI_for_each_case[i, "CI_UL"] = round(confusion_matrix_test[i, i] / 100 + 1.6 * Standard_Error, 3) * 100
}

ggplot(data = CI_for_each_case, aes(x = Test_Performance, y = CL)) +
  geom_errorbar(aes(xmin = CI_LL, xmax = CI_UL), width = 0.15, color = "blue", size = 2) +
  geom_point(size = 3) +
  geom_text(aes(x = Test_Performance, y = CL - 0.2, label = Test_Performance), size = 7.5) +
  geom_text(aes(x = CI_LL, y = CL + 0.2, label = CI_LL), size = 7.5) +
  geom_text(aes(x = CI_UL, y = CL + 0.2, label = CI_UL), size = 7.5) +
  xlab("Test Performance (%)") + ylab("Class") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 25)) +
  xlim(50, 100)

# 2.5
# Error List
ERR21 = filter(cbind(test_result, test_set[, c(1, 2)]), TRUC == 2 & test_result == 1)
ERR23 = filter(cbind(test_result, test_set[, c(1, 2)]), TRUC == 2 & test_result == 3)
ERR24 = filter(cbind(test_result, test_set[, c(1, 2)]), TRUC == 2 & test_result == 4)

dist_matrix = as.matrix(Dist(final_SDATA[, -c(1,2)]))
melted_dist_matrix = reshape2::melt(dist_matrix)

if (dim(ERR21)[1] == 0){
  ERR21_case = NULL
} else {
  ERR21_case = ERR21[1, "Case_Number"]
  melted_dist_matrix_ERR21_case = filter(melted_dist_matrix, Var1 == ERR21_case & Var2 != ERR21_case)
  ordered_melted_dist_matrix_ERR21_case = melted_dist_matrix_ERR21_case[order(melted_dist_matrix_ERR21_case$value, decreasing = F), ]
  ERR21_case_neighbor = data.frame()
  for (i in c(1:best_k)){
    ERR21_case_neighbor[paste("Neighbor", i), "Case #"] = ordered_melted_dist_matrix_ERR21_case[i, "Var2"]
    ERR21_case_neighbor[paste("Neighbor", i), "Distance"] = filter(ordered_melted_dist_matrix_ERR21_case, Var2 == ordered_melted_dist_matrix_ERR21_case[i, "Var2"])["value"]
    ERR21_case_neighbor[paste("Neighbor", i), "TRUC"] = filter(final_SDATA, Case_Number == ordered_melted_dist_matrix_ERR21_case[i, "Var2"])["TRUC"]
  }
}

if (dim(ERR23)[1] == 0){
  ERR23_case = NULL
} else {
  ERR23_case = ERR23[1, "Case_Number"]
  melted_dist_matrix_ERR23_case = filter(melted_dist_matrix, Var1 == ERR23_case & Var2 != ERR23_case)
  ordered_melted_dist_matrix_ERR23_case = melted_dist_matrix_ERR23_case[order(melted_dist_matrix_ERR23_case$value, decreasing = F), ]
  ERR23_case_neighbor = data.frame()
  for (i in c(1:best_k)){
    ERR23_case_neighbor[paste("Neighbor", i), "Case #"] = ordered_melted_dist_matrix_ERR23_case[i, "Var2"]
    ERR23_case_neighbor[paste("Neighbor", i), "Distance"] = filter(ordered_melted_dist_matrix_ERR23_case, Var2 == ordered_melted_dist_matrix_ERR23_case[i, "Var2"])["value"]
    ERR23_case_neighbor[paste("Neighbor", i), "TRUC"] = filter(final_SDATA, Case_Number == ordered_melted_dist_matrix_ERR23_case[i, "Var2"])["TRUC"]
  }
}

if (dim(ERR24)[1] == 0){
  ERR23_case = NULL
} else {
  ERR24_case = ERR24[1, "Case_Number"]
  melted_dist_matrix_ERR24_case = filter(melted_dist_matrix, Var1 == ERR24_case & Var2 != ERR24_case)
  ordered_melted_dist_matrix_ERR24_case = melted_dist_matrix_ERR24_case[order(melted_dist_matrix_ERR24_case$value, decreasing = F), ]
  ERR24_case_neighbor = data.frame()
  for (i in c(1:4)){
    ERR24_case_neighbor[paste("Neighbor", i), "Case #"] = ordered_melted_dist_matrix_ERR24_case[i, "Var2"]
    ERR24_case_neighbor[paste("Neighbor", i), "Distance"] = filter(ordered_melted_dist_matrix_ERR24_case, Var2 == ordered_melted_dist_matrix_ERR24_case[i, "Var2"])["value"]
    ERR24_case_neighbor[paste("Neighbor", i), "TRUC"] = filter(final_SDATA, Case_Number == ordered_melted_dist_matrix_ERR24_case[i, "Var2"])["TRUC"]
  }
}

# ===========================================================================
# Problem 3
# 3.1
# Eigenvalue and Eigenvector
SCORR = cor(SDATA)
W = eigen(SCORR)$vector
L = eigen(SCORR)$value
write.csv(W, file = "W.csv")
ggplot() +
  geom_point(aes(x = c(1:400), y = L)) +
  ylab("L(eigenvalue)") + xlab("m") +
  scale_y_continuous(expand = c(0, 1)) + scale_x_continuous(expand = c(0, 1)) +
  theme(text = element_text(size = 25), plot.margin = margin(10, 15, 10, 10, "point"))
rm(SCORR)

# 3.2
# PEV
PEV = NULL
for (m in 1:400){
  PEV[m] = sum(L[c(1:m)]) / 400 * 100
}
smallest_r = sum(PEV < 90) + 1
ggplot() +
  geom_point(aes(x = c(1:400), y = PEV)) +
  geom_segment(aes(x = smallest_r, xend = smallest_r, y = 0, yend = 90), size = 1, colour="blue") +
  geom_segment(aes(x = 0, xend = smallest_r, y = 90, yend = 90), size = 1, colour="blue") +
  geom_text(aes(x = smallest_r + 17, y = 3, label = paste("m =", smallest_r)), size = 7, color = "blue") +
  geom_text(aes(x = 24, y = 93, label = "PEV = 90%"), size = 7, color = "blue") +
  ylab("PEV (%)") + xlab("m") +
  scale_y_continuous(expand = c(0, 1)) + scale_x_continuous(expand = c(0, 2)) +
  theme(text = element_text(size = 25), plot.margin = margin(10, 15, 10, 10, "point"))

# 3.3
# PCA Data Set
ZDATA = as.data.frame(as.matrix(SDATA) %*% as.matrix(W))
ZDATA_with_TRUC = cbind(TRUC, ZDATA[, c(1:smallest_r)])
final_ZDATA = cbind(case, TRUC, ZDATA[, c(1:smallest_r)])

# 3.4
# kNN on PCA data set
PCA_test_set = final_ZDATA[test_set[, "Case_Number"], ]
PCA_training_set = final_ZDATA[training_set[, "Case_Number"], ]

start_time <- Sys.time()
PCA_training_result = knn(train = PCA_training_set[, -c(1,2)], test = PCA_training_set[, -c(1,2)], cl = PCA_training_set[, 2], k = best_k)
PCA_test_result = knn(train = PCA_training_set[, -c(1,2)], test = PCA_test_set[, -c(1,2)], cl = PCA_training_set[, 2], k = best_k)
end_time <- Sys.time()
new_computing_time = end_time - start_time

new_test_perf = (sum(PCA_test_result == PCA_test_set[, 2]) / length(PCA_test_result))

PCA_confusion_matrix_test = round(prop.table(confusionMatrix(PCA_test_result, as.factor(PCA_test_set[, 2]))$table, margin = 2)* 100, 1)
rownames(PCA_confusion_matrix_test) = c("True CL1", "True CL2", "True CL3", "True CL4")
colnames(PCA_confusion_matrix_test) = c("Pred CL1", "Pred CL2", "Pred CL3", "Pred CL4")
for (i in 1:4){
  PCA_Standard_Error = sqrt(PCA_confusion_matrix_test[i, i] / 100 * (1 - PCA_confusion_matrix_test[i, i] / 100) / dim(filter(PCA_test_set, TRUC == i))[1])
  PCA_CI_UL = PCA_confusion_matrix_test[i, i] / 100 + 1.6 * PCA_Standard_Error
  PCA_CI_LL = PCA_confusion_matrix_test[i, i] / 100 - 1.6 * PCA_Standard_Error
}

PCA_Standard_Error = sqrt(new_test_perf * (1 - new_test_perf) / length(PCA_test_result))

compare_before_and_after_PCA = data.frame("Data_Set" = c("Original", "PCA"))
compare_before_and_after_PCA[1, "Computing_Time"] = round(computing_time, 1)
compare_before_and_after_PCA[2, "Computing_Time"] = round(new_computing_time, 1)
compare_before_and_after_PCA[1, "Test_Performance"] = round(test_perf * 100, 1)
compare_before_and_after_PCA[2, "Test_Performance"] = round(new_test_perf * 100, 1)
compare_before_and_after_PCA[1, "UL"] = round(best_k_CI_UL, 3) * 100
compare_before_and_after_PCA[1, "LL"] = round(best_k_CI_LL, 3) * 100
compare_before_and_after_PCA[2, "UL"] = round(new_test_perf + 1.6 * PCA_Standard_Error, 3) * 100
compare_before_and_after_PCA[2, "LL"] = round(new_test_perf - 1.6 * PCA_Standard_Error, 3) * 100

ggplot(data = compare_before_and_after_PCA, aes(x = Test_Performance, y = Data_Set)) +
  geom_errorbar(aes(xmin = LL, xmax = UL), width = 0.1, color = "blue", size = 2) +
  geom_point(size = 3) +
  geom_text(aes(x = Test_Performance, y = c(0.9, 1.9), label = Test_Performance), size = 7.5) +
  geom_text(aes(x = LL, y = c(1.1, 2.1), label = LL), size = 7.5) +
  geom_text(aes(x = UL, y = c(1.1, 2.1), label = UL), size = 7.5) +
  xlab("Test Performance (%)") + ylab("Data Set") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 25), plot.margin = margin(10, 10, 10, 10, "point")) +
  xlim(60, 90)

# 3.5
# Plot each class on 2D.
CL24 = filter(final_ZDATA, TRUC == 2 | TRUC == 4)
CL23 = filter(final_ZDATA, TRUC == 2 | TRUC == 3)
CL21 = filter(final_ZDATA, TRUC == 2 | TRUC == 1)
ggplot() + 
  geom_point(data = CL24, aes(x = V1, y = V2, color = TRUC)) + 
  scale_color_manual(values = c("2" = "black", "4" = "green")) + 
  xlim(-20, 30) + ylim(-20, 20) + 
  theme(text = element_text(size = 25), legend.position = c(0.935, 0.85), legend.text = element_text(size = 25), legend.title = element_text(size = 25))
ggplot() + 
  geom_point(data = CL23, aes(x = V1, y = V2, color = TRUC)) + 
  scale_color_manual(values = c("2" = "black", "3" = "red")) + 
  xlim(-20, 30) + ylim(-20, 20) + 
  theme(text = element_text(size = 25), legend.position = c(0.935, 0.85), legend.text = element_text(size = 25), legend.title = element_text(size = 25))
ggplot() + 
  geom_point(data = CL21, aes(x = V1, y = V2, color = TRUC)) + 
  scale_color_manual(values = c("2" = "black", "1" = "blue")) + 
  xlim(-20, 30) + ylim(-20, 20) + 
  theme(text = element_text(size = 25), legend.position = c(0.935, 0.85), legend.text = element_text(size = 25), legend.title = element_text(size = 25))

# 3.6
# Plot class 2 and its misclassified cases on 2D.
PCA_CL2 = filter(final_ZDATA, TRUC == 2)

ZERR21_case = filter(cbind(PCA_test_result, PCA_test_set[, c(1, 2)]), TRUC == 2 & PCA_test_result == 1)
ZERR23_case = filter(cbind(PCA_test_result, PCA_test_set[, c(1, 2)]), TRUC == 2 & PCA_test_result == 3)
ZERR24_case = filter(cbind(PCA_test_result, PCA_test_set[, c(1, 2)]), TRUC == 2 & PCA_test_result == 4)
ZERR21 = final_ZDATA[ZERR21_case$Case_Number, ]
ZERR23 = final_ZDATA[ZERR23_case$Case_Number, ]
ZERR24 = final_ZDATA[ZERR24_case$Case_Number, ]

ggplot() + 
  geom_point(data = PCA_CL2, aes(x = V1, y = V2, color = "CL2")) + 
  geom_point(data = ZERR21, aes(x = V1, y = V2, color = "ERR21")) + 
  geom_point(data = ZERR23, aes(x = V1, y = V2, color = "ERR23")) + 
  geom_point(data = ZERR24, aes(x = V1, y = V2, color = "ERR24")) +
  scale_color_manual(name = "Data Point", values = c("CL2" = "black", "ERR21" = "blue", "ERR23" = "red", "ERR24" = "green")) +
  xlim(-20, 30) + ylim(-20, 20) + 
  theme(text = element_text(size = 25), legend.position = c(0.91, 0.8), legend.text = element_text(size = 25), legend.title = element_text(size = 25))

