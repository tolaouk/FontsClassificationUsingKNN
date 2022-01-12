This report shows how fonts can be classified by their features which are the gray scale value
extracted from a 20 x 20 pixels image. Thus, each case will have 400 features. The 
interested data is downloaded from “University of California Irvine Repository of Machine 
Learning Datasets”. There are 153 .csv files and each file contain a bunch of cases with 
some information about the case such as whether the font is bolded or presented in italic style, 
etc. The most important informations are the 400 features came from the gray scale value of 
each pixel. 4 out of 153 files are selected. Data will be cleaned and treated to
apply the k-Nearest Neighbor (kNN) Automatic Classifier. We would like to know if it is 
possible to use gray scale value of each pixel to predict the correct font. If so, what is the 
accuracy of prediction? 
Firstly, the data will be split into 20% of test set and 80% of training set randomly. Then, 
use these two sets to find out the best hyperparameter, k, in kNN model and identify the 
misclassified data and explain them. 
Second, Principal Component Analysis method will be introduced in Part 3 to reduce the 
number of features. This can help to reduce the computation time but keep the prediction 
accuracy as high as possible.
