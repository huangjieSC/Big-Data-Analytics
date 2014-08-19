
# Name: Jie Huang
# Exercise 1: Valid Sudokus
# Method: 
# 1. Check each row/column and see if each digit in the range of 1-9 exists only once in it(by checking sum and using bitwise XOR)
# 2. Also check each 3x3 cell in the 9x9 matrix
# Main function: validSudokus()
# Helper function: checkS()
# Validation results: In total 7 valid sudokus, index: 1, 13, 16, 19, 23, 28, 37. See more details in the following code.

> checkS=function(x){
+     m=sudokus[,,x] 
+     for(r in 1:9){
+         n=m[r, 1] 
+         for(s in 2:9){
+             n=bitwXor(n,m[r,s])                                                                                    
+         }
+         for(t in 1:9){
+             n=bitwXor(n,t)
+         }
+         if(n){return("invalid row")}
+         if(!sum(m[r,])==45){return("invalid row")}
+     }
+     for(c in 1:9){
+         n1=m[1, c] 
+         for(s1 in 2:9){
+             n1=bitwXor(n1,m[s1,c])
+         }
+         for(t1 in 1:9){
+             n1=bitwXor(n1,t1)
+         }
+         if(n1){return("invalid col")}
+         if(!sum(m[,c])==45){return("invalid col")}
+     } 
+     for(i in 0:2){
+         for(j in 0:2){
+                 colS=j*3
+                 colS=colS+1
+                 colE=j*3
+                 colE=colE+3
+                 rowS=i*3
+                 rowS=rowS+1
+                 rowE=i*3
+                 rowE=rowE+3
+                 if(!sum(m[rowS:rowE, colS:colE])==45){return("invalid 3x3 cell")}
+         }
+     }
+     cat("index of valid sudoku is ", x, "\n")    
+     return(1)
+ }
> 
> validSudokus=function(){
+     for(s in 1:50){
+         checkS(s)
+     }
+ }
> 
> validSudokus()
index of valid sudoku is  1 
index of valid sudoku is  13 
index of valid sudoku is  16 
index of valid sudoku is  19 
index of valid sudoku is  23 
index of valid sudoku is  28 
index of valid sudoku is  37 
> 

# Exercise 2: Expand data frame.
# Please refer to the following code for more details


> myScores=scores
> myScores
   Name     id Quiz1 Exam1 Exam2 Quiz2 Exam3
1 Susan 123412    50    47    33    67    79
2  John 548963    38    61    75    59    65
3   Bob 234563    89    97    85    88    92
4  Bill 429591    72    73    74    75    76
5  Mary 245887    92    95    79    89    90
6  Paul  97522    99     3    55    60    72
> attach(myScores)
> 
> myScores
   Name     id Quiz1 Exam1 Exam2 Quiz2 Exam3
1 Susan 123412    50    47    33    67    79
2  John 548963    38    61    75    59    65
3   Bob 234563    89    97    85    88    92
4  Bill 429591    72    73    74    75    76
5  Mary 245887    92    95    79    89    90
6  Paul  97522    99     3    55    60    72
> t1<-data.frame(Name=c("Average"), id=c(0), Quiz1=c(mean(Quiz1)), Exam1=c(mean(Exam1)), Exam2=c(mean(Exam2)), Quiz2=c(mean(Quiz2)), Exam3=c(mean(Exam)))
> 
> t1
     Name id    Quiz1    Exam1    Exam2 Quiz2    Exam3
1 Average  0 73.33333 62.66667 66.83333    73 73.16667
> myScores=rbind(myScores, t1)
> 
> myScores
     Name     id    Quiz1    Exam1    Exam2 Quiz2    Exam3
1   Susan 123412 50.00000 47.00000 33.00000    67 79.00000
2    John 548963 38.00000 61.00000 75.00000    59 65.00000
3     Bob 234563 89.00000 97.00000 85.00000    88 92.00000
4    Bill 429591 72.00000 73.00000 74.00000    75 76.00000
5    Mary 245887 92.00000 95.00000 79.00000    89 90.00000
6    Paul  97522 99.00000  3.00000 55.00000    60 72.00000
7 Average      0 73.33333 62.66667 66.83333    73 73.16667
> t2<-data.frame(Name=c("Standard Deviation"), id=c(0), Quiz1=c(sd(Quiz1)), Exam1=c(sd(Exam1)), Exam2=c(sd(Exam2)), Quiz2=c(sd(Quiz2)), Exam3=c(sd(Exam3)))
> 
> myScores=rbind(myScores, t2)
> 
> myScores
                Name     id    Quiz1    Exam1    Exam2    Quiz2    Exam3
1              Susan 123412 50.00000 47.00000 33.00000 67.00000 79.00000
2               John 548963 38.00000 61.00000 75.00000 59.00000 65.00000
3                Bob 234563 89.00000 97.00000 85.00000 88.00000 92.00000
4               Bill 429591 72.00000 73.00000 74.00000 75.00000 76.00000
5               Mary 245887 92.00000 95.00000 79.00000 89.00000 90.00000
6               Paul  97522 99.00000  3.00000 55.00000 60.00000 72.00000
7            Average      0 73.33333 62.66667 66.83333 73.00000 73.16667
8 Standard Deviation      0 24.68738 35.04093 19.39502 13.31165 10.43072
> View(myScores)
> myScores$Quiz<-c(rowMeans(myScores[,c(3,6)]))
>
> myScores$Exam<-c(rowMeans(myScores[,c(4,5,7)]))
> 
> myScores$Overall_Average<-c(rowMeans(myScores[,c(3:7)]))
> 
> myScores
                Name     id    Quiz1    Exam1    Exam2    Quiz2    Exam3     Quiz     Exam Overall_Average
1              Susan 123412 50.00000 47.00000 33.00000 67.00000 79.00000 58.50000 53.00000        55.20000
2               John 548963 38.00000 61.00000 75.00000 59.00000 65.00000 48.50000 67.00000        59.60000
3                Bob 234563 89.00000 97.00000 85.00000 88.00000 92.00000 88.50000 91.33333        90.20000
4               Bill 429591 72.00000 73.00000 74.00000 75.00000 76.00000 73.50000 74.33333        74.00000
5               Mary 245887 92.00000 95.00000 79.00000 89.00000 90.00000 90.50000 88.00000        89.00000
6               Paul  97522 99.00000  3.00000 55.00000 60.00000 72.00000 79.50000 43.33333        57.80000
7            Average      0 73.33333 62.66667 66.83333 73.00000 73.16667 73.16667 67.55556        69.80000
8 Standard Deviation      0 24.68738 35.04093 19.39502 13.31165 10.43072 18.99951 21.62222        20.57314
> View(myScores)