# Assignment 1
# Hui Lyu
# 1.48 sketch a box plot
score = c(57, 66, 69, 71, 72, 73, 74, 77, 78, 78, 79, 79, 81, 81, 82, 83, 83, 88, 89, 94)
framescore = data.frame(studentscore=score)
boxplot(framescore, ylab="scores")
