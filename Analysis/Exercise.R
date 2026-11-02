
Credit <- c(3, 3, 2, 1, 1, 3, 3, 3, 3, 3)

Study_hours <- c(6, 6, 6, 7, 6, 6, 6, 6, 6, 5)

Cgpa <- c(3.11, 3.23, 3.32, NA, 3.22, 4.44, NA, 3.50, 3.20, 3.40)

Sclorship <- c("Yes", "NO", "NO", "Yes", NA, "NO", "Yes", "NO", "Yes", "NO")

df_student <- data.frame(Credit, Study_hours, Cgpa, Sclorship)

is.na(df_student)

colSums(is.na(df_student))

df_student$Sclorship[is.na(df_student$Sclorship)]<-names(which.max(table(df_student$Sclorship)))


df_student[is.na(df_student$Sclorship),]

max(df_student$Cgpa, na.rm = TRUE)

min(df_student$Cgpa, na.rm = TRUE)

barplot(table(df_student$Study_hours),
        main = "Study Hours",
        xlab = "Study Hours",
        ylab = "Frequency")

boxplot(df_student$Credit,
        main = "Credit",
        ylab = "Credit")

pie(table(df_student$Sclorship),
    main = "Scholarship")

boxplot(df_student$Cgpa,
        main = "CGPA Box Plot",
        ylab = "CGPA")

mean(df_student$Cgpa, na.rm = TRUE)

mean(df_student$Study_hours, na.rm = TRUE)

mean(df_student$Credit, na.rm = TRUE)


correlation <- function(x, y) {
  n <- sum((x - mean(x)) * (y - mean(y)))
  
  d <- sqrt(  sum((x - mean(x))^2) * sum((y - mean(y))^2)
  )
  
  return(n / d)
}

student_cor <- correlation(df_student$Study_hours, df_student$Cgpa)

print(student_cor)

x_1 <- df_student$Credit

x_2 <- df_student$Study_hours

y <- df_student$Cgpa

model <- lm(y ~ x_1 + x_2)

summary(model)

test <- data.frame(
  x_1 = c(12, 14),
  x_2 = c(4, 4.5)
)

predict(model, newdata = test)

print(prediction)
