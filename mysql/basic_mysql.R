
library(RMySQL)
library(ggplot2)

db <- dbConnect(MySQL(), user='root', dbname='sampdb', host='localhost')
tables <- dbListTables(db)
print(tables)
for(i in tables) {
    print(i)
    print(paste("  ", dbListFields(db,i)))
}

query <- "
SELECT
  student.sex
  , score.score
FROM
  student student
JOIN
  score score
ON
  student.student_id = score.student_id
;"

student_sex_scores <- dbGetQuery(db, query)
str(student_sex_scores)
student_sex_scores$sex <- as.factor(student_sex_scores$sex)
summary(student_sex_scores)
plot(student_sex_scores)
dbDisconnect(db)


