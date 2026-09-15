student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# Part A

# convert section to a factor
section <- factor(section, levels = c("A", "B"))

section

# create a data frame
students <- data.frame(student_id, section, quiz1, quiz2, passed)

students

# create and store matrix
score_matrix <- matrix(c(quiz1, quiz2), nrow = 6, ncol = 2)

rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

score_matrix

# create the course record
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

course_record

typeof(student_id)
class(section)
length(section)
str(students)
dim(students)
dim(score_matrix)
str(course_record)


# Part B

# extract student S04's second quiz score
extracted <- score_matrix["S04", "quiz2"]

extracted

# extract rows S01 and S02
extr <- score_matrix[c("S01", "S02"), ]

extr

# extract course from course_record
course_record[c("course")]
course_record[["course"]]
course_record$course

# Single brackets keep the result as a list
# Double brackets and $ extract the value itself
# $ is convenient when you know the element's name


# Part C

# calculate each student's average
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

# add excellent column
students$excellent <- students$average >= 90

# Section A students with an average of at least 80
sectionA <- students[
  students$section == "A" & students$average >= 80,
]

# return only student_id, section, and average
sectionA <- sectionA[, c("student_id", "section", "average")]

sectionA

# create a named numeric vector
studentavgs <- students$average
names(studentavgs) <- students$student_id

studentavgs


# Part 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

# inspect the results
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# count missing values
sum(is.na(measurements$sample_id))
sum(is.na(measurements$site))
sum(is.na(measurements$temp_c))
sum(is.na(measurements$ph))
sum(is.na(measurements$status))

# keep complete observations
measurements_complete <- measurements[
  complete.cases(measurements),
]

measurements_complete

# identify sample IDs with missing values
measurements$sample_id[
  !complete.cases(measurements)
]

# x == NA is not a valid way to test for missing values
# use is.na(x) instead


# Part B - convert site and status to factors
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

# report factor levels
levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

#create phbelow 7

measurements$ph_below_7 <- measurements$ph < 7

selected <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]

#reutnr stuff

selected <- selected[c("sample_id", "site", "temp_c", "temp_f", "ph" )]

selected

mean(measurements$temp_c, na.rm = TRUE)

mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)


#PartC

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B

dim(A * B)

# 2 X 2

A %*% B

dim(A %*% B)

# 2 x 2

#A * B just mutliplies the numbers straight while A%*% multiplies the row and the columns with row and column multiplication 


#Part3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#THe output is NA, "A", "B", "B" "C"

#Part B

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id

grades

# i represents the current position in the scores vector during each loop iteration.

#Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  
  c(
    total = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    sd = round(sd(x, na.rm = na.rm), digits),
    min = round(min(x, na.rm = na.rm), digits),
    max = round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(scores)

summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#c2
plot_scores <- function(x, ...) {
  
  plot(
    seq_along(x),
    x,
    ...
  )
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

