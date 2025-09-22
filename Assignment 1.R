#Q.1
set.seed(1)  # For reproducibility
v <- sample(-100:100, 25, replace=TRUE)
neg_idx <- which(v < 0)
neg_values <- v[neg_idx]
data.frame(Position=neg_idx, Value=neg_values)

#Q.2
ip="172.168.45.112:3306"            
abc=substr(ip,16,19)
abc

#Q.3
set.seed(2)
status <- sample(c('working', 'faulty', 'maintenance'), 
                 1000, replace=TRUE, prob=c(0.6, 0.2, 0.2))
round(100 * table(status) / length(status), 2)

#Q.4
set.seed(3)
nums <- runif(100, 0, 100)
sum(nums >= 25 & nums <= 50)

#Q.5
lst <- list(nums = 1:5, chars = letters[1:5], logs = rep(TRUE, 5))
lst$nums[3] <- 999
lst$chars[3] <- "new"
lst$logs[3] <- FALSE
print(lst)

#Q.6
v <- c("apple", NA, "banana", "Cat", NA)
toupper(v)

is_not_na <- !is.na(v)
v[is_not_na] <- toupper(v[is_not_na])
print(v)

#Q.7
df <- data.frame(name = c('Alice', 'Bob', 'Amy', 'Charlie'), 
                 score = c(80, 65, 90, 70))
df$name <- gsub('^A', 'Anonymous', df$name)
print(df)

#Q.8
v <- c("10", "20", "hello", "40")
v_num <- as.numeric(v)
print(v_num)  # NAs produced for non-numeric
mean(v_num, na.rm=TRUE)

#Q.9
lst <- list(even = seq(2, 10, 2), chars = letters[1:5], 
            logs = c(TRUE, FALSE, TRUE, FALSE, TRUE))
lst$even[lst$logs]
lst$chars[lst$logs]

#Q.10
s <- seq(50, 5, by=-2)
s[s %% 3 == 0]

#Q.11
set.seed(4)
v <- runif(10) #for random values(runif)
v[c(3,6,9)] <- "NA_val"
!is.na(as.numeric(v))

#Q.12
vec <- c("Hello!", "Wow...?", "R's #1.", "Good, day!", "Test@R*")
cleaned <- gsub("[[:punct:]]", "", vec)
print(cleaned)

#Q.13
lst <- list(a = 1:5, b = 10:12, c = c(-2,3))
lapply(lst, max)

#Q.14
set.seed(5)
vals <- sample(c('low','medium','high'), 20, replace=TRUE)
tbl <- table(vals)
sort(tbl, decreasing=TRUE)

#Q.15
a <- 1:5
b <- c(10, 20)
res <- tryCatch(
  a + b,
  warning = function(w) { 
    print("Warning: recycling!"); 
    a + rep(b, length.out=length(a)) 
  }
)
print(res)


