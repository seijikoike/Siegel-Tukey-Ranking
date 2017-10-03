st.scoring <- function(treat1, treat2){
  data <- c(treat1, treat2)
  n <- length(data)
  
  df.x <- data.frame(data, rep(c(1,0), c(length(treat1), length(treat2))))
  names(df.x) <- c("Value", "Treatment")
  df <- df.x[order(data),]
  
  s.t.score <- 1:n
if(n %% 4 == 2){
  for(i in 2:n){
    if(((i-1) %% 2) == 1 && i <= (n/2)){s.t.score[i] <- s.t.score[i-1]+3}
    else if(((i-1) %% 2) == 0 && i <= (n/2)){s.t.score[i] <- s.t.score[i-1]+1}
    else if((i-1) == (n/2)){s.t.score[i] <- s.t.score[i-1]+1}
    else if(((i-1) %% 2 == 0) && i >= (n/2)){s.t.score[i] <- s.t.score[i-1]-3}
    else if(((i-1) %% 2 == 1) && i >= (n/2)){s.t.score[i] <- s.t.score[i-1]-1}
  }
}
if(n %% 4 == 0){
  for(i in 2:n){
    if(((i-1) %% 2) == 1 && i <= (n/2)){s.t.score[i] <- s.t.score[i-1]+3}
    else if(((i-1) %% 2) == 0 && i <= (n/2)){s.t.score[i] <- s.t.score[i-1]+1}
    else if((i-1) == (n/2)){s.t.score[i] <- s.t.score[i-1]-1}
    else if(((i-1) %% 2 == 0) && i >= (n/2)){s.t.score[i] <- s.t.score[i-1]-3}
    else if(((i-1) %% 2 == 1) && i >= (n/2)){s.t.score[i] <- s.t.score[i-1]-1}
  }
}
if(n %% 4 == 1){
  for(i in 2:n){
    if(((i-1) %% 2) == 1 && i <= ceiling(n/2)){s.t.score[i] <- s.t.score[i-1]+3}
    else if(((i-1) %% 2) == 0 && i <= ceiling(n/2)){s.t.score[i] <- s.t.score[i-1]+1}
    else if((i-1) == ceiling(n/2)){s.t.score[i] <- s.t.score[i-1]-2}
    else if(((i-1) %% 2 == 0) && i >= ceiling(n/2)){s.t.score[i] <- s.t.score[i-1]-1}
    else if(((i-1) %% 2 == 1) && i >= ceiling(n/2)){s.t.score[i] <- s.t.score[i-1]-3}
  }
}
if(n %% 4 == 3){
  for(i in 2:n){
    if(((i-1) %% 2) == 1 && i <= floor(n/2)){s.t.score[i] <- s.t.score[i-1]+3}
    else if(((i-1) %% 2) == 0 && i <= floor(n/2)){s.t.score[i] <- s.t.score[i-1]+1}
    else if((i-1) == floor(n/2)){s.t.score[i] <- s.t.score[i-1]+2}
    else if(((i-1) %% 2 == 0) && i >= floor(n/2)){s.t.score[i] <- s.t.score[i-1]-1}
    else if(((i-1) %% 2 == 1) && i >= floor(n/2)){s.t.score[i] <- s.t.score[i-1]-3}
  }
}
  
df <- data.frame(df, s.t.score)
  
names(df) <- c("Values", "Treatment", "ST.Score")
  
dups <- vector()
for(num in unique(df$Values[duplicated(df$Values)])){
  dups <- c(dups, df$Values[df$Values == num])
  }
for(dup in dups){
  df$ST.Score[df$Values == dup] <- mean(df$ST.Score[df$Values == dup])
}

return(df)
}

treat1 <- c(70, 69, 65, 64, 66, 65, 64, 66, 60, 70, 66)
treat2 <- c(67, 64, 62, 64, 69, 70, 65, 66, 63, 74, 60)


st.scoring(t1, t2)
