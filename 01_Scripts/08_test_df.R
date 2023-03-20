# create a small dataframe to manipulate with data and check results
valsts <- c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c","c", "c","c")
gads <- c(1:5, 1:5, 1:5)
vertiba <- c(1:4, 7:11, 15:20)

test_df <- data.frame(valsts, gads, vertiba)

n <- mutate(test_df, euLV=ifelse(valsts=="a"&gads>=3|valsts=="c"&gads>=4, "1", "0"))