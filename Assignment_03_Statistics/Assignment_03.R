# Define vectors
Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC_poll <- c(4, 62, 51, 21, 2, 14, 15)
CBS_poll <- c(12, 75, 43, 19, 1, 21, 19)

# Combine into data frame
df_polls <- data.frame(Name, ABC_poll, CBS_poll)

# Inspect structure and first rows
str(df_polls)
head(df_polls)

# Summary statistics
mean(df_polls$ABC_poll)
median(df_polls$CBS_poll)
range(df_polls[, c("ABC_poll", "CBS_poll")])

# Add difference column
df_polls$Diff <- df_polls$CBS_poll - df_polls$ABC_poll

# Assign colors to each candidate
colors <- rainbow(length(df_polls$Name))

# Scatter plot with colors
plot(df_polls$ABC_poll, df_polls$CBS_poll,
     main = "Poll Comparison: ABC vs CBS",
     xlab = "ABC Poll",
     ylab = "CBS Poll",
     pch = 19,
     col = colors,
     cex = 1.8,          # make points larger
     xlim = c(0, 70),    
     ylim = c(0, 80))    

# Add legend with matching colors
legend("topleft", legend = df_polls$Name,
       col = colors, pch = 19, pt.cex = 1.5,
       bty = "n", cex = 0.8)
