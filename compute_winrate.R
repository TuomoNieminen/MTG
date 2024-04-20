
setwd("C:/Users/tuomo/OneDrive/Työpöytä/MTG")

library(dplyr)
library(stringr)

df <- read.table("17LandsData/ICU2024.txt",sep = "\t", header = TRUE)

compute_winrate <- function(wins, losses) {
  x <- Hmisc::binconf(wins, wins + losses, alpha = 0.05, method = "wilson")
  x <- round(100*x)
  paste0(x[1], "% (95%CI: ", x[2], "-", x[3], ")")
 }

wl <- stringr::str_split_fixed(df$W.L, " - ", 2)
class(wl) <- "integer"
wl <- as.data.frame(wl)

colnames(wl) <- c("wins", "losses")
  
df <- bind_cols(df, wl)
df$Rank <- gsub("-(*.)","",df$Start.Rank)
df$Rank <- factor(df$Rank, levels = c("Silver", "Gold", "Platinum", "Diamond"))

tb <- df |> 
	group_by(Rank) |> 
	summarise(WINS = sum(wins),
		   LOSSES = sum(losses),
		   winrate = compute_winrate(WINS, LOSSES))
tb <- tb |> arrange(Rank)

knitr::kable(tb)

