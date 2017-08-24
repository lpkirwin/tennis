# SANDBOX ===========================================================

source("core_models.R")

# who wins a deuce? -------------------------------------------------

p <- 0.8

res <- sapply(1:100000, function(i) {
  x <- 0L
  while (abs(x) < 2L) {
    x <- x + sample(c(-1L, 1L), 1, prob = c(1-p,p))
  }
  x
})

a <- sum(res[res>0]) / 2
b <- sum(res[res<0]) / -2
a / (a+b)
p^2 / (p^2 + (1-p)^2)

rm(p,res,a,b)

# who wins a 6-6 tiebreak? ------------------------------------------

sp <- 0.8
rp <- 0.6

order <- c(1L,0L,0L,1L)

res <- sapply(1:200000, function(i) {
  x <- 0L
  t <- 0L
  while (abs(x) < 2L) {
    serve <- order[(t %% 4) + 1L] == 1L
    if (serve) {
      d <- sample(c(-1L,1L), 1, prob = c(1-sp,sp))
    } else {
      d <- sample(c(-1L,1L), 1, prob = c(1-rp,rp))
    }
    x <- x + d
    t <- t + 1L
  }; x/2L
})

a <- sum(res[res>0])
b <- -sum(res[res<0])
a / (a+b)
(sp*rp) / (sp*rp + (1-sp)*(1-rp))
# ^ close enough for me

rm(sp,rp,order,res,a,b)

# test game functions -----------------------------------------------

prb.game(0.6)
prb.game(c(0.6,0.7))
prb.game(0.6) + prb.game(0.4) # sum to 1
prb.game(0.9) + prb.game(0.1)
p.game(0.6)
p.game(0.8)
p.game(c(0.6,0.7))
p.game(0.6) + p.game(0.4)

x <- 0:100/100

plot(x, p.game(x), t = "l", col = "red")

# looks good!

# interesting how close fn is to normal CDF

fn <- function(x) dnorm(x, mean = 50, sd = 16)
plot(diff(p.game(x)), t = "l")
curve(fn, add = TRUE, col = "red")
rm(fn)


# test tiebreak functions -------------------------------------------

prb.tiebreak(0.8, 0.6)
prb.tiebreak(0.8, 0.6)
prb.tiebreak(c(0.8, 0.5), c(0.6,0.5))

p.tiebreak(0.7,0.6)
p.tiebreak(0.5,0.5) # 50% exactly (!)


# test set functions ------------------------------------------------

prb.set(0.7,0.6)
sum(prb.set(0.7,0.6))
prb.set(c(0.55,0.5),c(0.45,0.5))

p.set(c(0.56,0.5),c(0.46,0.5))

df <- expand.grid(x = 1:99/100, y = 1:99/100)
df$p_set <- p.set(df$x, df$y)

library(ggplot2)

ggplot(df, aes(x,y,color = p_set)) + geom_point()

p.set(0.55,0.45)
p.set(0.56,0.45)
p.set(0.55,0.46)

p.set(0.55,0.48)
p.set(0.55,0.48, has_tb = FALSE)

# test match functions ----------------------------------------------

prb.match(0.6,0.45)
prb.match(c(0.6, 0.5),c(0.45, 0.5))
prb.match(c(0.6, 0.5),c(0.45, 0.5), best_of = 5)

p.match(0.6,0.45)
p.match(c(0.6, 0.5),c(0.45, 0.5))
p.match(c(0.6, 0.5),c(0.45, 0.5), best_of = 5)

p.match(c(0.5, 0.52),c(0.5, 0.5))

# simple charts -----------------------------------------------------

x <- 1:49/50

df <- data.frame(
  x = x,
  game = p.game(x),
  tiebreak = p.tiebreak(x,x),
  set = p.set(x,x),
  match = p.match(x,x)
)

library(ggplot2)
library(ggthemes)
library(lemon)

df <- tidyr::gather(df, type, p, game:match)
df$type <- factor(df$type,
                  levels = c("game", "tiebreak", "set", "match"))

g <- ggplot(df, aes(x,p, color = type)) + geom_line() +
  scale_color_manual(values = c("black", "red", "blue", "cyan")) +
  labs(x = "Probability of winning a single point",
       y = "Probability of winning a ...") +
  theme(legend.title = element_blank(),
        legend.background = element_rect(color = "black",
                                         size = 0.25),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.5),
        panel.background = element_rect(color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks = element_line(color = "black", size = 0.25),
        axis.text = element_text(color = "black"),
        axis.line = element_blank())
g <- reposition_legend(g, "top left")
ggsave("ggsave.png", g)

# 60, 141, 16, 25, 65, 84, 104, 108, 138

# check no. of combinations -----------------------------------------

## TIEBREAK

L <- lapply(1:12, function(i) 0:1)
X <- do.call(expand.grid, L)

sum(rowSums(X) == 6)
sum(rowSums(X) == 7)
924 + 792

X <- X[rowSums(X) %in% 6:7,]

Y <- apply(X, 1, cumsum)
Y <- t(Y)
head(X)
head(Y)
W <- apply(Y, 1, function(r) min(c(99,which(r == 7))))
table(W)

## SET (somewhat trickier)

L12 <- lapply(1:12, function(i) 0:1)
X12 <- do.call(expand.grid, L12)

### how many 6-0 through 6-4 scores?

X6_ <- X12[rowSums(X12) == 6L,]
Y <- apply(X6_, 1, cumsum)
Y <- t(Y)
head(X6_)
head(Y)
W <- apply(Y, 1, function(r) min(c(99,which(r == 6))))
table(W) # diff possibilities for sets with 6 wins
sum(table(W)[1:5]) # 210 ways 

### how many 6-6 and 7-5 scores?

X66 <- X12[rowSums(X12) == 6L,] # score is 6-6
X66 <- X66[rowSums(X66[,1:10]) == 5L,] # score passed through 5-5
#### 504 ways to get to 6-6 

X75 <- X12[rowSums(X12) == 7L,] # p1 has seven points
X75 <- X75[rowSums(X75[,1:10]) == 5L,] # score passed through 5-5
#### 252 ways to get to 7-5

# TOTAL
210 + 504 + 252

# code profiling ----------------------------------------------------

source("prof_script.R")
x = runif(10000)
y = runif(10000)

# library(lineprof)
# l <- lineprof(prb.tiebreak(x,y))
# l
# shine(l)

Rprof(tmp <- tempfile())
g <- prb.tiebreak(x,y)
Rprof()
library(proftools)
plotProfileCallGraph(readProfileData(tmp),
                     score = "self")