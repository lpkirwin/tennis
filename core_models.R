
# SIMULATING A TENNIS MATCH

# What variables do we need?

# - probability of each player winning a service point
# --> prb of successful first serve
#       * prb of ace
#       * prb of service winner
#       * prb of win post-rally
#       * prb of lose post-rally
# --> prb of successful second serve
#       * prb of ace
#       * prb of service winner
#       * prb of win post-rally
#       * prb of lose post-rally
# --> else: double fault

# ___________________________________________________________________

# FN game ===========================================================

# probability of winning a game
# as a fn of server's point-winning probability
prb.game <- function(SPP) {
  if (!is.numeric(SPP) | !(SPP>=0 & SPP<=1)) message("Bad val: SPP")
  OPP <- 1 - SPP
  prb <- numeric(4)
  # 40-0, only one way this can happen
  prb[1] <- SPP^4
  # 40-15, four ways this can happen
  prb[2] <- SPP^4 * OPP * 4
  # 40-30, ten ways this can happen (5c2)
  prb[3] <- SPP^4 * OPP^2 * 10
  # 40-40, twenty ways this can happen (6c3)
  prb[4] <- SPP^3 * OPP^3 * 20
  # multiply last by prb of winning deuce
  prb[4] <- prb[4] * (SPP^2 / (SPP^2 + OPP^2))
  # name + return vector
  names(prb) <- c("40-0", "40-15", "40-30", "40-40")
  prb
}
prb.game <- Vectorize(prb.game)
# ^ vector of prbs for four different outcomes

# ___________________________________________________________________

p.game <- function(SPP) {colSums(prb.game(SPP))}
# ^ single number for win probability

# ___________________________________________________________________

# FN tiebreak =======================================================

# the following function returns a data.frame
# that tabulates the possble tiebreak outcomes
# resulting in a win by p1 -- this will be 
# used in conjunction with the point-winning
# probabilities in a later function
gen_TB <- function() {
  
  tb_order <- c(1L,0L,0L,1L,1L,0L,0L,1L,1L,0L,0L,1L)
  # ^ 1 = service point, 0 = return point
  
  # possible winning outcomes for p1
  tb_df <- data.frame(
    p1p = c(7L,7L,7L,7L,7L,7L,6L), # p1 points
    p2p = c(0L,1L,2L,3L,4L,5L,6L)  # p2 points
  )
  
  # tp := total points
  tb_df$tp <- as.integer(rowSums(tb_df))
  
  # tsp := total service points
  tb_df$tsp <- sapply(tb_df$tp, function(p) {
    sum(tb_order[1:p])
  })
  
  # trp := total return points
  tb_df$trp <- tb_df$tp - tb_df$tsp
  
  # indicators for whether last points is serve/return
  tb_df$lps <- tb_order[tb_df$tp]
  tb_df$lpr <- 1L - tb_order[tb_df$tp]
  # set to zero if final score is 6-6
  tb_df$lps[tb_df$p2p == 6L] <- 0L
  tb_df$lpr[tb_df$p2p == 6L] <- 0L
  
  # for each row of tb_df, we want to know
  # all of the different ways the points could be
  # split in terms of service/return points won
  # by p1 and p2
  tb_cases <- lapply(1:7, function(i) {
    r <- tb_df[i,]
    row.names(r) <- NULL
    p1_max_sp <- r$tsp # maximum service points for p1
    p1_min_sp <- r$p1p - r$trp # min service pts for p1
    tmp <- data.frame(p1sp = p1_max_sp:p1_min_sp)
    cbind(r,tmp)
  })
  
  tb_df <- do.call(rbind, tb_cases)
  
  # p1 return pts; p2 service and return pts
  tb_df$p1rp <- tb_df$p1p - tb_df$p1sp
  tb_df$p2sp <- tb_df$tsp - tb_df$p1sp
  tb_df$p2rp <- tb_df$trp - tb_df$p1rp
  
  # to calculate the overall probability of each
  # state, e.g. winning 7-3, we need the number of 
  # combinations of wins that results in that state
  
  # will do this from the POV of p2, but could
  # do it from p1's POV and get equivalent results
  
  # because p2 can't win the last serve if the score
  # is 7-x, we will subtract one point from the 
  # relevant choice set
  tb_df$c_sp <- choose(tb_df$tsp - tb_df$lps, tb_df$p2sp)
  tb_df$c_rp <- choose(tb_df$trp - tb_df$lpr, tb_df$p2rp)
  tb_df$comb <- tb_df$c_sp * tb_df$c_rp
  
  # sum(tb_df$comb) # CHECK: should be 1716
  
  tb_df
  
}

TB <- gen_TB()

# ___________________________________________________________________

# probability of winning a tiebreak
# as a fn of player 1's point-winning probabilities
# SPP := service point probability
# RPP := return point probability
prb.tiebreak <- function(SPP, RPP, X = TB) {
  
  OSPP <- 1 - SPP
  ORPP <- 1 - RPP
  
  tmp <- data.frame(
    SPP = SPP, RPP = RPP,
    OSPP = OSPP, ORPP = ORPP
  )
  
  # add probabilities to table of outcomes
  X <- cbind(tmp, X)
  
  # for each row in the table, compute likelihood
  X$L <- with(X,
    SPP^p1sp * RPP^p1rp * OSPP^p2sp * ORPP^p2rp * comb)
  
  # sum likelihood within each category (7-0, 7-1, etc.)
  X <- aggregate(list(L = X$L), 
                 list(p1p = X$p1p, p2p = X$p2p),
                 sum)
  
  out <- X$L
  
  # need to multiply 6-6 scenario by appropriate factor
  out[7] <- out[7] * 
    (SPP*RPP) / (SPP*RPP + (1-SPP)*(1-RPP))
  
  # rename and return
  names(out) <- paste0(X$p1p,"-",X$p2p)
  out
  
}
prb.tiebreak <- Vectorize(prb.tiebreak)

# ___________________________________________________________________

p.tiebreak <- function(SPP, RPP, TB = TB) {
  colSums(prb.tiebreak(SPP, RPP))
}

# ___________________________________________________________________

# FN set ============================================================

# the following function returns a data.frame
# that tabulates the possble set outcomes
# resulting in a win by p1 -- this will be 
# used in conjunction with the game-winning
# probabilities in a later function
gen_ST <- function() {
  
  st_order <- rep(1:0,6)
  # ^ 1 = service game, 0 = return game
  
  # possible winning outcomes for p1
  st_df <- data.frame(
    p1g = c(6L,6L,6L,6L,6L,7L,6L), # p1 games
    p2g = c(0L,1L,2L,3L,4L,5L,6L)  # p2 games
  )
  
  # tp := total games
  st_df$tg <- as.integer(rowSums(st_df))
  
  # tsp := total service games
  st_df$tsg <- sapply(st_df$tg, function(p) {
    sum(st_order[1:p])
  })
  
  # trp := total return games
  st_df$trg <- st_df$tg - st_df$tsg
  
  # indicators for whether last game is serve/return
  st_df$lgs <- st_order[st_df$tg]
  st_df$lgr <- 1L - st_order[st_df$tg]
  # set to zero if final score is 6-6
  # st_df$lgs[st_df$p2g == 6L] <- 0L
  # st_df$lgr[st_df$p2g == 6L] <- 0L
  
  # for each row of st_df, we want to know
  # all of the different ways the games could be
  # split in terms of service/return games won
  # by p1 and p2
  st_cases <- lapply(1:7, function(i) {
    r <- st_df[i,]
    row.names(r) <- NULL
    p1_max_sg <- r$tsg # maximum service games for p1
    p1_min_sg <- r$p1g - r$trg # min service games for p1
    tmp <- data.frame(p1sg = p1_max_sg:p1_min_sg)
    cbind(r,tmp)
  })
  
  st_df <- do.call(rbind, st_cases)
  
  # p1 return games; p2 service and return games
  st_df$p1rg <- st_df$p1g - st_df$p1sg
  st_df$p2sg <- st_df$tsg - st_df$p1sg
  st_df$p2rg <- st_df$trg - st_df$p1rg
  
  # indicator for 12-game set
  st_df$g12 <- as.numeric(st_df$tg == 12)
  
  # to calculate the overall probability of each
  # state, e.g. winning 6-3, we need the number of 
  # combinations of wins that results in that state
  
  # will do this from the POV of p2, but could
  # do it from p1's POV and get equivalent results
  
  # because p2 can't win the last serve if the score
  # isn't 6-6, we will subtract one game from the 
  # relevant choice set
  st_df$c_sg <- choose(st_df$tsg - st_df$lgs, st_df$p2sg)
  st_df$c_rg <- choose(st_df$trg - st_df$lgr, st_df$p2rg)
  st_df$comb <- pmax(st_df$c_sg * st_df$c_rg, 1)
  
  sum(st_df$comb) # CHECK: should be 1716
  
  st_df
  
}

ST <- gen_ST()

ST[22:28,]

aggregate(ST$comb, list(tg = ST$tg, p1g = ST$p1g), sum)
sum(ST$comb) # 1134

# probability of winning a set 
# as a fn of player 1's point-winning probabilities
# sgp := service game-winning probability
# rgp := return game-winning probability
# also uses p.tiebreak()
prb.set <- function(SPP, RPP) {
  
  sgp <- p.game(SPP)
  rgp <- p.game(RPP)
  osgp <- 1 - sgp # prb of break on p1 serve
  orgp <- 1 - rgp # prb of hold on p2 serve
  prb <- numeric(7)
  
  # assume that p1 serves first (!)
  
  # 6-0, only one way this can happen
  prb[1] <- sgp^3 * rgp^3
  
  # 6-1, two types of ways this can happen:
  #   1st: drop one return point, 3c1
  prb[2] <- sgp^4 * rgp^2 * orgp *        3 +
  #   2nd: drop one service point, 3c1
            sgp^3 * rgp^3 *        osgp * 3
  
  # 6-2, three types of ways this can happen:
  #   1st, drop two return points, 4c2
  prb[3] <- sgp^4 * rgp^2 * orgp^2 *        6 +
  #   3rd, drop one of each, 4c1^2
            sgp^3 * rgp^3 * orgp * osgp *  16 +
  #   2nd, drop two service points, 4c2
            sgp^2 * rgp^4 *        osgp^2 * 6
  
  # 6-3, four types of ways this can happen:
  #   1st, drop 3R and 0S
  prb[4] <- sgp^5 * rgp^1 * orgp^3 *           4 +
  #   2nd, drop 2R and 1S
            sgp^4 * rgp^2 * orgp^2 * osgp   * 30 +
  #   3rd, drop 1R and 2S
            sgp^3 * rgp^3 * orgp   * osgp^2 * 40 +
  #   4th, drop 0R and 3S
            sgp^2 * rgp^4 * orgp^3 *          10
  
  # 6-4, five types of ways this can happen:
  #   1st, drop 4R and 0S
  prb[5] <- sgp^5 * rgp^1 * orgp^4 *            5 +
  #   2nd, drop 3R and 1S
            sgp^4 * rgp^2 * orgp^3 * osgp   *  50 +
  #   3rd, drop 2R and 2S
            sgp^3 * rgp^3 * orgp^2 * osgp^2 * 100 +
  #   4th, drop 1R and 3S
            sgp^2 * rgp^4 * orgp   * osgp^3 *  50 +
  #   5th, drop 0R and 4S
            sgp^1 * rgp^5 *          osgp^4 *   5
  
  # 7-5, six types of ways this can happen:
  #   1st, drop 5R and 0S
  prb[6] <- sgp^6 * rgp^1 * orgp^5 *            6 +
  #   2nd, drop 4R and 1S
            sgp^5 * rgp^2 * orgp^4 * osgp   *  90 +
  #   3rd, drop 3R and 2S
            sgp^4 * rgp^3 * orgp^3 * osgp^2 * 300 +
  #   4th, drop 2R and 3S
            sgp^3 * rgp^4 * orgp^2 * osgp^3 * 300 +
  #   5th, drop 1R and 4S
            sgp^2 * rgp^5 * orgp^1 * osgp^4 *  90 +
  #   6th, drop 0R and 5S
            sgp^1 * rgp^6 * orgp^0 * osgp^5 *   6
  
  # 6-6, seven types of ways this can happen:
  #   1st, drop 6R and 0S
  prb[7] <- sgp^6 *         orgp^6 *            1 +
  #   2nd, drop 5R and 1S
            sgp^5 * rgp   * orgp^5 * osgp   *  36 +
  #   3rd, drop 4R and 2S
            sgp^4 * rgp^2 * orgp^4 * osgp^2 * 225 +
  #   4th, drop 3R and 3S
            sgp^3 * rgp^3 * orgp^3 * osgp^3 * 400 +
  #   5th, drop 2R and 4S
            sgp^2 * rgp^4 * orgp^2 * osgp^4 * 225 +
  #   6th, drop 1R and 5S
            sgp   * rgp^5 * orgp   * osgp^5 *  36 +
  #   7th, drop 0R and 6S
                    rgp^6 *          osgp^6 *   1
  
  # multiply last by chance of winning tiebreak
  prb[7] <- prb[7] * p.tiebreak(SPP, RPP)
  
  # name + return vector
  names(prb) <- c("6-0", "6-1", "6-2", "6-3", "6-4", "7-5", "tb")
  prb
  
}

prb.set(0.7,0.6)
sum(prb.set(0.7,0.6)) # V BAD !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ___________________________________________________________________

# SANDBOX ===========================================================

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


# check no. of combinations -----------------------------------------

## tiebreak

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

## set

L10 <- lapply(1:10, function(i) 0:1)
L12 <- lapply(1:12, function(i) 0:1)
X10 <- do.call(expand.grid, L10)
X12 <- do.call(expand.grid, L12)

sum(rowSums(X10) == 5) * 3 #756

sum(rowSums(X) == 6) # 924
X6 <- X[rowSums(X) == 6,]

# X7 <- X[rowSums(X) == 7,]
# X7 <- X7[X7[,12] == 1L,]

Y <- apply(X6, 1, cumsum)
Y <- t(Y)
head(X6)
head(Y)
W <- apply(Y, 1, function(r) min(c(99,which(r == 6))))
table(W) # diff possibilities for sets with 6 wins
sum(table(W)[1:5]) # 210

1134 - 210

aggregate(ST$comb, list(tg = ST$tg, p1g = ST$p1g), sum)
sum(ST$comb) # 1134


## starting AGAIN

### how many 6-6 scores?

L12 <- lapply(1:12, function(i) 0:1)
X12 <- do.call(expand.grid, L12)

X12 <- X12[rowSums(X12) == 6L,] # score is 6-6
X12 <- X12[rowSums(X12[,11:12]) == 1L,] # each won...
# ...one of last two games

#### 504 ways to get to 6-6 

### how many 7-5 scores?
L12 <- lapply(1:12, function(i) 0:1)
X12 <- do.call(expand.grid, L12)

X12 <- X12[rowSums(X12) == 7L,] # p1 has seven points
X12 <- X12[X12[,12] == 1L,] # won the last point
X12 <- X12[rowSums(X12[,1:10]) == 5L,] # score passed through 5-5

#### 252 ways to get to 7-5

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
