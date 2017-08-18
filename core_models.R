
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

# -------------------------------------------------------------------

# FN ==============
# probability of winning a game
# as a fn of server's point-winning probability
prb.game <- function(spp) {
  if (!is.numeric(spp) | !(spp>=0 & spp<=1)) message("Bad val: spp")
  opp <- 1 - spp
  prb <- numeric(4)
  # 40-0, only one way this can happen
  prb[1] <- spp^4
  # 40-15, four ways this can happen
  prb[2] <- spp^4 * opp * 4
  # 40-30, ten ways this can happen (5c2)
  prb[3] <- spp^4 * opp^2 * 10
  # 40-40, twenty ways this can happen (6c3)
  prb[4] <- spp^3 * opp^3 * 20
  # multiply last by prb of winning deuce
  prb[4] <- prb[4] * (spp^2 / (spp^2 + opp^2))
  # name + return vector
  names(prb) <- c("40-0", "40-15", "40-30", "40-40")
  prb
}
prb.game <- Vectorize(prb.game)
# ^ vector of prbs for four different outcomes

p.game <- function(spp) colSums(prb.game(spp))
# ^ single number for win probability

# # test it out
# prb.game(0.6)
# prb.game(c(0.6,0.7))
# prb.game(0.6) + prb.game(0.4) # sum to 1
# prb.game(0.9) + prb.game(0.1)
# p.game(0.6)
# p.game(0.8)
# p.game(c(0.6,0.7))
# p.game(0.6) + p.game(0.4)
# 
# x <- 0:100/100
# 
# plot(x, p.game(x), t = "l", col = "red")
# 
# # looks good!
# 
# # interesting how close fn is to normal CDF
# 
# fn <- function(x) dnorm(x, mean = 50, sd = 16)
# plot(diff(p.game(x)), t = "l")
# curve(fn, add = TRUE, col = "red")
# rm(fn)


# -------------------------------------------------------------------

# FN ==============
# probability of winning a set 
# as a fn of player 1's game-winning probabilities
# sgp = service game-winning probability
# rgp = return game-winning probability
# tbp = tie break win probability
prb.set <- function(sgp, rgp) {
  # assume that p1 starts first
  prb <- numeric(7)
  osgp <- 1 - sgp # prb of break on p1 serve
  orgp <- 1 - rgp # prb of hold on p2 serve
  # 6-0, only one way this can happen
  prb[1] <- sgp^3 * rgp^3
  # 6-1, two types of ways this can happen:
  #   1st: drop one return point, 3c1
  prb[2] <- sgp^4 * rgp^2 * orgp * 3 +
  #   2nd: drop one service point, 3c1
            sgp^3 * rgp^3 * osgp * 3
  # 6-2, three types of ways this can happen:
  #   1st, drop two return points, 4c2
  prb[3] <- sgp^4 * rgp^2 * orgp^2 * 6 +
  #   2nd, drop two service points, 4c2
            sgp^2 * rgp^4 * osgp^2 * 6 +
  #   3rd, drop one of each, 4c1^2
            sgp^3 * rgp^3 * orgp * osgp * 16
  # 6-3, four types of ways this can happen:
  #   1st, drop 3R and 0S
  prb[4] <- sgp^5 * rgp^1 * orgp^3 *           4 +
  #   1st, drop 2R and 1S
            sgp^4 * rgp^2 * orgp^2 * osgp   * 30 +
  #   1st, drop 1R and 2S
            sgp^3 * rgp^3 * orgp   * ogsp^2 * 40 +
  #   1st, drop 0R and 3S
            sgp^2 * rgp^4 * orsp^3 *          10
  # 6-4, five types of ways this can happen:
  #   1st, drop 4R and 0S
  prb[5] <- sgp^5 * rgp^1 * orgp^4 *            5 +
  #   2nd, drop 3R and 1S
            sgp^4 * rgp^2 * orgp^3 * osgp   *  50 +
  #   3rd, drop 2R and 2S
            sgp^3 * rgp^3 * orgp^2 * ogsp^2 * 100 +
  #   4th, drop 1R and 3S
            sgp^2 * rgp^4 * orgp   * ogsp^3 *  50 +
  #   5th, drop 0R and 4S
            sgp^1 * rgp^5 *          ogsp^4 *   5
}

# -------------------------------------------------------------------

# Who wins a deuce?

p <- 0.8

res <- sapply(1:100000, function(i) {
  x <- 0L
  while (abs(x) < 2L) x <- x + sample(c(-1L, 1L), 1, prob = c(1-p,p))
  x
})

a <- sum(res[res>0]) / 2
b <- sum(res[res<0]) / -2
a / (a+b)
p^2 / (p^2 + (1-p)^2)

rm(p,res,a,b)


