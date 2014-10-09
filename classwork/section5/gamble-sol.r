# This function should return a numeric value indicating your bet based on a
# wealth value and previous winnings. This function may not bet more than the
# current wealth value. This value may not be below 0
betFun <- function(current.wealth,  previous.winnings=NA) {

    # your code here
    bet <- max(bet, 0)
    return(min(bet, current.wealth))
}

# Implement a function that runs a gambling simulation with the following
# parameters:
#
# <bet.FUN>: a betting function that returns a numeric value based on inputs of
#   current wealth and previous winnings
# <init.wealth>: a numeric value giving starting wealth
# <prob.win>: a numeric value between 0 and 1 giving the probability of winning
#   each round
# <max.turns>: an integer giving the maximum number of turns to be played
#
# Your function should return the difference between your initial and final
# wealth after playing a game for <max.turns> rounds. If your wealth falls below
# 0, the simulation should end.
gamble <- function(bet.FUN, init.wealth=50, prob.win=0.52, max.turns=25) {

    current.wealth <- init.wealth
    bet <- bet.FUN(current.wealth, 0)
    games <- rbinom(max.turns, 1, prob.win)
    
    for (turn in 1:max.turns) {

        if (games[turn]) {
            current.wealth <- current.wealth + bet
            bet <- bet.FUN(current.wealth, bet)
        } else {
            current.wealth <- current.wealth - bet
            bet <- bet.FUN(current.wealth, -bet)
        }

        if (current.wealth < 0) break
        if (current.wealth > 2.5 * init.wealth) break
    }

    return(current.wealth - init.wealth)
}



