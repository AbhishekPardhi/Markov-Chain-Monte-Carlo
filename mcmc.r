set.seed(1)
repeats <- 1e4 # We will repeat the experiment 10000 times
win.no.switch <- numeric(length = repeats) # will save 0 or 1 based on winning no switch
win.switch <- numeric(length = repeats) # will save 0 or 1 based on winning switch
for(r in 1:repeats) # Repeat process many times
{
    # The setup
    doors <- 1:3 # three doors
    prize <- sample(1:3, 1) # randomly select the door which has the prize
    # Contestants are ready. Game starts
    chosen.door <- sample(1:3, 1) # choose a door
    # reveal a door that is not the chosen door and not the door with a prize in it
    # doing rep(., 2) because sample() is being annoying
    which.reveal <- rep((1:3)[-c(prize, chosen.door)], 2)
    reveal <- sample(which.reveal, size = 1) # randomly choose which door to reveal
    win.no.switch[r] <- chosen.door == prize #tracking win if don't change door
    chosen.door <- (1:3)[-c(reveal, chosen.door)] #change door
    win.switch[r] <- chosen.door == prize # tracking win if change door
}
mean(win.no.switch) #Prob of winning if you don't switch
mean(win.switch) # Prob of winning if you switch
# ------------------------------------------------------------------------------------
set.seed(1)
# the setup
prob.table <- c(.2, .1, .1, .1, .1, .1, .05, .05, .05, .05, .02, .02, .02, .02, .02)
boxes <- 1:length(prob.table)
# writing the code a little differently now.
# I made a function that will be called repeatedly
box.count <- function(prob)
{
check <- rep(0, length(prob))
i <- 0
while(sum(check) < length(prob)) # check if all toys collected
{
x <- sample(boxes, 1, prob = prob) # generate a toy with given prob
check[x] <- 1 # x has been collected
i <- i + 1
}
return(i)
}
repeats <- 1e4
sim.boxes <- numeric(repeats)
for(i in 1:repeats)
{
sim.boxes[i] <- box.count(prob = prob.table)
}
hist(sim.boxes, breaks = 30)
mean(sim.boxes)
# ------------------------------------------------------------------------------------
set.seed(1)
repeats <- 1e4
esin <- numeric(length = repeats)
for(i in 1:repeats)
{
samp <- runif(1, min = 0, max = pi) # draw from U(0, pi)
esin[i] <- exp(sin(samp))
}
pi * mean(esin) #pi*E(exp(sin(x))))
