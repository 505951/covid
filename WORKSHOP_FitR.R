library('fitR')

data(SIR)

?fitmodel

theta <- c(R0 = 3, D_inf = 2)
init.state <- c(S = 999, I = 1, R = 0)
times <- 1:100
traj <- SIR$simulate(theta, init.state, times)
head(traj)

plot(traj$time,traj$I)

plotTraj(traj)

SIR$simulate

SIR$dprior
SIR$dprior(theta) 
SIR$dPointObs(data.point = c(obs = 18), model.point = c(I = 31.1/2), theta, log = FALSE)

data(epi)

plotTraj(data = epi1)

plot(traj$time,traj$I, type = "l")
lines(epi1$time,epi1$obs, type = "p")

dTrajObs(SIR, theta, init.state, epi1, log = TRUE)

traj <- SIR$simulate(theta, init.state, times)
plot(traj$time,traj$I, type = "l")
lines(epi1$time,epi1$obs, type = "p")
dTrajObs(SIR, theta, init.state, epi1, log = TRUE)

# nejprve si zkuste pohrat s parametry sami

fr <- function(par) { #funkci verohodnosti budeme maximalizovat bez zmeny v apriorni pravdepodobnosti
# sem dopsat funkci
# return(???)
}

res <- optim(c(3,2), fr) # pozor, funkce optim minimalizuje!

theta <- c(R0 = res$par[1], D_inf = res$par[2])
traj <- SIR$simulate(theta, init.state, times)
plot(traj$time,traj$I, type = "l")
lines(epi1$time,epi1$obs, type = "p")
dTrajObs(SIR, theta, init.state, epi1, log = TRUE)

SIR$rPointObs
SIR$rPointObs(model.point = c(I = 31), theta)

rTrajObs

obs.traj <- rTrajObs(SIR, theta, init.state, epi1$time)
head(obs.traj)
plotTraj(data = obs.traj)

plot(traj$time,traj$I, type = "l")
lines(obs.traj$time,obs.traj$obs, type = "p")



