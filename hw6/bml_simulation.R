#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.simulation <- function(r, c, p, max_steps = 1000){
  
  m = bml.init(r,c,p)
  temp = m
  temp2 = m;
  count = 0;
  for (i in 1:max_steps){
    temp = bml.step(temp2)
    temp2 = temp[[1]]
    count = count + 1
    if (temp[[2]]==TRUE){
      break
    }
  }
  #return T/F if there's gridblock, number of steps.
  return(list(temp[[2]],count))
}

#Producing Images
#png(file="example%03d.png", width=300, heigh=300)
#initial_matrix = bml.init(64,64,0.5)
#color = c('white','red','blue')
#image(t(apply(initial_matrix,2,rev)),col=color)
 # temp = initial_matrix
  #temp2 = initial_matrix
  #count = 0;
  #for (i in 1:10000){
   # temp = bml.step(temp2)
    #temp2 = temp[[1]]
    #count = count + 1
    #if (temp[[2]]==TRUE){
     # break
    #}
  #}
  #image(t(apply(temp[[1]],2,rev)),col=color)
  #count
#dev.off()


a = replicate(100,bml.sim(64,64,0.5))
hist(unlist(a[2,]),main = 'Square Gridlock (64*64, p = 0.5)',xlab = 'Steps')
save(a,file = '.RData')

b = replicate(100,bml.sim(64,64,0.35))
hist(unlist(b[2,]),main = 'Square Gridlock (64*64, p = 0.35)',xlab = 'Steps')
save(b,file = '.RData')

c = replicate(100,bml.sim(64,64,0.3, max_steps=2500))
hist(unlist(c[2,]), main = 'Square Gridlock (64*64, p = 0.33)',xlab = 'Steps')
save(c,file = '.RData')
