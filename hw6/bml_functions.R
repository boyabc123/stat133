#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  
  n = r*c
  car = p*r*c
  m = matrix(sample(c(0,1,2),n,prob=c(1-p,p/2,p/2),replace=TRUE) ,nrow = r)
  
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  
  m_init = m
  
  m_red = m
  
  ind_1 = which(m==1)
  ind_0 = which(m==0)
  
  ind_0_firstcol= ind_0[which( ind_0 <= nrow(m) )]
  ind_1_lastcol= ind_1[which(ind_1 > (nrow(m)*( ncol(m) -1 )))]
  
  ind_0_firstcol_mod= ind_0_firstcol + nrow(m)*(ncol(m)-1)
  ind_1_lastcol_mod= ind_1_lastcol - nrow(m)*(ncol(m)-1)
  
  ind_1 = ind_1[which(ind_1<=(nrow(m)*(ncol(m)-1)))]
  ind_0 = ind_0[which(ind_0>nrow(m))]
  
  a = ind_1_lastcol_mod[which(ind_1_lastcol_mod %in% ind_0_firstcol)]
  b = ind_0_firstcol_mod[which(ind_0_firstcol_mod %in% ind_1_lastcol)]
  
  ind_1_after = c(ind_0[which(ind_0 %in% (ind_1 + nrow(m)))],a)
  ind_0_after = c(ind_1[which(ind_1 %in% (ind_0 -nrow(m)))],b)
  
  
  m_red[ind_1_after] = 1
  m_red[ind_0_after] = 0
  
  m_blue = m_red
  
  m = m_blue
  
  ind_2_b = which(m==2)
  ind_0_b = which(m==0)
  
  ind_0_lastrow_b= ind_0_b[which(ind_0_b %% nrow(m) == 0)]
  ind_2_firstrow_b= ind_2_b[which(ind_2_b %% nrow(m) == 1)]
  
  ind_0_lastrow_b_mod= ind_0_lastrow_b - nrow(m) + 1
  ind_2_firstrow_b_mod= ind_2_firstrow_b + nrow(m) - 1
  
  
  ind_0_b = ind_0_b[which(ind_0_b %% nrow(m) != 0)]
  ind_2_b = ind_2_b[which(ind_2_b %% nrow(m) != 1)]
  
  h = ind_2_firstrow_b_mod[which(ind_2_firstrow_b_mod %in% ind_0_lastrow_b)]
  k = ind_0_lastrow_b_mod[which(ind_0_lastrow_b_mod %in% ind_2_firstrow_b)]
  
  ind_2_b_after = c(ind_0_b[which(ind_0_b %in% (ind_2_b - 1))],h)
  ind_0_b_after = c(ind_2_b[which(ind_2_b %in% (ind_0_b + 1))],k)
  
  
  m_blue[ind_2_b_after] = 2
  m_blue[ind_0_b_after] = 0
   
   
   grid.new = !any(m_init != m_blue)
   
   return(list(m_blue, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  
  m = bml.init(r,c,p)
  temp = m
  temp2 = m;
  count = 0;
  for (i in 1:10000){
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
