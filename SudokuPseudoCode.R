set.seed(1234)
install.packages("tkrplot") # we need this for the sudoku plotting function

# throughtout this code I use 'z' to refer to the sudoku grid and 'w' is a matrix that says 
# which elements of z are fixed (w[i,j]==1) by the puzzle

# the sample function has weird behavior when you sample a vector of length 1. For example,
# sample (10,1) returns a number between 1 and 10. We don't want that, so use the function below instead
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

# In order to use this version of the plotting function you need to define a global 9x9 matrix called NumberOfErrors 
# that keeps track of how many errors are caused by grid entry i,j
SudokuPlot <- function(z,w) {
  cols <- ifelse(w>0, "blue","black")
  cols2 <- ifelse(NumberOfErrors>0, "red","black")
  #cols <- ifelse(((w>0)&&(NumberOfErrors>0)),"red","black")
  par(mar=c(0,0,0,0), bg="white")
  plot(0.5:9.5, 0.5:9.5, type="n", axes=FALSE, xlab="", ylab="")
  cusr <<- par("usr"); cplt <<- par("plt")
  segments(0.5:9.5, rep(0.5,10), 0.5:9.5, rep(9.5,10), col="grey")
  segments(rep(0.5,10), 0.5:9.5, rep(9.5,10), 0.5:9.5, col="grey")
  segments(c(0,3,6,9)+0.5, rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), lwd=3)
  segments(rep(0.5,4), c(0,3,6,9)+0.5, rep(9.5,4), c(0,3,6,9)+0.5, lwd=3)
  for (i in 1:9) for (j in 1:9) if (z[i,j]) {
    if (cols[i,j]=="red") text(j, 10-i, "X", col="pink", cex=3)
    text(j, 10-i, z[i,j], col=cols[i,j], font=ifelse(cols[i,j]=="blue",2,1),
         cex=ifelse(cols[i,j]=="blue", 2.0, 1.8))
    text(j+0.3, 10-i+0.3, NumberOfErrors[i,j], col=cols2[i,j], font=2,
         cex=ifelse(cols[i,j]=="red", 0.75, 0.75))
    
  }
}

# Function to update a 3x3 square of the sudoku
# We label the 3x3 squares 1,2...,9 from top-left to bottom right, by column
# so it goes:
# 1 4 7
# 2 5 8
# 3 6 9
SquareChanger<-function(z,FixedElementIndicator,WhichSquare)
{
  NonFixedElements<-vector()
  iIndex<-1+3*((WhichSquare-1)%%3)   # the i coordinate of the top-left entry in that square
  jIndex<-1+3*floor((WhichSquare-1)/3)    # the j coordinate of the top-left entry in that square
  for (i in iIndex:(iIndex+2)){
    for (j in jIndex:(jIndex+2)){
      if (FixedElementIndicator[i,j]==0){
        # add this number to our list
        NonFixedElements<-cbind(NonFixedElements,z[i,j])
      }
    }
  }
  #cat("\nNonFixed:",NonFixedElements)
  
  # now make some sort of change to the numbers in square z
  cat("\nYou need to write some code for the function SquareChanger()")
  return (z)
}


# the q function for the sudoku
# I wrote it to pick one of the 3x3 squares and then change the elements in that 3x3 square somehow
SudokuChanger<-function(z,w)   # Z is the current sudoku configuration; w is the matrix recording which points are fixed
{
  # Pick a 3x3 square and then change it
  WhichSquare<-sample(1:9,1)
  if (runif(1)<1.0){
    NewZ<-SquareChanger(z,w,WhichSquare)     # switch a pair of elements in that square 
  }else{
    NewZ<-GridChanger(z,w)  # change an element that is causing an error (in any square)
  }
  return(NewZ)
}

# a function to generate a random starting Sudoku configuration that satisfies the initial conditions
FillTheGrid<-function(z)  # z is the sudoku
{
  for (ThreeByThreeSquare in (1:9)){
    #cat("\nFilling",ThreeByThreeSquare)
    z<-SquareFiller(z,ThreeByThreeSquare)
  }
  return (z)
}

# A function to fill a 3x3 square in a legal way
SquareFiller<-function(z,WhichSquare){
  # which elements are missing from this 3x3 square
  MissingElements<-seq(1:9)
  iIndex<-1+3*((WhichSquare-1)%%3)   # the i coordinate of the top-left entry in that square
  jIndex<-1+3*floor((WhichSquare-1)/3)    # the y coordinate of the top-left entry in that square
  #cat("\nindices",iIndex,jIndex)
  for (i in iIndex:(iIndex+2)){
    for (j in jIndex:(jIndex+2)){
      if (z[i,j]!=0){
        # remove this number from our list
        MissingElements<-subset(MissingElements,MissingElements!=z[i,j])
      }
    }
  }
  #cat("\n",MissingElements)
  # shuffle them up
  MissingElements<-sample(MissingElements,length(MissingElements),replace=FALSE)
  #cat("\n",MissingElements)
  # and put them into the grid in a random order
  for (i in iIndex:(iIndex+2)){
    for (j in jIndex:(jIndex+2)){
      if (z[i,j]==0){
        # remove this number from our list
        a<-resamp(MissingElements,1)
        MissingElements<-MissingElements[MissingElements!=a]
        #cat("\n",MissingElements,"   we took:",a)
        z[i,j]<-a
      }
    }
  }
  return(z)
}

# This function calculates a score for the current solution
# we count the number of duplicates in rows and columns
# the 3x3 squares will be correct by construction (since we never move numbers between 3x3 squares)
Score<-function(z,w)
{
  Score<-0
  for (i in 1:9){
    Score<-Score+(9-length(unique(z[i,1:9])))
    Score<-Score+(9-length(unique(z[1:9,i])))
  }
  return(Score)
}


#######################################
#### optimisation functions ###########
#######################################


# Here's a version of Simulated Annealing that is set up for Sudoku
SimAnneal<-function(Score,Z,W,InitialTemp,FinalTemp,TempDecreaseRate){
  OldZ<-Z
  FnVal<- Score(Z,W)
  ScoreRecorder<<-FnVal  
  Temp<-InitialTemp
  Count<-1
  while (Temp>FinalTemp){
    #propose new point
    CountErrors(Z,W)
    NewZ<-SudokuChanger(Z,W)
    
    #decide whether to move
    NewVal<- Score(NewZ,W)
    cat("\nScore: ",NewVal," old Score:",FnVal  ," Temp:",Temp)
    h<-min(1,exp(-1*(NewVal-FnVal)/Temp))
    p<-runif(1)
    #browser()
    if (p<h){ #move
      OldZ<-NewZ
      Z<-NewZ
      FnVal<-NewVal
      cat("   move")
    }else{cat("   no move")}
    
    #reduce temperature
    Temp<-Temp*(1-TempDecreaseRate)
    Count<-Count+1
    CountErrors(Z,W)
    # pause for a bit
    if (Count%%10==0){
       SudokuPlot(Z,W)   
    }
    Start.Time<-Sys.time()
    while (Sys.time()<Start.Time+0.02){}
    ScoreRecorder<<-append(ScoreRecorder,FnVal)
  }
  #points(X,Y,pch=19,col="blue")
  return (Z)
}

# This function counts the number of errors for the entire grid and records 
# that answer in the matrix NumberOfErrors
CountErrors<-function(z,w){
  TotalErrors<-0
  for (i in 1:9){
    for (j in 1:9){
      # if this element is not pre-defined, count how many conflicts it causes
      # note I only check rows and columns
      E<-0
      if (w[i,j]==0){
        E<-E-1+sum(z[1:9,j]==z[i,j])  # there's a -1 because elements will always match with themself
        E<-E-1+sum(z[i,1:9]==z[i,j])  # there's a -1 because elements will always match with themself        
      }
      NumberOfErrors[i,j]<<-E   # Note you need a double left arrow to assign to an operator from outside the functions scope
      TotalErrors<-TotalErrors+E
      #cat("\n Errors for ",i,j," which is ",z[i,j],": ",NumberOfErrors[i,j])
    }
  }
  cat("\nTotalErrors: ",TotalErrors)
}
###################################
###################################
###################################


# Here's how I read a sudoku, convert it to a matrix
# (the function above doesn't work on lists) and then plot it
MyZ<-read.table("TestSudoku.txt")
MyZ<-matrix(unlist(MyZ), ncol = 9, byrow = TRUE)

# Record the points at which the numbers are fixed:
MyFixedPoints<-ifelse(MyZ>0,1,0)

NumberOfErrors<-mat.or.vec(9,9)  # this will keep track of how many errors each number causes
SudokuPlot(MyZ,MyFixedPoints)  # plot the puzzle


# Fill up the grid in a random way, making sure the nine 3x3 squares each 
# have the numbers 1 through 9 in them once
MyZ2<-FillTheGrid(MyZ)

# make sure the graphics parameters are set to draw just one plot
op <- par(mfrow = c(1, 1)) 
# Optimize!
InitialTemp<-1
FinalTemp<-0.0001
TempDecreaseRate<-0.02
SimAnneal(Score,MyZ2,MyFixedPoints,InitialTemp,FinalTemp,TempDecreaseRate)
# output a plot of the number of errors as the algorithm progresses
plot(ScoreRecorder,type='l')
