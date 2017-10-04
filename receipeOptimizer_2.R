#source link generate function
source("epilink.R")
receipeOptimizer_2 = function(myProtein, num_meal, cal_input) {
  
  #initial data cleaning
  myData2<-read.csv(file="data/epi_r.csv", header = TRUE)
  myData2<-myData2[myData2[,7]==1,]
  myData2<-myData2[complete.cases(myData2),]
  myData2 <-myData2[myData2[[2]] != 0 & myData2[[3]] > 250, ]

  #filter by 2 protein options
  protein_1 <- myData2[myData2[[myProtein[[1]]]]==1,]
  protein_2 <- myData2[myData2[[myProtein[[2]]]]==1,]
  
  #fitler each protein list by veggie options.--> eliminated due to limited amount of 22min recipe
  #protein_1 <- protein_1[protein_1[[myVeggie[[1]]]]==1 | protein_1[[myVeggie[[2]]]]==1 | protein_1[[myVeggie[[3]]]]==1, ]
  #protein_2 <- protein_2[protein_2[[myVeggie[[1]]]]==1 | protein_2[[myVeggie[[2]]]]==1 | protein_2[[myVeggie[[3]]]]==1, ]

  #rank recipe based on protein level to optimize protein goal
  protein_1 <- protein_1[order(protein_1$protein, decreasing = TRUE),]
  protein_2 <- protein_2[order(protein_2$protein, decreasing = TRUE),]
  
  #combine 2 protein options into one dataframe
  top1_1 <- protein_1 [1,]
  if (protein_2 [1,]== protein_1[1,]) {
    myFinal <- rbind(top1_1, protein_2 [2,])
  } else {
    myFinal <- rbind(top1_1, protein_2 [1,])
  }
  
  #load solver 
  library(lpSolve)
  library(lpSolveAPI)
  
  #make a new model with 3 constraints
  newmodel = make.lp(nrow=0,ncol=2)
  #put in the constraints for the protein and set it to maximise
  set.objfn(newmodel,c(myFinal[1,4],myFinal[2,4]))
  lp.control(newmodel,sense='maximize')
  
  #adding constraints for calories
  myCalorie = as.numeric(num_meal*cal_input)
  add.constraint(newmodel, c(myFinal[1,3],myFinal[2,3]), "<=", myCalorie)

  
  #making sure at least one serving each
  set.bounds(newmodel, lower=c(1, 1))
  #setting them all to be integers
  set.type(newmodel,columns=1,type = "integer")
  set.type(newmodel,columns=2,type = "integer")
  
  #check the model
  newmodel
  ColNames = c(as.character(myFinal[1,1]),as.character(myFinal[2,1]))
  RowNames = c("Calories")
  dimnames(newmodel) <- list(RowNames, ColNames)
  
  solve(newmodel)
  #get the servings of each of the receipes
  get.variables(newmodel)
  #and the value for protein
  get.objective(newmodel)
  #the values for cals
  get.constraints(newmodel)
  
  #generate result as a dataframe
  serving <- get.variables(newmodel)
  myResult<-cbind(myFinal[,1:6], serving)
  
  #validate result and generate output accordingly.
  if(myResult[,7]<= 0){
    output <- as.data.frame(1, as.character("NOOO!!!! You don't wanna get too skinny too fast!! Eat some more!!"))
    
  }else{
    output <- myResult
    output$links = sapply(output$title, getepilink)
  }


  #return result
  return(output)
  
}

