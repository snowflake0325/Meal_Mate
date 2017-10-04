#optimizer for tab1
source("epilink.R")
receipeOptimizer = function(myProtein, myVeggie, num_meal, cal_input) {
  
  #initial cleaning of the data
  myData<-read.csv(file="data/RData.csv", header = TRUE)
  myData <-myData[complete.cases(myData),]
  myData <-myData[myData[[2]] != 0 & myData[[3]] > 250, ]
  averageCal=1500
  averageFat=50
  averageSod=1500
  
  
  #filter by 2 protein options
  protein_1 <- myData[myData[[myProtein[[1]]]]==1,]
  protein_2 <- myData[myData[[myProtein[[2]]]]==1,]
  
  #filter each protein list by veggie options.
  protein_1 <- protein_1[protein_1[[myVeggie[[1]]]]==1 | protein_1[[myVeggie[[2]]]]==1 | protein_1[[myVeggie[[3]]]]==1, ]
  protein_2 <- protein_2[protein_2[[myVeggie[[1]]]]==1 | protein_2[[myVeggie[[2]]]]==1 | protein_2[[myVeggie[[3]]]]==1, ]
  
  #filter outliners
  aft_CFS_1 <- protein_1[protein_1$calories<averageCal & protein_1$fat <averageFat & protein_1$sodium < averageSod,]
  aft_CFS_2 <- protein_2[protein_2$calories<averageCal & protein_2$fat <averageFat & protein_2$sodium < averageSod,]
  
  #intitally used for select the best option for high protein, taken out and used random sample instead for return users.
  #aft_CFS_1 <- aft_CFS_1[order(aft_CFS_1$protein, decreasing = TRUE),]
  #aft_CFS_2 <- aft_CFS_2[order(aft_CFS_2$protein, decreasing = TRUE),]
  
  #generate sample from each list
  top2_2 <- aft_CFS_2[sample(nrow(aft_CFS_2), 2),]
  top1_1 <- aft_CFS_1[sample(nrow(aft_CFS_1), 1),]
  if (top2_2 [1,]== aft_CFS_1[1,]) {
    myFinal <- rbind(top1_1, top2_2 [2,])
  } else {
    myFinal <- rbind(top1_1, top2_2 [1,])
  }
  
  #set up 
  library(lpSolve)
  library(lpSolveAPI)
  
  #make a new model with 3 constraints
  newmodel = make.lp(nrow=0,ncol=2)
  #put in the constraints for the protein and set it to maximise
  set.objfn(newmodel,c(myFinal[1,4],myFinal[2,4]))
  lp.control(newmodel,sense='maximize')
  #adding constraints for calories, fat and sodium
  
  myCalorie = as.numeric(num_meal*cal_input)
  myFat = as.numeric(myCalorie*0.40/9)
  mySod = as.numeric(myCalorie)
  
  add.constraint(newmodel, c(myFinal[1,3],myFinal[2,3]), "<=", myCalorie)
  add.constraint(newmodel, c(myFinal[1,5],myFinal[2,5]), "<=", myFat)
  add.constraint(newmodel, c(myFinal[1,6],myFinal[2,6]), "<=", mySod)
  
  #making sure at least one serving each
  set.bounds(newmodel, lower=c(1, 1))
  #setting them all to be integers
  set.type(newmodel,columns=1,type = "integer")
  set.type(newmodel,columns=2,type = "integer")
  
  #check the model
  newmodel
  ColNames = c(as.character(myFinal[1,1]),as.character(myFinal[2,1]))
  RowNames = c("Calories","Fat", "sodium")
  dimnames(newmodel) <- list(RowNames, ColNames)
  
  solve(newmodel)
  #get the servings of each of the receipes
  get.variables(newmodel)
  #and the value for protein
  get.objective(newmodel)
  #the values for cals, fat, sodium
  get.constraints(newmodel)
  
  #generate final result as a data frame
  serving <- as.integer(get.variables(newmodel))
  myResult<-cbind(myFinal[,1:6], serving)
  
  #testing result and generate output
  if(myResult[,7]==0){
    output <- as.data.frame(1, as.character("NOOO!!!! You don't wanna get too skinny too fast!! Eat some more!!"))
    
  }else{
    output <- myResult
    output$links = sapply(output$title, getepilink)
  }
  
 
  #return output
  return(output)
  
}

#tester:
#receipeOptimizer(c("beef","chicken"),c("carrot","onion","potato"),5,1000)

