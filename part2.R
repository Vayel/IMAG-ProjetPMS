theta_0 <- function(numbers){
  max=numbers[1]
  for(i in 2:length(numbers)){
    if(numbers[i]>max)
      max=numbers[i]
  }
  return(max)
}

theta_1 <- function(numbers){
  max=numbers[1]
  n=length(numbers)
  for(i in 2:n){
    if(numbers[i]>max)
      max=numbers[i]
  }
  return(((n+1)/n)*max-1)
}

theta_2 <- function(numbers){
  max=numbers[1]
  min=numbers[1]
  for(i in 2:length(numbers)){
    if(numbers[i]>max){
      max=numbers[i]
    }
    else if(numbers[i]<min){
      min=numbers[i]
    }
  }
  return(max+min-1)
}

theta_3 <- function(numbers){
  average=numbers[1]
  for(i in 2:length(numbers)){
    average=average+numbers[i]
  }
  average=average/length(numbers)
  return (2*average-1)
}

main <- function(n,theta){
  numbers=sample(1:theta,n)
  cat("The real number of tanks is",theta,"\n")
  print("The estimations are as follow:")
  cat("theta_0",theta_0(numbers),"\n")
  cat("theta_1",theta_1(numbers),"\n")
  cat("theta_2",theta_2(numbers),"\n")
  cat("theta_tilde",theta_3(numbers),"\n")
}

score_thetas <-function(theta,count){
  cat("The real number of tanks is:",theta,"and the average error is\n\n")
  scales=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.75,1)
  for(i in 1:length(scales)){
    n=ceiling(theta*scales[i])
    cat("When n=theta*",scales[i],"we have:\n")
    score_n(theta,n,count)
  }
}

score_n <- function(theta,n,count){
  averages=c(0,0,0,0)
  variances=c(0,0,0,0)
  for(i in 1:count){
    numbers=sample(1:theta,n)
    averages[1]=averages[1]+abs(theta_0(numbers)-theta);
    averages[2]=averages[2]+abs(theta_1(numbers)-theta);
    averages[3]=averages[3]+abs(theta_2(numbers)-theta);
    averages[4]=averages[4]+abs(theta_3(numbers)-theta);
  }
  for(i in 1:4){
    averages[i]=round(averages[i]/count);
  }
  cat("theta_0",averages[1],"\n")
  cat("theta_1",averages[2],"\n")
  cat("theta_2",averages[3],"\n")
  cat("theta_tilde",averages[4],"\n\n")
}