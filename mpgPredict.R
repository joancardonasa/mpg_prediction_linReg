mpgPredict <- function(input) {
    # This function is a simple example of linear regression prediction.
    # The objective is to predict the miles per gallon a car can get depending 
    # on its horsepower. 
    # The training data set will be mtcars, provided by R.
    
    library(dplyr)
    library(scatterplot3d)
    
    data <- mtcars
    data <- tbl_df(data) # Makes the algorithm run faster (C++ implementation)
    
    data <- select(data, hp, mpg) # Subsets the data to the variables we need.
    
    ## data will now be our training data set
    
    ## Plotting the relationship between these two variables we can observe
    ## that they are somehow linearly dependant on each other. 
    
    ## We can hypothesize: h(x) = th_0 + th_1 * x
    ## This hypothesis h(x) must be similar to y.
    
    ## The parameters we must estimate is th_0 and th_1.
    
    ## We will use the cost function J(th_0, th_1) and minimize it to obtain
    ## the ideal values of these parameters, using gradient descent.
    
    th_0 <- 50 ## First parameter value, based on observations (of the plot)
    th_1 <- -0.6 ## First parameter value, based on observations (of the plot)
    
    alpha <- 0.000001 ## Don't know what is a reasonable value for alpha (learning rate)
    
    ## The gradient descent loop should start here..! Don't know which conditions
    ## to establish for convergence situation...let's use a simple 1 to 10
    
    i <- 1
    
    while(i < 109) {
    
        data <- mutate(data, h_x = th_0 + th_1 * data$hp) ## We now add the first 
                                                          ## estimation of y, h(x)
        data <- mutate(data, error_vector = h_x - mpg)
        
        costFunction_J <- (1/(2*nrow(data))) * ((sum(data$error_vector))^2)
        
        J_th_0 <- (1/(nrow(data))) * ((sum(data$error_vector)))
        J_th_1 <- (1/(nrow(data))) * ((sum(data$error_vector * data$hp)))
        
        
        ## First value of J is 338.57
        
        temp_0 <- th_0 - alpha * (J_th_0)
        temp_1 <- th_1 - alpha * (J_th_1)
        
        th_0 <- temp_0
        th_1 <- temp_1
        
        i <- i + 1
        
        ##print(costFunction_J)
        ##scatterplot3d(th_0,th_1,costFunction_J, main="Cost Function")
    
    }
    
    ## This gradient descent algorithm currently gives us a minimum of 
    ## J = 0.08864. 
    
    ## To do: plot the two graphs and code the prediction function for an input
    
    print(paste("The prediction for your hp input", input, "is", th_0 + th_1 * input, "mpg"))
    
}