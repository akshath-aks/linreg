#' Ridgereg Class calculates Ridge Regression Model For Data frame.
#'
#' @field formula (formula) Dependent and independent columns in data frame
#' @field data (data.frame) Data frame for ridge regression models
#' @field B_ridge (vector) calculates Beta ridge coefficients
#' @field y_h (matrix) Estimated dependent values
#' @field X (matrix) model matrix which is scaled later
#' @field data_name (character) Data Frame name
#' @field lamda (numeric) hyper parameter value
#'
#' @return
#' @exportClass ridgereg
#' @export ridgereg
#' @importfrom MASS lm.ridge

ridgereg<-setRefClass('ridgereg', 
                      fields=list(formula='formula',
                                         data ='data.frame',
                                         B_ridge='vector',
                                         y_h='matrix',
                                         X='matrix',
                                         data_name='character',
                                         lamda='numeric'),
                      
                      #creating class and adding all the attributes used inside field list.
                    
                    #adding the methods list, first initiliaze function with formula,data.
                    
                    methods=list(
                      initialize=function(formula=formula(),data=data.frame(),lamda=numeric()){
                        
                        .self$formula<<-formula # assigning the formula and data to attributes
                        .self$data<<-data
                        .self$lamda<<-lamda
                        
                        data_name<<-deparse(substitute(data)) # getting data.frame name
                        
                        #creating independent variable matrix
                        X<<-model.matrix(formula,data)
                        X[,-1]<<-(scale(X[,-1]))

                        
                        #getting dependent variable matrix
                        Y<-as.matrix(data[,all.vars(formula)[1]])

                        
                        #calculating estimator coefficients.
  
                        B_ridge_a<-solve((t(X)%*%X+diag(ncol(X))*lamda))%*%t(X)%*% Y
                        
                        #calculating predicted y values, residuals(error),degree of freedom,
                        #residual variance, variance of the regression coefficients and t value
                        y_h_a<-X%*%B_ridge_a
                        
                        #converting estimators(B_h) from matrix to vector
                        B_ridge_a<-as.vector(B_ridge_a)
                        names(B_ridge_a)<-colnames(X)# adding names to estimators(B_h) vector
                        
                        .self$B_ridge<<-B_ridge_a
                        .self$y_h<<-y_h_a
                      },
                      predict=function(newdata){
                        x_m<-model.matrix(formula,newdata)
                        x_m[,-1]<-(scale(x_m[,-1]))
                        y_f<-x_m %*% B_ridge
                        return(y_f)
                        },
                      
                      #returns coefficients(estimators)
                      coef=function(){
                      return(B_ridge)},
                      
                      show=function(){
                        cat('call:')
                        cat(sep='\n')
                        cat(paste('ridgereg(formula = ',format(formula),', ' ,'data = ',data_name ,')\n\n',sep=''))
                        cat('Coffiecients:\n')
                        print.default(format(B_ridge),
                                      print.gap = 2L,quote=FALSE)}
))

# ridgereg_obj1<-ridgereg$new(formula=medv~.,data=training_set,lamda=4)
# ridgereg_obj1
# ridgereg_obj1$predict(test_set)
# ridgereg_obj1$coef()
# print(ridgereg_obj)

