#Installed packages: library(stringr), library(ggplot2)

  
#' Linear Regression
#'
#' @field formula defines a formula of how we want to model the relationship between different variables. 
#' @field data It is a dataframe containing multiple variables in the columns and different observations of those variables in the rows.  
#' @field coeff Regressions coefficients. 
#' @field fitted The fitted values.  
#' @field residuals The residuals.  
#' @field dof The degrees of freedom.  
#' @field rvariance The residual variance. 
#' @field varcoeff The variance of the regression coefficients. 
#' @field tvalues The t-values for each coefficient.  
#' 
#' 
#' @return A class with multiple fields and methods to use on. 
#' @export
#'
#' 
  linreg<-setRefClass("linreg",
                      fields = list(formula="formula",data="data.frame",d = "character",X="matrix",y="vector",coeff="matrix",fitted="matrix",
                                    residuals="matrix", dof="numeric", rvariance="matrix", varcoeff="matrix", tvalues="matrix"),
                      methods=list(
                        initialize = function(formula,data){
                          #.self$formula <<-formula
                          #.self$data <<- data
                          formula <<- formula 
                          d<<-deparse(substitute(data))
                          X <<- model.matrix(formula,data)
                          y <<- data[[all.vars(formula)[1]]]
                          coeff <<- solve((t(X)%*%X))%*%t(X)%*%y 
                          fitted <<- X%*%coeff
                          residuals <<- y - fitted
                          dof <<- nrow(data)-ncol(data)-1
                          rvariance <<- (t(residuals)%*%residuals)/dof
                          varcoeff <<- as.numeric(rvariance)* solve(t(X)%*%X)
                          tvalues <<- coeff/sqrt(diag(varcoeff))
                          #how to find p-values using pt()?Is that something that needs to be calculated?  
                        },
                        print = function(){
                          a <- as.vector(coeff)
                          names(a)<-row.names(coeff)
                          e<- as.character(formula)
                          e<-paste(e[2],e[3])
                          c<- stringr::str_glue("lm(formula = {e}, data = {d})")
                          b<-list(Call = c , Coefficients = a)
                          return(b)
                        },
                        plot = function(){
                          a<- as.data.frame(residuals)
                          b<- as.data.frame(fitted)
                          c<-cbind(a,b)
                          names(c)<-c("Residuals","Fitted")
                          e<- as.character(formula)
                          e<-paste(e[2],e[3])
                          d<<- stringr::str_glue("Fitted values lm({e})", .sep = "\n")
                          ggplot2::ggplot(c,aes(Fitted,Residuals))+geom_point()+labs(title="Residuals vs Fitted", x = d)
                        },
                        resid = function(){
                          return(residuals)
                        },
                        pred = function(){
                          return(fitted)
                        },
                        coef = function(){
                          a <- as.vector(coeff)
                          names(a)<-row.names(coeff)
                          return(a)
                        },
                        summary = function(){
                          a <- as.vector(coeff)
                          names(a)<-row.names(coeff)
                          e<- as.character(formula)
                          e<-paste(e[2],e[3])
                          c<- stringr::str_glue("lm(formula = {e}, data = {d})")
                          b<-list(Call = c , Coefficients = a, tvalue = tvalues, p_value = " " ,StandardDev = rvariance, DegreeOfFreedom = dof)
                          return(b)
                        }
                      )
                      )
  
            
  
  #l1<-linreg(formula = Petal.Length~Species + Petal.Width, data = iris)
