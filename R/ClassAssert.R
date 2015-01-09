# -- CONSTRUCTOR
assert <- function(){
  return(new("Assert"))
  # -- CLASS
}
#' Class Assert
#' 
#' Description
#' 
#' @slot result Description
#' 
setClass( Class = "Assert",
          representation( result = "data.frame" ),
          validity = function(object){ 
            if(TRUE){
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
)
# -- INITIALIZER
setMethod( f = "initialize",
           signature = "Assert",
           definition = function(.Object, 
                                 result){ 
             # -- Set the attibutes with the defaults
             .Object@result <- data.frame()
             # -- Class inspection
             validObject(.Object)
             return(.Object) }
)
# -- SETTER
#' Method Assert.setResult
#' 
#' Description.
#' 
#' @param object Description.
#' @param value Description.
#' @return object Description.
#' @examples
#' Assert.setResult()
#' @export
#' 
setGeneric("Assert.setResult<-",
           function(object, value){standardGeneric("Assert.setResult<-")})
setReplaceMethod(f = "Assert.setResult",
                 signature = "Assert",
                 definition = function(object, value){
                   # -- Add a new line to the results
                   object@result<-rbind(object@result, value)
                   return(object) }
)
# -- OTHERS
#' Method Assert.summary
#' 
#' Description.
#' 
#' @param object Description.
#' @examples
#' Assert.summary()
#' @export
#' 
setGeneric("Assert.summary",
           function( object ){standardGeneric("Assert.summary")})
setMethod( "Assert.summary", "Assert",
           function( object ){
             
             result <- object@result
             
             # -- Check the methods
             meth <- table(result$method)
             exec_methods <- length(meth)
             # -- Check the status
             status <- table(result$status)
             # -- Count the Success and Failures
             exec_success  <- status["S"]
             exec_fail     <- status["F"]
             # -- Avoid NA values
             if( is.na(exec_success) ) {
               exec_success <- 0
             }else{}
             # --
             if( is.na(exec_fail) ){
               exec_fail <- 0 
             }else{
               
             } 
             
             exec_total <- nrow(result)
             # -- Print summary
             cat("<<< UNIT TESTING SUMMARY >>>", "\n")
             cat("You tested ", exec_methods,"different methods. \n")
             cat("The final result of the execution was:", "\n")
             cat("Succeed [", exec_success," | ", exec_fail, "] Failed", "\n")
             cat("Total   [", exec_total  ,"]", "\n")
             
             cat("<<< UNIT TESTING REPORT >>>", "\n")
             if(exec_fail <= 0){
               message("\\o/ Congrats, all tests successfully executed.")
             }else{
               # -- Failure report
               cat("Status Method"  , "\n")
               for( r in 1: nrow(result)){
                 cat(result[r,2], "....", r,".",result[r,1], "\n")
                 # -- Details only for status F (Fail)
                 if(result[r,2] == "F"){
                   cat("...... - ",  result[r,5] , "\n")
                   cat("...... - ", "Expected [", result[r,3], " | ", result[r,4], "] Actual", "\n")  
                 }else{
                   cat("...... + ", "\n")
                 }
               }
             }
             cat("<<< END OF UNIT TESTING >>>", "\n")
           }
) 
#' Method Assert.equals
#' 
#' Description.
#' 
#' @param object Description.
#' @param meth Description.
#' @param act Description.
#' @param exp Description.
#' @return result Description.
#' @examples
#' Assert.equals()
#' @export
#' 
setGeneric("Assert.equals",
           function( object, meth, act, exp ){standardGeneric("Assert.equals")})
setMethod( "Assert.equals", "Assert",
           function( object, meth, act, exp ){
             
             result  <- data.frame()
             msg     <- ""
             stat    <- ""
             supported <- TRUE
             
             typ_act <- typeof(act)
             typ_exp <- typeof(exp) 
             
             # -- Threatment for unsupported comparisons
             # -- lists are not supported
             if(typ_act == "list"){ 
               supported <- FALSE 
               msg       <- "Check not supported."
               stat      <- "F"
               act <- exp <- ""
             }else{supported <- TRUE}
             
             # -- Execute the comparison
             # -- Check the types
             if(supported == TRUE){
               if(typ_act == typ_exp){
                 # -- Check the sizes
                 if(length(act) == length(exp)){
                   # --- Check content
                   if(act == exp){
                     stat <- "S"
                   }else{
                     msg  <- "Not the same value/content."
                     stat <- "F"
                   } # -- Content
                 }else{
                   msg  <- "Not the same size."
                   stat <- "F"
                 } # -- Size
               }else{
                 msg  <- "Not the same type."
                 stat <- "F"
               } # -- Type
             }else{ }
             # -- Store the current result
             result <- data.frame(method  = meth,
                                  status   = stat,
                                  expected = exp,
                                  actual   = act,
                                  message  = msg,
                                  stringsAsFactors = FALSE)
             return(result)
           }
)
#' Method Assert.dFrameEquals
#' 
#' Description.
#' 
#' @param object Description.
#' @param meth Description.
#' @param act Description.
#' @param exp Description.
#' @return result Description.
#' @examples
#' Assert.dFrameEquals()
#' @export
#' 
setGeneric("Assert.dFrameEquals",
           function( object, meth, act, exp ){standardGeneric("Assert.dFrameEquals")})
setMethod( "Assert.dFrameEquals", "Assert",
           function( object, meth, act, exp ){
             
             stat <- "S"             
             msg  <- ""
             
             exp_lines   <- 0
             exp_columns <- 0
             exp_value   <- ""
             exp_type    <- ""
             
             act_value   <- ""
             act_type    <- ""
             
             #--- Only data.frame possible
             if(is.data.frame(act) && is.data.frame(exp)){
               act_size <- length(act)
               exp_size <- length(exp)
               #--- Check the sizes
               if(act_size == exp_size){
                 exp_lines   <- nrow(exp)
                 exp_columns <- ncol(exp)
                 #--- Compare data.frame content
                 for(l in 1 : exp_lines){
                   for(c in 1 : exp_columns){
                     #--- Avoid NA values
                     if(!is.na(exp[l, c])){
                       exp_value <- as.character(exp[l,c])
                     }else{
                       exp_value <- ""
                     }
                     #--- Avoid NA values
                     if(!is.na(act[l,c])){
                       act_value <- as.character(act[l,c])
                     }else{
                       act_value <- ""
                     }
                     exp_type <- typeof(exp[l,c])
                     act_type <- typeof(act[l,c])
                     #--- Check the types
                     if( act_type == exp_type){ 
                       #--- Finally compare the content
                       if( exp_value == act_value ){
                         #--- If okay it is already set as Success
                         TRUE 
                       }else{ 
                         #--- Contents are not the same
                         stat <- "F"
                         msg  <- paste0("Position ", l, "x", c, " not the same.")
                       }                     
                     }else{ 
                       #--- Contents are from differnet types
                       stat <- "F"
                       msg  <- paste0("Position ", l, "x", c, " not the same.")
                     }
                   } #--- Column loop
                 } #--- Line loop
               }else{ 
                 #--- Different sizes
                 stat <- "F"
                 msg  <- "Expected and Actual with different sizes." 
               }
             }else{ 
               #--- "Other than a data.frame
               stat <- "F"
               msg  <- "Not a data.frame." 
             }
             act <- ""
             exp <- ""
             # -- Store the current result
             result <- data.frame(method   = meth,
                                  status   = stat,
                                  expected = exp,
                                  actual   = act,
                                  message  = msg,
                                  stringsAsFactors = FALSE)                          
             return(result)
           }
)

#' Method Assert.listEquals
#' 
#' Description.
#' 
#' @param object Description.
#' @param meth Description.
#' @param act Description.
#' @param exp Description.
#' @return result Description.
#' @examples
#' Assert.listEquals()
#' @export
#' 
setGeneric("Assert.listEquals",
           function( object, meth, act, exp ){standardGeneric("Assert.listEquals")})
setMethod( "Assert.listEquals", "Assert",
           function( object, meth, act, exp ){
             
             stat <- "S"
             msg  <- ""
             
             actual    <- unlist(act)
             act_size  <- length(unlist(act))
             act_value <- ""
             act_type  <- ""
             act_ret   <- ""
             
             expected  <- unlist(exp)
             exp_size  <- length(unlist(exp))
             exp_value <- ""
             exp_type  <- ""
             exp_ret   <- ""
             
             #--- Only list are accepted
             #--- Check the type
             if(is.list(act)){ 
               #--- Compare sizes
               if(act_size == exp_size){ 
                 #--- Compare the content
                 for(l in 1 : exp_size){
                   
                   exp_value <- (expected[l])
                   exp_type  <- typeof(exp_value)
                   act_value <- (actual[l])
                   act_type  <- typeof(act_value)
                   #--- Ignore NAs
                   if(is.na(act_value) | is.na(exp_value)){
                     #--- Do nothing
                   }else {
                     #--- Compare the variable types
                     if( exp_type == act_type ){
                       #--- Compare the content
                       if( exp_value == act_value ){
                         # -- Okay, return is already set to Success
                         TRUE
                       }else{
                         #--- Content is not the same
                         stat    <- "F"
                         msg     <- paste0("Position ", l, " not the same content.")
                         act_ret <- act_value
                         exp_ret <- exp_value                      
                       } #--- If value                 
                     }else{
                       #--- Types are not the same
                       stat    <- "F"
                       msg     <- paste0("Position ", l, " not the same type.")
                       act_ret <- act_type
                       exp_ret <- exp_type                      
                     } #--- If Type
                   }#--- Not NA
                 } # For
               }else{ 
                 #--- If size
                 stat <- "F"
                 msg  <- "Expected and Actual with different sizes." 
                 act_ret <- act_size
                 exp_ret <- exp_size
               }
             }else{ 
               #--- If other than list
               stat <- "F"
               msg  <- "Parameter is not a list." 
             }
             # -- Store the current result
             result <- data.frame(method   = meth,
                                  status   = stat,
                                  expected = exp_ret,
                                  actual   = act_ret,
                                  message  = msg,
                                  stringsAsFactors = FALSE)                          
             return(result)
           }
)

#' Method Assert.differs
#' 
#' Description.
#' 
#' @param object Description.
#' @param meth Description.
#' @param act Description.
#' @param exp Description.
#' @return result Description.
#' @examples
#' Assert.Assert.differs()
#' @export
#' 
setGeneric("Assert.differs",
           function( object, meth, act, exp ){standardGeneric("Assert.differs")})
setMethod( "Assert.differs", "Assert",
           function( object, meth, act, exp ){
             
             result  <- data.frame()
             msg     <- ""
             stat    <- ""
             supported <- TRUE
             
             typ_act <- typeof(act)
             typ_exp <- typeof(exp)
             
             # -- Threatment for unsupported comparisons
             # -- lists are not supported
             if(typ_act == "list"){ 
               supported <- FALSE 
               msg       <- "Check not supported."
               stat      <- "F"
               act <- exp <- ""
             }else{
               supported <- TRUE
             }            
             # -- Execute the comparison            
             # -- Check the types
             if(supported == TRUE){
               if(typ_act == typ_exp){
                 # --- Check content
                 if(act != exp){
                   stat <- "S"
                 }else{
                   msg  <- "Same value/content."
                   stat <- "F"                
                 }
               }else{
                 msg  <- "Not the same type."
                 stat <- "F"                
               }
             } else {
               
             }
             # -- Store the current result
             result <- data.frame(method   = meth,
                                  status   = stat,
                                  expected = exp,
                                  actual   = act,
                                  message  = msg,
                                  stringsAsFactors = FALSE)
             return(result)
           }
)