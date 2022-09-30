#' A silly function to let you know when your code is done running. 
#' 
#' All this function does is tell you that your code is done running and beeps. Put this line at the end of a long function so you can know your code is done. Ideally will never be used because our code is so efficient and doesn't require human intervention :)
#'
#' @param x time, in seconds
#' @param text What the computer will say. Input "" for just beeps.
#' @export
beep_alert = function(x=0.8, text="Just finished running!") {
  system(paste("say",  text))
  beep(); Sys.sleep(x); beep(); Sys.sleep(x);  beep()
}


#' Wrapper for merge that is verbose
#' 
#' Lets you know how many rows were dropped if all=F, and how many rows are all NA's if all=T
#'
#' @param x,y data frames, or objects to be coerced to one.
#' @param by,by.x,by.y specifications of the columns used for merging. See ‘Details’.
#' @param all logical; all = L is shorthand for all.x = L and all.y = L, where L is either TRUE or FALSE.
#' @param all.x logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. 
#'              These rows will have NAs in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output.
#' @param all.y logical; analogous to all.x.
#' @param sort logical. Should the result be sorted on the by columns?
#' @param suffixes a character vector of length 2 specifying the suffixes to be used for making unique the names of columns in the result which are not used for merging (appearing in by etc).
#' @return A data frame. The rows are by default lexicographically sorted on the common columns, but for sort = FALSE are in an unspecified order. The columns are the common columns followed by the remaining columns in x and then those in y. If the matching involved row names, an extra character column called Row.names is added at the left, and in all cases the result has ‘automatic’ row names.
#' @export
merge_verbose = function(x, y, by, by.y=by, by.x=by, all=F, all.x=all, all.y=all, sort=T, suffixes=c(".x", ".y"), no.dups=T) {
    
    new = merge(x, y, by, by.x, by.y, all, all.x, all.y, sort, suffixes, no.dups)
    
    x_names = names(new)[names(new) %in% names(x)]
    x_names = c(x_names, names(new)[grepl(".x", names(new))])
    y_names = names(new)[names(new) %in% names(y)]
    y_names = c(x_names, names(new)[grepl(".y", names(new))])
    
    nas_y = rowSums(is.na(new[, x_names]))
    nas_y = sum(nas_y == length(x_names))
    
    nas_x = rowSums(is.na(new[, y_names]))
    nas_x = sum(nas_x == length(y_names))
    
    if (nas_x > 0)  {
        print(sprintf("There are %i rows in x that didn't have match in y.", nas_x))
    }
    if (nas_y > 0) {
        print(sprintf("There are %i rows in y that didn't have match in x.", nas_y))
    }
    if (nrow(new) != nrow(x)) {
        print(sprintf("There are %i more rows in new dataset than in x.", nrow(new)-nrow(x)))
    }
    
    return(new)
}
