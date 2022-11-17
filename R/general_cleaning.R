#' A function to detach all packages
#' 
#' Primarily useful for checking replication, or when switching between things you're working on and you're not using R Projects.
#' Credit to: https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
#' 
#' @export
detach_all_packages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

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


#' Get RSEI data that is consistent for a time series
#'
#' @param rsei The dataset as read in from the Duke Box folder `raw/rsei`. No changes made, or else output isn't guaranteed, MUST be passed as a data.frame. It expects the data to have the columns c("geoid", "releasenum", "ChemicalNumber", "facilitynum", "media", "conc", "toxconc", "score", "scorecancer", "scorenoncancer", "pop")
#' @param chemicals The dataset as read in from the Duke Box folder `raw/rsei/chemical_data_rsei_v2310.csv`. MUST be passed as a data.frame.
#' @param starting_year The year you want to start having a consistent dataset from. The function will only return data for the year you've inputed, but will drop certain data based on the year you input here.
#' @return a dataset of RSEI data, with certain data filtered out based on your `starting_year` of choice. 
#' @export
clean_timeseries_RSEI = function(rsei = NA, chemicals = NA, facility = NA, 
                                 starting_year = 2011) {
  
  # do checks on inputs to make sure we have everything we need before we start running
  if (starting_year <= 1998 & is.data.frame(facility)) {
    stop("You must include the facility table!! If year <= 1998, we need to filter out seven industry groups that weren't required to report before that year. If we don't filter them out, we'll see an articial jump in toxicity in 1998 onward, the data will not be comprable to pre 1998.")
  } else if (!is.data.frame(rsei) | !is.data.frame(chemicals)) {
    stop("The RSEI and Chemicals inputs are both required to create a RSEI timeseries.")
  } 
  
  # give all the cols easy to type names
  names(rsei) = c("geoid", "releasenum", "ChemicalNumber", "facilitynum", 
                  "media", "conc", "toxconc", "score", "scorecancer", 
                  "scorenoncancer", "pop")
  
  # subset the chemicals data to the best core set for your chosen set
  cutoff_years = c(1988, 1995, 1998, 2000, 2001, 2011)
  choice_year = max(which(cutoff_years - starting_year <= 0)) # which is the last that is before your starting year
  choice_year = cutoff_years[choice_year]
  warning(paste("Using consistent chemical set from", choice_year, "onwards."))
  choice_year = paste0("Core", substr(choice_year, 3, 4), "ChemicalFlag")
  chemicals = chemicals %>%
    filter((!!as.name(choice_year)) == 1) %>%
    select(ChemicalNumber)
  
  # merge w/ chemicals, keeping only the obs that are in chemicals
  rsei = rsei %>%
    select(geoid, ChemicalNumber, conc, toxconc) %>%
    filter(ChemicalNumber %in% chemicals$ChemicalNumber)
  
  if (starting_year <= 1998) {
    # drop facilities that don't report pre 1998
    facility = facility %>% 
      filter(!NewIndustryFlag | is.na(NewIndustryFlag)) %>% 
      mutate(FacilityNumber = as.integer(FacilityNumber))
    rsei = rsei %>% filter(facilitynum %in% facility$FacilityNumber)
  }
  
  # add up all the toxicities within a block group
  rsei_agg = rsei %>%
    group_by(geoid) %>%
    summarise(toxconc = sum(toxconc))
  
  return(rsei_agg)
}

