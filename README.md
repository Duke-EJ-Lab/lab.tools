# lab.tools

Please add any functions you think might be useful to the lab at large to this repo!

To "make" this package, and make sure that your functions are included, you'll need 2 packages. 

```
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
```

After you've installed the packages there's only **three steps** before everyone can reuse your code! 

**1) Add the functions you want.** If there's an existing file that seems like a reasonable place to put it (eg. if you're adding a plotting function throw it in to the plotting.R file) you can just add to it. If not, just make a new file and name it whatever feels right. 

**2) Add documentation!!** This chunk of text should directly precede the function you've defined. This step is incredibly important so that other people can understand what your function does. It's got a few parts: 
* a title, 
* a description that gives a little more information on how the function works 
* **@import** a way to import any packages needed for that function to work. A better practice (if you can figure out specifically which functions are needed) is to use @importFrom package function. This reduces warnings and errors if multiple packages imported have functions of the same name.
* **@param** which defines what each parameter means
* **@return** which lets the user know what the function will return
* **@export** which lets the package know that it's external facing and a user should be able to use this function after importing the library. Probably the only time you wouldn't include export is if this is a helper function that you don't want users to be able to touch.
* **@examples** which shows different use cases/potential inputs. 

Here's an example of some documentation (for a very silly function).

```
#' Adding function
#'
#' This function adds the two numbers passed to it. One number defaults to two if only one variable is given. 
#' @import ggplot
#' @param x The first variable. Defaults to 2
#' @param y The second variable
#' @return The value of x+y 
#' @export
#' @examples
#' adding_function(7)
#' adding_function(1, 1)
adding_function = function(x = 2, y){
    return(x+y)
}
```

**3) "Process" the documentation.** This is what we installed those packages for! Set the working directory to the folder above your package and run

```
document("lab.tools")
```

You'll know it worked if there's a new (or updated) file in the "man" folder with the name of your function. These are the files that allow you to call things like `?adding_function` from the console.

Now the package is ready to be installed! Make sure to commit your changes to GitHub!

Once your changes are committed to GitHub, anyone in the lab should be able to install the package either by :
* cloning the repository locally and running `install("lab.tools")` while their working directory is directly in the folder above
* using a devtools function to pull directly from github: `install_github('Duke-EJ-Lab/lab.tools', auth_token="XXXX")`. Here's a doc that can help you to [create your auth_token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token), beware that it will only show it once so be sure to save it somewhere (that's not saved on GitHub!)

If you want more info on building packages Hilary Parker wrote a [short and sweet tutorial](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) with more information than this, and there's a much much more [in depth report/book](https://r-pkgs.org/) by the icons Hadley Wickham and Jenny Bryan (the chief scientists @ RStudio) on creating packages. 