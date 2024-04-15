# R Code Style Guide

This style guide emphasis writing R code that other programmers can edit. 

##General

- When coding, emphasize accessibility for readers over brevity, trendiness, and speed. Given our work environment, even if you are working alone on a project, someone else should be able to take over your code. You may be surprised how often you return to your own code to discover you feel like a different person than the person who wrote it. 
  - Inline comments should, at a minimum give a general outline of what is going on.
  - Any tricky / potentially confusing code should have comments explaining what is going on.
  - If you do optimize something and make it more difficult to read, it can help to keep the original code and indicate that it achieves the same goal.
- For functions you expect others to use (exported functions in a package) write complete documentation, including: 
  - At least one working example of every function (excepting trivial functions such as print functions). 
    - The coverage of examples included should reviewed by another programmer who did not program the function. This provides independent validation. 
  - A complete "returns" section in the R documentation that specifies the class and meaning of the returned object. 
    - When that object has a data.frame or matrix, the column names and meanings are described.
    - When that object has a list, each element of that list is described with details similar to if it was the only thing being returned.
    - When the function returns nothing (e.g. a print function) this section can be omitted.

##The five types of functions their placement in code

1. Main exported functions. These are the functions that people use the package to get access to (e.g. `lm.sdf`). Put these in a .R file named [functionname].R
2. Helper exported functions. These are functions that make the main functions work (e.g. `print.lm.sdf`). These are placed in [functionname]Helpers.R or [functionname].R at the programmers discretion.
3. Non-exported exclusive “subroutines.” These are code chunks that you remove from code because you either repeat a task or want to isolate some code for another reason. These get used in calls from exactly one function. Place these in [functionname.R]
4. Functions similar to those in 2 except that the functions are used by multiple functions. These are treated similar to main functions. functions of this type should include documentation.
5. R S3/S4 OO calls, e.g. calls to setMethod. These are either placed in functionname.R or all.R or zzz.R when required to do so by CRAN/the compiler or when it is not clear which file they should go in.

##Variables:
- All function names and arguments are in camel case (`likeThis`) 
  - However function names use dots when required to do so by the R specification (e.g. `print.objectName`)
- Inside functions, shorter variable names can be used but the logic should be explained when not immediately apparent (e.g. # `od` is short for "original data")

##Type setting: 
- Indenting is accomplished with two spaces
- Opening a function call or conditional leads to an increase of one level indenting
- If/else statements always use curly braces, e.g., {}, and appear on multiple lines
- Open curly braces appear on the same line as the function / conditional that they open and are closed on their own line e.g.
```
if(foo) {
  [code]
} else {
  [more code]
}
```

##Details
- Use `TRUE/FALSE` in preference to `T/F` because `T` and `F` can be redefined and yield surprising results.
- Avoid using other packages when a minor bit of functionality is required. This decreases issues due to others functions having surprising changes in their API that cause our package to malfunction.
- All long conditionals should indicate, when closed, what statement is being closed e.g.
```
if(bar) {
  [a bunch of code]
} else { # end if(bar) 
  [more code]
} # end else if(bar)
```


