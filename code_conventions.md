# Social Charging code conventions

## Notation and naming
### File names
File names should be meaningful and end in .R.
```
# Good
fit-models.R
utility-functions.R

# Bad
foo.r
stuff.r
```
### Object names
Variable and function names should be **camel-case**. Generally, variable names should be nouns and function names should be verbs. Strive for names that are concise and meaningful.
```
# Good
dayOne
doSomething()

# Bad
first_day_of_the_month
DayOne
dayone
djm1
```
Where possible, **avoid using names of existing functions and variables**. Doing so will cause confusion for the readers of your code.
```
# Bad
T <- FALSE
c <- 10
mean <- function(x) sum(x)
```
## Comments
### Goal of file
A file's first line should be a comment describing the goal of the script.
```
# This script plots smart_charging against charged_kwh.
...
...
...
```

### Comment sections
Comment sections should be used as in template.R.
These can be created with the shortcut `ctrl/cmd + shirt + r` or in the following way:
```
# This is a collapsable section ----
```
(The four dashes are mandatory)

See template.R in `socialcharging/src` for comment sections.

### Function documentation
A comment above the function should describe the goal of it.
```
# Returns a plot of something.
plotSomething <- function() {
    ...
}
```

## Syntax
### Spacing
Place spaces around all infix operators (=, +, -, <-, etc.). The same rule applies when using = in function calls. Always put a space after a comma, and never before (just like in regular English).
```
# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)
```
There’s a small exception to this rule: :, :: and ::: don’t need spaces around them.
```
# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get
```
Place a space before left parentheses, except in a function call.
```
# Good
if (debug) do(x)
plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)
```
Do not place spaces around code in parentheses or square brackets (unless there’s a comma, in which case see above).
```
# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before
```
### Curly braces
An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it’s followed by else.

Always indent the code inside curly braces.
```
# Good

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
message("Y is negative")

if (y == 0) {
  log(x)
}
else {
  y ^ x
}
```
### Line length
Strive to limit your code to **80 characters per line.** This fits comfortably on a printed page with a reasonably sized font. If you find yourself running out of room, this is a good indication that you should encapsulate some of the work in a separate function.

### Indentation
When indenting your code, use **two spaces**. Never use tabs or mix tabs and spaces.

The only exception is if a function definition runs over multiple lines. In that case, indent the second line to where the definition starts:
```
long_function_name <- function(a = "a long argument",
                               b = "another argument",
                               c = "another long argument") {
  # As usual code is indented by two spaces.
}
```
### Assignment
Use <-, not =, for assignment.
```
# Good
x <- 5
# Bad
x = 5
```

### Return values
A function can return both explicitly and as the last statement in the function
```
# Good
a <- function(x) {
    x
}

b <- function(x) {
    return(x)
}
```

## Template
Use template.R's comment style where possible (see `socialcharging/src`).
