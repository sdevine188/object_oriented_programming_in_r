
# object-oriented programming in R with S3 classes

# https://www.cyclismo.org/tutorial/R/s3Classes.html

# create list object
good_guy_test <- list(first = "one", second = "two", third = "three")
class(good_guy_test)

# add a class
class(good_guy_test) <- append(class(good_guy_test), "good_guy")
class(good_guy_test)

# define a function, which calls UseMethod to look for same-named methods on an object
# this tells R, look for a defined get_status method for one of the classes of object x
# in this case, it's looking for a defined get_status method for class list or class good_guy
get_status <- function(x) {
        UseMethod("get_status", x)
}

# this is where we define the get_status method for class good_guy (methods are defined using function.class syntax)
get_status.good_guy <- function(x) {
        return(x$first)
}

# call get_status
get_status(good_guy_test)


################################################


# create bad_guy_test list
bad_guy_test <- list(first = "one", second = "two", third = "three")
bad_guy_test

# add bad_guy class
class(bad_guy_test) <- append(class(bad_guy_test), "bad_guy")
class(bad_guy_test)

# try calling get_status on bad_guy_test fails initially because UseMethod finds no defined get_status method for class list or class bad_guy
get_status(bad_guy_test)

# define get_status method for class bad_guy
get_status.bad_guy <- function(x) {
        return(x$third)
}

# call get_status function
get_status(bad_guy_test)


#####################################################


# note that if for some reason the object has both classes (eg good_guy and bad_guy), UseMethod will use method for first class it matches

# create good_and_bad_guy_test list
good_and_bad_guy_test <- list(first = "one", second = "two", third = "three")
good_and_bad_guy_test

# add good_guy and bad_guy class
class(good_and_bad_guy_test) <- append(class(good_and_bad_guy_test), "good_guy")
class(good_and_bad_guy_test)

class(good_and_bad_guy_test) <- append(class(good_and_bad_guy_test), "bad_guy")
class(good_and_bad_guy_test)

# call get_status function
get_status(good_and_bad_guy_test)


#######################################################
#######################################################
#######################################################


# build function to create instances of a class
good_guy <- function(saves_girl = TRUE, has_lair = FALSE, special_power) {
        
        # create instance_of_class
        instance_of_class <- list(saves_girl = saves_girl, has_lair = has_lair, special_power = special_power)
        
        # set name for class
        class(instance_of_class) <- append(class(instance_of_class), "good_guy")
        
        # return instance_of_class
        return(instance_of_class)
}

# need to specify special_power attribute
super_man <- good_guy()

super_man <- good_guy(special_power = "everything")
super_man
super_man$saves_girl

wonder_woman <- good_guy(saves_girl = FALSE, special_power = "lasso")
wonder_woman
wonder_woman$saves_girl

# build bad_guy class
bad_guy <- function(saves_girl = FALSE, has_lair = TRUE, special_power) {
        
        # create instance_of_class
        instance_of_class <- list(saves_girl = saves_girl, has_lair = has_lair, special_power = special_power)
        
        # set name for class
        class(instance_of_class) <- append(class(instance_of_class), "bad_guy")
        
        # return instance_of_class
        return(instance_of_class)
}

doomsday <- bad_guy(special_power = "indestructible")
doomsday


##########################################################


# build function to set attributes of class
set_saves_girl <- function(x, value) {
        UseMethod("set_saves_girl", x)
}

# defining default method handles situations where method does not exist for any of the object's classes
set_saves_girl.default <- function(x, value) {
        print("cannot find specified method for any of the object's classes")
        return(x)
}

citizen <- list(ordinary_person = TRUE)
citizen
set_saves_girl(x = citizen, value = FALSE)

# define set_saves_girl method for good_guy class
set_saves_girl.good_guy <- function(x, value) {
        print("setting saves_girl for good_guy class")
        x$saves_girl <- value
        return(x)
}

# just calling the function will only return the revised object
set_saves_girl(x = super_man, value = FALSE)

# need to overwrite the object with the revised onject to keep changes
super_man <- set_saves_girl(x = super_man, value = FALSE)
super_man

wonder_woman <- set_saves_girl(x = wonder_woman, value = TRUE)
wonder_woman


############################################################


# build function to get attributes of class
get_saves_girl <- function(x) {
        UseMethod("get_saves_girl", x)
}

# defining default method handles situations where method does not exist for any of the object's classes - return NULL
get_saves_girl.default <- function(x) {
        print("cannot find specified method for any of the object's classes")
        return(NULL)
}

# define get_saves_girl method for good_guy class
get_saves_girl.good_guy <- function(x) {
        print("getting saves_girl attribute for good_guy class")
        return(x$saves_girl)
}

# call get_saves_girl for object where the method does not exist for any of the object's classes
get_saves_girl(x = citizen)

# call get_saves_girl
get_saves_girl(x = super_man)
get_saves_girl(wonder_woman)


#####################################################################


# use class inheritance to define taxonomy of classes inheriting attributes from higher-level ancestor classes
x_men <- function(saves_girl = TRUE, has_lair = FALSE, special_power) {
        
        # create instance_of_class
        instance_of_class <- good_guy(saves_girl = saves_girl, has_lair = has_lair, special_power = special_power)
        
        # set name for class
        class(instance_of_class) <- append(class(instance_of_class), "x_men")
        
        # return instance_of_class
        return(instance_of_class)
}

# create objects with class x_men
wolverine <- x_men(special_power = "claws")
wolverine

storm <- x_men(saves_girl = FALSE, special_power = "weather")
storm


# create axis_of_villains class
marvel_villain <- function(saves_girl = FALSE, has_lair = FALSE, special_power) {
        
        # create instance_of_class
        instance_of_class <- bad_guy(saves_girl = saves_girl, has_lair = has_lair, special_power = special_power)
        
        # set name for class
        class(instance_of_class) <- append(class(instance_of_class), "marvel_villain")
        
        # return instance_of_class
        return(instance_of_class)
}

# create objects with class axis_of_villain
magneto <- marvel_villain(special_power = "magnetism")
magneto


########################################################################


# define a method to make use of the class inheritance feature by using NextMethod function to see if next class has the method defined
is_lucrative <- function(x) {
        UseMethod("is_lucrative", x)
}

is_lucrative.default <- function(x) {
        print("object does not have a defined is_lucrative method for any of its classes")
        return(x)
}

is_lucrative.good_guy <- function(x) {
        print("good guys are likely to sell more toys")
        NextMethod("is_lucrative", x)
        return(x)
}

is_lucrative.x_men <- function(x) {
        print("x_men are highly popular based on comics and movies")
        NextMethod("is_lucrative", x)
        return(x)
}

is_lucrative.bad_guy <- function(x) {
        print("bad guys are likely to sell less toys")
        NextMethod("is_lucrative", x)
        return(x)
}

is_lucrative.marvel_villain <- function(x) {
        print("marvel villains get top-notch actors to star in movies")
        NextMethod("is_lucrative", x)
        return(x)
}

# call is_lucrative method
super_man <- is_lucrative(super_man)
super_man
is_lucrative(wonder_woman)
is_lucrative(wolverine)
is_lucrative(storm)
is_lucrative(magneto)
is_lucrative(citizen)


