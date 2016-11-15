source("setup.R")
require(triangle)

setwd("~/Documents/CUNY/Simulation_604/Final_proj")

# standard setup params: 1000, 50, 36, 0.1, 0.05
set.seed(1234)
data = setup(1000,50,36,0.1,0.05)
books = data$books
shelves = data$shelves

# define some methods for each step of the sim...

# order of methods: subtractive methods first, then additive:
# weeding, checkouts, dedups, checkins, purchases, update_shelves()

# called to add new books by cross-referencing the shelves df
# also updates the shelves dataframe 
update_shelves = function(books, shelves, new_texts, new_non_texts){
    
    
}



book_width = function(a,b){        # utility returns book width between a and b for single book
    rtriangle(1, a, b, (a + b)/b )
}


# round of weeding - 2% of the collection -> DO NOT weed books that have been checked out
weeding = function(books){
    n_to_pull = round(nrow(books) * .02) # number of rows to randomly pull
    books = books[-sample(which(!books$chkdout), n_to_pull), ] # pull em, update and return
    return(books)
}


# set of purchases - 5 to 25 books (simulated book widths)
# set of textbook purchases (simulated book widths) - 2 copies of 3 to 5 books
# set flag to 'n', 't' for text-book or non-textbook
# return a new dataframe to be appended to books in outer scope
purchase_non_textbooks = function(){
    n_books = round(runif(1,5,26))
    book_type = 'n'
    
    
    return(purchase_order)
} 

purchase_textbooks = function(){
    
    
    return(purchase_order)
}


# round of de-duplicating (textbooks)
de_dup = function(books){}

# 2% of the collection gets newly checked out -> check flag
check_outs = function(books){}

# 2% of the collection gets checked in (not the same as the check outs)
check_ins = function(books){}