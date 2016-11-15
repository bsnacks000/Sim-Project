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

# special logic for appending a textbook df to main library df
add_textbooks = function(new_books){
    
}

#speical logic for appending a non-textbook df to main library df
add_non_textbooks = function(new_books){
    
}

book_widths = function(n,a,b){        # utility returns book widths between a and b for n books
    rtriangle(n, a, b, (a + b)/b )
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
# this is a blank order... all field's build/add logic is done in the update_shelves() function
purchase_books = function(flag){
    
    if (flag=='n')      # branch for text/nontext -- R throws an exception if not 'n /'t'
        n_books = round(runif(1,5,26))
    else if (flag=='t')
        n_books = round(runif(1,3,6))
    
    width = book_widths(n_books,1,2) # calls utility function book_widths()
    btype = rep(flag,n_books)       # sets flags
    
    book_id = copym = dupeof = shelf_id = chkdout = dedupe = rep(0,n_books)
    purchase_order = data.frame(
        book_id, btype,copym,dupeof,shelf_id,width,chkdout,dedupe
    )
    
    return(purchase_order)
} 


# round of de-duplicating (textbooks)
de_dup = function(books){
    books = books[-which(books$dedup), ] # dedup and return
    return(books)
}

# 2% of the collection gets newly checked out -> check flag
check_outs = function(books){
    n_to_chkout = round(nrow(books) * .02) # number of rows to randomly checkout
    book_ids = books[sample(which(!books$chkdout), n_to_chkout), ]$book_id
    books$chkdout[books$book_id %in% chkdout_ids] = 1
    return(books)
}


# 2% of the collection gets checked in (not the same as the check outs)
check_ins = function(books){
    n_to_pull = round(length(books$chkdout[books$chkdout == 1])* .02) # 2% of previously checked out books (this might need to change) 
    book_ids = books[sample(which(books$chkdout==1), n_to_pull), ]
    books$chkdout[books$book_id %in% chkdout_ids] = 0
    return(books)
}