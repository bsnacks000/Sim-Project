source("setup.R")
require(triangle)

setwd("~/Documents/CUNY/Simulation_604/Final_proj")

# standard setup params: 1000, 50, 36, 0.1, 0.05         boost free space
set.seed(1234)
data = setup(1000,50,36,0.1,0.05) # returns list, need to set to two dfs
books = data$books
shelves = data$shelves

# define some methods for each step of the sim...


# Utility functions

# helper function that duplicates textbooks and assigns ids
duplicate_textbooks = function(last_indx, texts){     
    
    texts$book_id = seq(last_indx+1, last_indx+nrow(texts))  # id and set to master
    texts$copym = 1
    
    for(i in 1:nrow(texts)){
        n_dups = runif(1,1,3)
        dups = data.frame(lapply(texts, function(j){ 
            rep(j[i],n_dups) 
        }))
        dups$copym = 0                  # unset master, set btype, dupeof id
        dups$btype = 't'
        dups$dupeof = dups$book_id
        texts = rbind(texts,dups)             # bind to purchase order
    }
    texts$book_id = seq(last_indx+1, last_indx+nrow(texts))      # reset book ids
    return(texts)
}



book_widths = function(n,a,b){        # utility returns book widths between a and b for n books
    rtriangle(n, a, b, (a + b)/b )
}


# Simulation processes API

# also updates the shelves dataframe 
# 95% threshold for shelve full... then flag the shelf might need to add a column <<<<--
update_shelves = function(books, shelves, shelf_width){
    
    for (i in 1:nrow(shelves)){ 
        new_width = sum(books[which(books$shelf_id==i & !books$chkdout),]$width) # calculate new width
        shelves[shelves$shelf_id==i,]$in_use = new_width        # set new widths for each shelf
    }
    shelves$perc_used = shelves$in_use/shelf_width  # reset perc_used
    
    return(shelves)
}

# set of purchases - 5 to 25 books (simulated book widths)
# set of textbook purchases (simulated book widths) - 2 copies of 3 to 5 books
# set flag to 'n', 't' for text-book or non-textbook
# return a new dataframe to be appended to books in outer scope
# this is a blank order... all field's build/add logic is done in the add()/update_shelves() functions
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

# special logic for appending a textbook df to main library df
# textbooks need to be shelved together
add_textbooks = function(books,new_books){
    
    last_indx = tail(books$book_id,1)
    new_books = duplicate_textbooks(last_indx,new_books)
    # assign shelf_ids... for now uniform random but should probably change to more meaningful distribution.
    # need to add all books to the same shelf <<<<--
    new_books$shelf_id = round(runif(nrow(new_books),1,51))  # hard coded shelf numbers here... need to change
    books = rbind(books,new_books)
    return(books)
}

#speical logic for appending a non-textbook df to main library df
add_non_textbooks = function(books,new_books){
    
    last_indx = tail(books$book_id,1)
    new_books$book_id = seq(last_indx+1, last_indx+nrow(texts))
    # assign shelf_ids... for now uniform random but should probably change to more meaningful distribution.
    new_books$shelf_id = round(runif(nrow(new_books),1,51))  # hard coded shelf numbers here... need to change
    books = rbind(books,new_books)
    return(books)
}


# round of weeding - 2% of the collection -> DO NOT weed books that have been checked out
weeding = function(books){
    n_to_pull = round(nrow(books) * .02) # number of rows to randomly pull
    books = books[-sample(which(!books$chkdout), n_to_pull), ] # pull em, update and return
    return(books)
}

# <<<<-- Reassess deduping methods
# set_dup()
# if chkdout == 1 then set dedupe = 1


# round of de-duplicating (textbooks) - removes 
de_dup = function(books){
    
    # if not checked
    books = books[-which(books$dedup==1), ] # dedup and return
    return(books)
}

# 2% of the collection gets newly checked out -> check flag
# need to set deduping flag for textbooks that get checked out <<<<--
check_outs = function(books){
    n_to_chkout = round(nrow(books) * .02) # number of rows to randomly checkout
    book_ids = books[sample(which(!books$chkdout), n_to_chkout), ]$book_id
    books$chkdout[books$book_id %in% chkdout_ids] = 1
    return(books)
}


# 2% of the collection gets checked in (not the same as the check outs)
check_ins = function(books){
    n_to_pull = round(length(books$chkdout[books$chkdout == 1])* .2) # 20% of previously checked out books (this might need to change) 
    
    # if dedup ==1 remove -- add this logic here
    book_ids = books[sample(which(books$chkdout==1), n_to_pull), ]
    books$chkdout[books$book_id %in% chkdout_ids] = 0
    return(books)
}