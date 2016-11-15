# can import this function using source("setup.R")
# returns list of 2 dataframes: books and shelves

setup = function(Nvols, Nshelves, shelf_width, sfree_space){
    
    library(triangle)
    
    # set max shelf space consumed by books
    max_shelved <- shelf_width - (shelf_width * sfree_space)

    # Initialize 'books' data frame
    books <- data.frame(book_id = 1:Nvols, shelf_id = numeric(Nvols),
                        width = numeric(Nvols), chkdout = numeric(Nvols), stringsAsFactors = FALSE)
    
    # set book widths: distrib is between 1-2 inches; assume UNIFORM distribution
    # NOTE: distribution to be sampled from can be changed to anything we want
    a <- 1 # set lower bound of triangular distribution
    b <- 2 # set upper bound of triangular distribution
    
    # now sample from triangular distribution: Number of samples = number of books ('Nvols')
    books$width <- rtriangle(Nvols, a, b, (a + b)/b )
    
    # Initialize 'shelves' data frame
    shelves <- data.frame(shelf_id = 1:Nshelves, in_use = numeric(Nshelves), 
                          perc_used = numeric(Nshelves), stringsAsFactors = FALSE)
    
    k <- 1
    # sum book widths to ensure they don't exceed (max shelf width - free_space)
    for (i in 1: Nshelves) {
        # while space used on shelf < max space consumed by books and book index < Nvols
        while ((shelves$in_use[i] + books$width[k]) < max_shelved & (k <= Nvols) ) {
            # assign shelf to next book
            books$shelf_id[k] <- shelves$shelf_id[i]
            # add width of book to total used on current shelf
            shelves$in_use[i] <- shelves$in_use[i] + books$width[k]
            k <- k + 1
        } 
    } 
    
    shelves$perc_used = shelves$in_use/36  # sets perc_used
    
    # =====================================================
    # Initialization: set 10% of books to 'checked out' prior to running any simulations
    # =====================================================
    
    # select 10% of book_id values to set to 'checked out' status
    c_out <- sample(books$book_id, (Nvols * .10), replace = FALSE)
    
    # For each book_id in c_out, set chkdout = 1 and subtract
    # book width from shelf space used for appropriate shelf_id
    for (i in c_out) {
        # set chkdout flag to 1
        books$chkdout[i] <- 1
        # subtract width of chkdout book from space in use on its assigned shelf
        shelves$in_use[books$shelf_id[i]] <- shelves$in_use[books$shelf_id[i]] - books$width[i]
    }
    
    return(list(books=books,shelves=shelves)) # return books shelves and max_shelved
    
}

