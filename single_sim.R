# Single sim wrapped in function
source("setup.R")   # these need to be in the same folder...
source("methods.R")

single_sim = function(nvols=1000,nshelves=50,shelf_width=36,sfree_space=0.1,textb_masters=0.05,reshelf_thresh=5,seed_val=NULL){
    
    set.seed(seed_val)   # specify seed
    data = setup(nvols,nshelves,shelf_width,sfree_space,textb_masters) # returns list, need to set to two dfs
    
    #init books and shelves
    books = data$books
    shelves = data$shelves
    
    
    books = data$books
    shelves = data$shelves
    
    counter = 0
    full_count = 0
    reshelf_thresh = 5
    
    while(T){
        ### SIM CODE
        books = weeding(books,0.02)
        new_texts = purchase_books('t',3,6)
        new_ntexts = purchase_books('n',5,26)
        books = add_non_textbooks(books,new_ntexts,shelves)
        books = add_textbooks(books,new_texts,shelves)
        books = check_ins(books,0.2)
        books = check_outs(books,0.02)
        books = de_dup(books)
        shelves= update_shelves(books,shelves,36)
        ####
        
        full_count = sum(shelves$full)       # get number full 
        if(full_count >= reshelf_thresh)     # break if >= threshold
            break
        counter = counter + 1
    }
    
    # The counter value is number of rounds  
    return(counter)
    
}
