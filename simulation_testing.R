#simulation 
setwd("~/Documents/CUNY/Simulation_604/Final_proj")

source("setup.R")
source("methods.R")

# standard setup params: 1000, 50, 36, 0.1, 0.05         boost free space
set.seed(1234)
data = setup(1000,50,36,0.1,0.05) # returns list, need to set to two dfs

#init books and shelves
books = data$books
shelves = data$shelves


# Sim Method order as outlined in v3.Rmd
# Weeding, Purchase (t and n) and shelve, check in, check out, dedupe

books2 = weeding(books)
new_texts = purchase_books('t',3,6)
new_ntexts = purchase_books('n',5,26)
books2 = add_non_textbooks(books2,new_ntexts,shelves)
books2 = add_textbooks(books2,new_texts,shelves)
books2 = check_ins(books2)
books2 = check_outs(books2)
books2 = de_dup(books2)
shelves2= update_shelves(books2,shelves,36)



