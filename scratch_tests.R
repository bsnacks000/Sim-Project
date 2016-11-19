## scratch code
# test dedup
test = books[1:20,]
new_books = purchase_books('t')
test = add_textbooks(test,new_books,shelves)

test[c(27,28,30),"chkdout"] = 1
test


n_dedup = round(runif(1,1,3))
# pull master ids to dedupe
master_ids = test[sample(which(test$copym==1),3),]$book_id
# dedupe books 
master_ids

test[which(test$dupeof %in% master_ids & test$chkdout==1), ]$dedupe = 1
test

test[-which(test$dupeof %in% master_ids & !test$chkdout), ]
test
#test add text to same shelf
test = books[1:20,]


new_books = purchase_books('t')
add_textbooks(test,new_books,shelves)


new_books$shelf_id = sample(get_shelf_ids(shelves),nrow(new_books))


last_indx = tail(test$book_id,1)
new_books = duplicate_textbooks(last_indx,new_books)

# need to add all books to the same shelf <<<<--
# switch duplicates to master
new_books


new_books$shelf_id[which(new_books$book_id==new_books$dupeof)]
books = rbind(books,new_books)
return(books)





#tests for setting dedupe flag on textbook checkout
test = books[45:55,] ;rownames(test) = NULL;
test

book_chkout = test[c(6,7), ]$book_id 
test$chkdout[test$book_id %in% book_chkout] = 1

if(any(test[test$book_id %in% book_chkout,]$dupeof > 0))
    test[which(test$book_id %in% book_chkout),]$dedupe = 1
test
    
## tests for dedupe on checkin
test = books[sample(30),] ;rownames(test) = NULL
test[sample(which(test$chkdout==1),3),]$dedupe = 1
test

chkdout_ids = test[sample(which(test$chkdout==1), 4), ]
chkdout_ids
#if (chkdout_ids$dedupe == 1) 

chkdout_ids[chkdout_ids$dedupe == 1,]$book_id    

test = test[-which(test$book_id %in% chkdout_ids[chkdout_ids$dedupe == 1,]$book_id), ] 
test
nrow(test)

# backup logic for deduping --> does not belong in check out
# set dedupe flag if book that gets checked out is a duplicate
if(any(books[books$book_id %in% book_chkout,]$dupeof > 0))
    books[which(books$book_id %in% book_chkout),]$dedupe = 1



x = c(.1,.2,.3,.99,.99)
y = c(0,0,0,0,0)
z = data.frame(x,y)

z$y[which(z$x>0.95)] = 1
z

