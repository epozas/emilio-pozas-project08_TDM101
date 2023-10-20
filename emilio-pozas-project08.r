options(jupyter.rich_display = F)
options(repr.matrix.max.cols=30, repr.matrix.mas.rows = 200)
library(data.table)
products <- fread("/anvil/projects/tdm/data/icecream/combined/products.csv")
reviews <- fread("/anvil/projects/tdm/data/icecream/combined/reviews.csv")
titles <- fread("/anvil/projects/tdm/data/movies_and_tv/titles.csv")
episodes <- fread("/anvil/projects/tdm/data/movies_and_tv/episodes.csv")
ratings <- fread("/anvil/projects/tdm/data/movies_and_tv/ratings.csv")

head(products$ingredients)
unique(trimws(unlist(strsplit(products$ingredients[1], ",|\\(|\\)"))))
unique(trimws(unlist(strsplit(products$ingredients[2], ",|\\(|\\)"))))
getingredients <- function(x) {
    unique(trimws(unlist(strsplit(x, ",|\\(|\\)"))))
    }
getingredients(products$ingredients[3])
counts <- tail(sort(table(trimws(unlist(sapply(products$ingredients,getingredients), use.names=FALSE)))),11)
tail(sort(table(trimws(unlist(sapply(products$ingredients,getingredients), use.names=FALSE)))),11)
barplot(counts, main ="barplot for top ingredients", xlab="Ingredients")
plot(counts, type="l")

products_reviews_by_rating <- function(products_df, reviews_df, myrating) #names the function as well as the inputs
    { 
    merge_results <- merge(products_df, reviews_df, by="key") #merges the inputs into a key
    products_reviews_results <- merge_results[merge_results$rating >= myrating, ] #
    return(products_reviews_results)
    }

# Define a function that filters products and reviews based on a rating threshold
products_reviews_by_rating <- function(products_df, reviews_df, myrating) 
{ 
    # Merge the product and review data frames based on a common column "key"
    merge_results <- merge(products_df, reviews_df, by="key")
    
    # Filter the merged data to retain rows with a rating greater than or equal to the specified "myrating"
    products_reviews_results <- merge_results[merge_results$rating >= myrating, ] 
    
    # Return the filtered results
    return(products_reviews_results)
}

my_selection <- products_reviews_by_rating(products, reviews, 4.5)
nrow(my_selection)

getcount <- function(products_df, ingredient) {
    all_ingredients <- table(trimws(unlist(sapply(products_df$ingredients,getingredients), use.names=FALSE)))
    count <- all_ingredients[ingredient]
    return(count)
    }

getcount(products, "SALT")