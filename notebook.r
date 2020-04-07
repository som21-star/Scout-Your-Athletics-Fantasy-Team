
# Load the tidyverse package
# .... YOUR CODE FOR TASK 1 ....

# Import the full dataset
data <- ....

# Select the results of interest: women's javelin
javelin <- ....
 
# Give yourself a snapshot of your data 
# .... YOUR CODE FOR TASK 1 ....

# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

correct_data <- read_csv("datasets/athletics.csv")
    correct_javelin <- correct_data %>%
        filter(Male_Female == "Female" & Event == "Javelin") %>%
        select(-Male_Female, -Event)

run_tests({
    test_that("data is correct", {
        expect_equivalent(correct_data, data, 
            info = "datasets/athletics.csv should be read in using read_csv")
    })
    test_that("javelin is correct", {
        expect_equivalent(correct_javelin, javelin, 
            info = "javelin should be female javelin results only, and don't forget to remove the Male_Female and Event columns.")
    })
})

# Assign the tidy data to javelin_long
javelin_long <- ....

# Make Flight a numeric
# .... YOUR CODE FOR TASK 2 .... 

# Examine the first 6 rows
# .... YOUR CODE FOR TASK 2 ....

correct_javelin_long <- javelin %>%
     gather(Flight1:Flight6, key = "Flight", value="Distance")

correct_javelin_long$Flight = as.numeric(gsub("Flight", "", correct_javelin_long$Flight))

correct_javelin_wide_full <- javelin %>%
    mutate(early = Flight1+Flight2+Flight3, late = Flight4+Flight5+Flight6, diff = late - early)

run_tests({
    test_that("javelin_long is correct", {
        expect_equivalent(correct_javelin_long, javelin_long, 
            info = "Check your gather statement - you want the six `Flight` columns with `Flight` and `Distance` as 
your key:value pair")
    })
    test_that("Flight values are numeric", {
        expect_that(javelin_long$Flight, is_a("numeric"),
        info = "Use `gsub` to remove the word `Flight from each entry in the new `Flight` column, then
convert the values to numeric.")
    })
})

javelin_totals <- javelin_long %>%
# .... YOUR CODE FOR TASK 3 .... 
# .... YOUR CODE FOR TASK 3 .... 
# .... YOUR CODE FOR TASK 3 .... 

# View 10 rows of javelin_totals
# .... YOUR CODE FOR TASK 3 .... 

correct_javelin_totals <- javelin_long %>%
     filter(Distance > 0) %>%
     group_by(Athlete, EventID) %>% 
     summarize(TotalDistance = sum(Distance), StandardDev = round(sd(Distance),3), Success = n())

run_tests({
    test_that("javelin_long contains correct athletes and results", {
        expect_equivalent(javelin_totals$Athlete, correct_javelin_totals$Athlete, 
        info = "Did you filter by `Distance` so you only have results greater than zero?")
    })
    
    test_that("summarize values are correct", {
        expect_true(all(c("TotalDistance", "StandardDev", "Success") %in% names(javelin_totals)) &
                    all(javelin_totals$StandardDev %in% correct_javelin_totals$StandardDev),
            info = "Did you correctly define `TotalDistance`, `StandardDev` and `Success` in a `summarize` statement?
    Be sure to wrap your `sd()` function in `round()` and round to 3 places for `StandardDev`.")
    })
})

javelin <- ....
# .... YOUR CODE FOR TASK 4 ....

# Examine the last ten rows
# .... YOUR CODE FOR TASK 4 ....

last_value <- .Last.value 

run_tests({
    test_that("diff column is properly created and defined", {
    expect_equal(javelin[-5], correct_javelin_wide_full[-5],
        info = "Make sure `early` is the sum of the first three flights, `late` is the sum of the latter three flights, and 
   `diff` is `early` subtracted from `late`.")
    })
})

javelin_totals <- ....
# .... YOUR CODE FOR TASK 5 ....
# .... YOUR CODE FOR TASK 5 ....

# Examine the first ten rows
# .... YOUR CODE FOR TASK 5 ....

correct_javelin_totals <- correct_javelin_totals %>%
    left_join(javelin, by=c("EventID", "Athlete")) %>%
    select(1, 3:5, 14)

run_tests({
    
    test_that("Correct columns are in the new data frame",{
        expect_true(all(c("Athlete", "TotalDistance", "StandardDev", "Success", "diff") %in% names(javelin_totals)),
                   info = "Double check the columns you selected after performing the left join.")
    })
    
    test_that("Athletes and events are grouped correctly", {
    expect_equivalent(javelin_totals, correct_javelin_totals, 
        info = "Remember to start with `javelin_totals`, then `left_join` by `EventID` and `Athlete`")
    })
})

.... <- .... {
    (result - min(result)) / (max(result) - min(result))
}
aggstats <- c("TotalDistance", "StandardDev", "Success", "diff")
.... <- javelin_totals %>%
 ungroup() %>%
# .... YOUR CODE FOR TASK 6 ....
# .... YOUR CODE FOR TASK 6 ....
# .... YOUR CODE FOR TASK 6 ....

head(javelin_norm)

correct_javelin_norm <- correct_javelin_totals %>%
 ungroup() %>%
 mutate_at(aggstats, norm) %>%
 group_by(Athlete) %>%
 summarize_all(mean)

run_tests({
    test_that("norm is properly defined as a function", {
        expect_that(norm, is_a("function"), info="Take a look at how you defined the function `norm.`")
    })
    test_that("data is correctly normalized", {
    expect_equivalent(correct_javelin_norm, javelin_norm, 
        info = "Use `mutate_at` to compute the function `norm` over the `aggstats` vector. Then check to make sure
you grouped by `Athlete` and took the mean across all columns.")
    })
})

weights <- c(...., ...., ...., ....)
javelin_team <- javelin_norm %>%
# .... YOUR CODE FOR TASK 7
# .... YOUR CODE FOR TASK 7
# .... YOUR CODE FOR TASK 7
# .... YOUR CODE FOR TASK 7

javelin_team

run_tests({
    test_that("weights sum to 10", {
    expect_equal(sum(weights), 10, 
        info = "Uh oh! Do your weights add up to 10?")
    })

    test_that("players are arranged in descending order",{
        expect_true(javelin_team$TotalScore[1] > javelin_team$TotalScore[5], 
         info = "Make sure you `arrange` the data frame in descending order to get the 5 best players!")
    })
    
    test_that("student selects top 5 athletes", {
    expect_equal(nrow(javelin_team), 5, 
        info = "Check your slice call to take only the top 5 players.")
    })
})

team_stats <- javelin_totals %>% 
# .... YOUR CODE FOR TASK 8 ....
# .... YOUR CODE FOR TASK 8 ....

pool_stats <- data.frame(do.call('cbind', sapply(javelin_totals, function(x) if(is.numeric(x)) c(max(x), mean(x)))))
pool_stats$MaxAve <- c("Maximum", "Average")
pool_stats <- pool_stats %>%
    gather(key="Statistic", value="Aggregate", -MaxAve)
                                                 
# Examine team stats
# .... YOUR CODE FOR TASK 8 ....

correct_team_stats <- correct_javelin_totals %>% 
 filter(Athlete %in% javelin_team$Athlete) %>% 
 summarize_all(mean) 

run_tests({
    test_that("correct athletes are selected", {
    expect_equivalent(correct_team_stats, team_stats, 
        info = "Did you filter the `javelin_totals` data frame for those athletes who are `%in%` your team?")
    })
})

p <- team_stats %>%
# .... YOUR CODE FOR TASK 9 ....
# .... YOUR CODE FOR TASK 9 ....
# .... YOUR CODE FOR TASK 9 ....
# .... YOUR CODE FOR TASK 9 .... +
  geom_hline(data=pool_stats, aes(yintercept=Aggregate, group=Statistic, color=MaxAve), size=1) +
#   labs(title=".... Your Team Name....: Women's Javelin", color="Athlete pool maximum / average") +
  scale_fill_hue(l=70) +
  scale_color_hue(l=20) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
  
p

last <- last_plot()
facet_class <- class(last_plot()$facet)

run_tests({
     test_that("The plot is faceted into 2 x 2 grid", {
    expect_true(! "FacetNull" %in% facet_class, 
        info = "Use `facet_wrap` to create a 2 x 2 grid based on `Statistic.`")
    })
    
    test_that("correct columns are plotted", {
    mappings <- str_replace(as.character(last$mapping), "~", "")
    expect_true(all(c("Athlete", "Aggregate") %in% mappings), 
        info = "Check your key:value pair in the gather statement and then your aesthetics in the 
        ggplot statement. You should have `Athlete` for two aesthetics and `Aggregate` - your 'value' for the other.")
    })
    
    test_that("plot is a bar graph", {
    expect_true( "GeomBar" %in% class(last$layers[[1]]$geom), 
        info = "Did you include a `geom_bar` to create a bar plot?")
    })

# Reason for deleting: it seems that the task won't be validated unless nrow and ncol are specified whish isn't necessary in practice
#     test_that("Plots are displayed in two rows", {
#     expect_true(p$facet$params$ncol == 2 & p$facet$params$nrow == 2, 
#         info = "Use `nrow` and `ncol` in `facet_wrap` to create a 2x2 display of your plots.")
#     })
})

home <- c(....,....,....)
away <- sample(1:nrow(javelin_totals), 3, replace=FALSE)

HomeTeam <- round(sum(team_stats$TotalDistance[home]),2)
AwayTeam <- round(sum(javelin_totals$TotalDistance[away]),2)

print(paste0("Javelin match, Final Score: ", HomeTeam, " - ", AwayTeam))
ifelse(HomeTeam > AwayTeam, print("Win!"), print("Sometimes you just have to take the L."))

run_tests({
     test_that("digits within range are chosen for home", {
         expect_equal(sum(home > 5), 0, info="Make sure you place three digits between 1 and 5 in `home`.")
     })
})
