#install.packages("janitor")
library(janitor)
library(readr)
library(ggplot2)
#load file

hf <- read.csv("hf-survey.csv")

#view column names and index
data.frame(
  index = seq_along(names(hf)),
  name  = names(hf)
)
#remove columns for full-time employment, malaysia-born and kitchen equipment
hf <- hf[ , -c(2, 3, 36)]

#columns that have scale questions
#extract numbers only
cols_to_clean <- c(7, 11:28)
names(hf)[cols_to_clean]

#extraction function
extract_number <- function(x) {
  x <- as.character(x)
  as.numeric(sub("^\\s*(\\d+).*", "\\1", x))
}
#apply to selected columns only
hf[cols_to_clean] <- lapply(hf[cols_to_clean], extract_number)

#check result
str(hf[cols_to_clean])

names(hf)
#clean and standardise each column name
hf <- janitor::clean_names(hf)
names(hf)
#convert each column name to short and brief names
hf <- hf |> dplyr::rename(
  # Basic info
  id                  = id,
  meals_breakfast     = breakfast,
  meals_lunch         = lunch,
  meals_dinner        = dinner,
  cook_people         = how_may_people_do_you_usually_cook_for,
  cook_for_children   = do_you_usually_cook_for_your_child_children,
  rush_scale          = scale_of_rush,
  diet_type           = how_would_you_describe_your_diet_type,
  
  used_mealkit_12m    = have_you_used_a_meal_kit_service_e_g_hello_fresh_gousto_mindful_chef_in_the_past_12_months,
  used_hellofresh     = have_you_ever_used_the_hello_fresh_meal_kit_service,
  
  # Delivery
  delivery_flex       = flexibility_to_choose_delivery_day_time,
  delivery_on_time    = deliveries_arrive_on_time_without_issues,
  
  # Ease of preparation
  ease_simple         = simple_and_easy_to_follow_recipes,
  ease_quick          = quick_prep_and_cooking_time,
  ease_cleanup        = having_minimal_cleanup_after_cooking,
  
  # Authenticity
  auth_flavour        = authentic_malaysian_or_southeast_asian_flavours_and_ingredients_matter_to_me,
  auth_malaysian      = meals_that_are_part_of_malaysian_cuisine,
  
  # Freshness
  quality_fresh       = fresh_and_high_quality_produce,
  
  # Dietary compatibility
  diet_halal          = halal_friendly_or_halal_certified_ingredients_matter_to_my_household,
  diet_labeling       = clear_dietary_and_ingredient_labeling_is_important_to_me_e_g_meat_allergens,
  
  # Nutrition
  nutrition_score     = nutritionally_balanced_healthy_meals,
  
  # Packaging
  pack_recyclable     = recyclable_or_eco_friendly_packaging,
  pack_sort           = easy_to_sort_and_manage_packaging,
  pack_minimal        = minimal_reduced_packaging,
  
  # Pricing
  price_per_meal      = price_per_meal_value_for_money,
  price_weekly        = consistent_weekly_subscription_price,
  price_discount      = promotional_discounts,
  price_delivery_fee  = delivery_fees,
  
  # Demographics
  age_group           = what_s_your_age_group,
  malaysian_background= which_best_describes_your_malaysian_background,
  years_uk            = how_long_have_you_lived_in_the_uk,
  religion            = what_is_your_religion_or_faith_background,
  halal_household     = does_your_household_follow_strictly_halal_food_practises
)


#check key factor grouping
#compute Cronbach’s alpha for each factor in R.


install.packages("psych")
library(psych)

delivery_items <- hf[, c(
  "delivery_flex",
  "delivery_on_time"
)]
#psych::alpha(delivery_items)

ease_items <- hf[, c(
  "ease_simple",
  "ease_quick",
  "ease_cleanup"
)]
#psych::alpha(ease_items)

auth_items <- hf[, c(
  "auth_flavour",
  "auth_malaysian"
)]
#psych::alpha(auth_items)

diet_items <- hf[, c(
  "diet_halal",
  "diet_labeling"
)]
#psych::alpha(diet_items)

pack_items <- hf[, c(
  "pack_recyclable",
  "pack_sort",
  "pack_minimal"
)]
#psych::alpha(pack_items)

price_items <- hf[, c(
  "price_per_meal",
  "price_weekly",
  "price_discount",
  "price_delivery_fee"
)]
#psych::alpha(price_items)

psych::alpha(delivery_items)$total$raw_alpha
psych::alpha(ease_items)$total$raw_alpha
psych::alpha(auth_items)$total$raw_alpha
psych::alpha(diet_items)$total$raw_alpha
psych::alpha(pack_items)$total$raw_alpha
psych::alpha(price_items)$total$raw_alpha

#find the mean for ranking
mean(unlist(hf[, c("ease_simple", "ease_quick", "ease_cleanup")]), na.rm = TRUE) #grouped ease of preparation
mean(unlist(hf[, c("auth_flavour", "auth_malaysian")]), na.rm = TRUE)  #grouped authenticity
mean(unlist(hf[, c("pack_recyclable", "pack_sort", "pack_minimal")]), na.rm = TRUE) #grouped packaging
mean(hf$delivery_flex)
mean(hf$delivery_on_time)
mean(hf$diet_halal)
mean(hf$diet_labeling)
mean(hf$price_per_meal)
mean(hf$price_delivery_fee)
mean(hf$price_discount)
mean(hf$price_weekly)

#main factors (group mean)
main_groups <- list(
  ease_items = c("ease_simple", "ease_quick", "ease_cleanup"),
  auth_items = c("auth_flavour", "auth_malaysian"),
  pack_items = c("pack_recyclable", "pack_sort", "pack_minimal")
)

#compute group means for main factors
for(group_name in names(main_groups)) {
  hf[[group_name]] <- rowMeans(hf[, main_groups[[group_name]]], na.rm = TRUE)
}

#all factors 
all_factors <- c("ease_items", "auth_items", "pack_items",
                 "diet_halal", "diet_labeling",
                 "price_per_meal","price_weekly","price_discount","price_delivery_fee",
                 "delivery_flex","delivery_on_time",
                 "nutrition_score", "quality_fresh")

#initialise table
report_table <- data.frame(
  factor = all_factors,
  spearman_rho = NA,
  mean_non_users = NA,
  mean_users = NA
)

#binary adoption variable
hf$used_mealkit_12m_bin <- ifelse(
  hf$used_mealkit_12m == "No, I have never used one", 0,
  ifelse(hf$used_mealkit_12m %in% c("Yes, I used it before but not now", 
                                    "Yes, I am currently subscribed"), 1, NA)
)

#compute correlations and means
for(i in seq_along(all_factors)) {
  factor_name <- all_factors[i]
  
  report_table$spearman_rho[i] <- cor(
    hf[[factor_name]], 
    hf$used_mealkit_12m_bin, 
    method = "spearman", 
    use = "pairwise.complete.obs"
  )
  
  report_table$mean_non_users[i] <- mean(
    hf[[factor_name]][hf$used_mealkit_12m_bin == 0],
    na.rm = TRUE
  )
  
  report_table$mean_users[i] <- mean(
    hf[[factor_name]][hf$used_mealkit_12m_bin == 1],
    na.rm = TRUE
  )
}

#view final table
print(report_table)

#factor names
factors <- c(
  "ease_items", "auth_items", "pack_items",
  "diet_halal", "diet_labeling",
  "price_per_meal", "price_weekly", "price_discount", "price_delivery_fee",
  "delivery_flex", "delivery_on_time",
  "nutrition_score", "quality_fresh"
)

#Spearman rho values
rho_values <- c(
  -0.33071532, -0.14044835, -0.19055805,
  -0.30165803, -0.36545561,
  0.03030303, -0.22149955, -0.12767034, -0.48096487,
  0.28066558, -0.14297734, -0.06984503, -0.04351941 
)

corr_table <- data.frame(
  factor = factors,
  spearman_rho = rho_values
)


ggplot(corr_table, aes(x = spearman_rho, 
                       y = reorder(factor, spearman_rho))) +
  geom_point(size = 4, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Spearman Correlation with Meal-Kit Adoption",
    x = "Correlation (rho)",
    y = "Factor"
  ) +
  theme_minimal(base_size = 10)





