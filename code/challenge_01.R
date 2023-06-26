library(tidyverse)
library(lubridate)

# Visualization
library(plotly)

bikes_tbl      <- readRDS("01_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("01_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("01_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  
  # Add the total price
  mutate(total_price = price_euro * quantity)

format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
  
}

total_sales_m_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

total_sales_m_tbl

#create quartarly sales fig
quarterly_sales_m_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "quarter")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

weekly_sales_m_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "week")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
  
g0 <- quarterly_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )
g0



g1 <- total_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g1

g2 <- weekly_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g2



quarterly_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "quarter")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g3 <- quarterly_sales_cat_tbl %>%
  filter(category_2 == "Triathlon Bike" | category_2 == "Race" | category_2 == "Endurance"| category_2 == "Cyclocross") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g3

monthly_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g4 <- monthly_sales_cat_tbl %>%
  filter(category_2 == "Triathlon Bike" | category_2 == "Race" | category_2 == "Endurance"| category_2 == "Cyclocross") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g4

weekly_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "week")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g5 <- weekly_sales_cat_tbl %>%
  filter(category_2 == "Triathlon Bike" | category_2 == "Race" | category_2 == "Endurance"| category_2 == "Cyclocross") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g5


quarterly_mountain_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2, category_1) %>%
  filter(category_1 == "Mountain") %>% 
  
  mutate(date_rounded = floor_date(order_date, unit = "quarter")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g5 <- quarterly_mountain_sales_cat_tbl %>%
  filter(category_2 == "Trail" | category_2 == "Cross-Country" | category_2 == "Enduro"| category_2 == "Downhill"| category_2 == "Fat Bikes"| category_2 == "Dirt Jump") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g5

monthly_mountain_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2, category_1) %>%
  filter(category_1 == "Mountain") %>% 
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g6 <- monthly_mountain_sales_cat_tbl %>%
  filter(category_2 == "Trail" | category_2 == "Cross-Country" | category_2 == "Enduro"| category_2 == "Downhill"| category_2 == "Fat Bikes"| category_2 == "Dirt Jump") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g6

weekly_mountain_sales_cat_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price, category_2, category_1) %>%
  filter(category_1 == "Mountain") %>% 
  
  mutate(date_rounded = floor_date(order_date, unit = "week")) %>%
  
  group_by(date_rounded, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

g7 <- weekly_mountain_sales_cat_tbl %>%
  filter(category_2 == "Trail" | category_2 == "Cross-Country" | category_2 == "Enduro"| category_2 == "Downhill"| category_2 == "Fat Bikes"| category_2 == "Dirt Jump") %>% 
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(vars(category_2), ncol = 1, scales = "free")+
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g7