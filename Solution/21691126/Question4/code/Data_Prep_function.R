Data_Prep_Function <- function(Lst, Tournament_Inputs = c("G", "M", "F")){

    read_file_func <- function(Lst){
        # listchoose <- Lst[[10]]
        listchoose <- Lst

        shhcsv <- function(x, ...) {
            sjrd <- purrr::quietly(read_csv)
            df <- sjrd(x, ... )
            df$result
        }
        result <- suppressWarnings(shhcsv(listchoose))
        result <-
            result %>%
            mutate(across(.cols = -c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.numeric(.))) %>%
            mutate(across(.cols = c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.character(.)))
    }

    result <- Lst %>% map_df(~read_file_func(.))
}

