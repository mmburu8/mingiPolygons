# library
library(shiny)
library(reactable)
library(tidyverse)
# short function to create %!in% operator
'%!in%' <- function(x, y)!('%in%' (x, y))


# preprocessing function
# function to evaluate datatypes of columns in dataframe and columns in dataframe
preWork <- function(data_main, col_spec){
  # determine if dataframe has sfc datatype
  select_cols <- data_main %>%
    select_if(~inherits(., "sfc"))
  sfc_cols <- colnames(select_cols)
  # if sfc cols are present perform data processing
  if (length(sfc_cols) >= 1){
    # define polygon object being obtained from King's Landing
    arya_df <- data_main[, c(col_spec)]
    arya_df$L1 <- row.names(arya_df)
    arya_df$L1 <- as.numeric(arya_df$L1)
    # convert geometry column in sf dataframe to x and y column dataframe
    xyz <-  data.frame(st_coordinates(st_cast(data_main[,"geometry"], "MULTIPOINT")))
    # x, y and polygon name
    right_df <- left_join(xyz, arya_df)
    right_df <- right_df[, c("X", "Y", col_spec)]
    # change column names
    colnames(right_df) <- c("x", "y", col_spec)
  } else {
    if (!all(c("x", "y") %in% colnames(data_main)) & !all(c("X", "Y") %in% colnames(data_main))){
      stop("The x and y columns do not exist in dataframe")
    } else {
      # change column names
      names(data_main)[names(data_main) == "X"] <- "x"
      names(data_main)[names(data_main) == "Y"] <- "y"
      right_df <- data_main[, c("x", "y", col_spec)]
    }
  }
  return(right_df)
}

# accurate find state or county through click
# evaluate states by a piece in boundary
piece_boundary <- function(main_data, click_data, vector_click, col_spec){
  unique_states <- unique(click_data[, col_spec])
  # choose data from both states
  virgin <- main_data[main_data[, col_spec] %in% unique_states,]
  # add input$click x and y into click_data dataframe
  click_data[nrow(click_data) + 1,] <- c('unknown', vector_click$x, vector_click$y)
  # change datatype
  click_data$x <- as.numeric(click_data$x)
  click_data$y <- as.numeric(click_data$y)
  # unique values in major variable
  copy_unique <- unique(virgin[virgin[,col_spec] == "major", "copying"])
  # find points near the border
  # separate polygon objects
  obj <- TRUE
  pygn <- 1
  while (obj){
    vrgn_a <- virgin[virgin[,col_spec]!="major",]
    vrgn_b <- virgin[virgin[,"copying"]==copy_unique[pygn],]
    # loop through first polygon objects
    pb_x <- c()
    pb_y <- c()
    for (i in 1:nrow(vrgn_a)){
      # loop through the other polygonal object
      for (j in 1:nrow(vrgn_b)){
        if (vrgn_a[i, 'x'] == vrgn_b[j, 'x'] & vrgn_a[i, 'y'] == vrgn_b[j, 'y']){
          pb_x <- c(pb_x, vrgn_a[i, 'x'])
          pb_y <- c(pb_y, vrgn_a[i, 'y'])
        }
      }
    }
    lin <- data.frame(x=pb_x, y=pb_y)
    if (nrow(lin) == 0){
      pygn <- pygn + 1
    } else {obj <- FALSE}
  }
  # DETERMINE THE PART OF THE BOUNDARY TO BE EVALUATED
  x_sd <- 0
  ted <- 0
  while (x_sd == 0){
    # determine the x and y range
    xrange <-  10 ** (nchar(as.integer(max(click_data$x) - min(click_data$x))) - 1 + ted)
    yrange <- 10 ** (nchar(as.integer(max(click_data$y) - min(click_data$y))) - 1)
    # piece of boundary to be evaluated with
    virgin_island <- filter(lin, x > (min(click_data$x) - xrange) &
                              x < (max(click_data$x) + xrange) & y > (min(click_data$y) - yrange) &
                              y < (max(click_data$y) + yrange))
    # identify which state is TRUE or FALSE
    # filter data
    x_sd <- sd(virgin_island$x)
    ted <- ted + 1
    if (is.na(x_sd)){x_sd <- 0}
  }
  robin <- 1
  ding <- TRUE
  while (ding){
    # determine the x and y range
    xrange <-  xrange * robin
    disc_m <- virgin[virgin[,col_spec] != 'major' & virgin[,'x'] > (min(click_data$x) - xrange) & virgin[,'x'] < (max(click_data$x) + xrange),]
    # determine if disc_m and virgin_island are equal
    dc_m <- disc_m %>%
      arrange(x) %>%
      select(x, y)
    vg_m <- virgin_island %>%
      arrange(x) %>%
      select(x, y)
    equality <- anti_join(dc_m, vg_m)
    if (nrow(equality) == 0){
      robin <- robin + 1
    } else{
      ding <- FALSE
    }
  }
  # determine if data is above or below the curve
  # get linear interpolation equation
  Frit <- approxfun(x=virgin_island$x, y=virgin_island$y)
  # iterate over x to calculate y by reducing through standard deviation
  clickX <- vector_click$x
  div <- NA
  while (is.na(div)){
    # estimate of y
    div <- Frit(clickX)
    if (is.na(div) & clickX > mean(virgin_island$x)){
      clickX <- clickX - sd(virgin_island$x)
    } else if(is.na(div) & clickX < mean(virgin_island$x)){
      clickX <- clickX + sd(virgin_island$x)
    }
  }
  # point out if y_prediction is greater than y_actual
  barney <- div > vector_click$y
  # get the row closest to the mean
  close_m <- disc_m[which.max(abs(mean(virgin_island$y) - disc_m$y)), c(col_spec, 'x', 'y')]
  # evaluate by close_3
  closeX <- close_m[1, 'x']
  closeY <- close_m[1, 'y']
  # evaluate states
  stateYes <- close_m[1, col_spec]
  stateNo <- "major"
  span <- NA
  while (is.na(span)){
    span <- Frit(closeX)
    if (is.na(span) & closeX > mean(virgin_island$x)){
      closeX <- closeX - sd(virgin_island$x)
    } else if(is.na(span) & closeX < mean(virgin_island$x)){
      closeX <- closeX + sd(virgin_island$x)
    }
  }
  stinson <- span > closeY
  if (stinson == barney){
    statement <- stateYes
  }
  else{statement <- stateNo
  }
  return(statement)
}

# function (possible library)
polygonIdentify <- function(data_main, data_click, click_vector, col_spec){
  #get states captured by click
  statesClick <- unique(data_click[, col_spec])
  # rows from data_main
  df_a <- data_main[data_main[, col_spec] == as.character(statesClick[1]),]
  df_b <- data_main[data_main[, col_spec] == as.character(statesClick[2]),]
  # evaluate the correct states
  # find closest point to nearPoints
  dist_y <- c(abs(max(df_b$y) - mean(data_click$y)), abs(min(df_b$y) - mean(data_click$y)),
              abs(max(df_a$y) - mean(data_click$y)), abs(min(df_a$y) - mean(data_click$y)))
  dist_x <- c(abs(max(df_b$x) - mean(data_click$x)), abs(min(df_b$x) - mean(data_click$x)),
              abs(max(df_a$x) - mean(data_click$x)), abs(min(df_a$x) - mean(data_click$x)))
  # find closest points by clickVector
  click_y <- c(abs(max(df_b$y) - click_vector$y), abs(min(df_b$y) - click_vector$y),
               abs(max(df_a$y) - click_vector$y), abs(min(df_a$y) - click_vector$y))

  click_x <- c(abs(max(df_b$x) - click_vector$x), abs(min(df_b$x) - click_vector$x),
               abs(max(df_a$x) - click_vector$x), abs(min(df_a$x) - click_vector$x))

  # Evaluate by longitude
  if (min(click_y) < min(click_x)){
    # Evaluate by longitude
    if (max(df_a$y) < max(df_b$y)){
      if (click_vector$y < max(df_a$y) & click_vector$y < min(df_b$y)){
        state <-  df_a[1, col_spec]
      } else if (click_vector$y > max(df_a$y) & click_vector$y > min(df_b$y)){
        state <-  df_b[1, col_spec]
      } else if (click_vector$y < max(df_a$y) & click_vector$y > min(df_b$y)){
        state <- piece_boundary(data_main, data_click, click_vector, col_spec)

      }
    } else if (max(df_a$y) > max(df_b$y)){
      if (click_vector$y < max(df_b$y) & click_vector$y < min(df_a$y)){
        state <-  df_b[1, col_spec]

      } else if (click_vector$y > max(df_b$y) & click_vector$y > min(df_a$y)){
        state <-  df_a[1, col_spec]

      } else if (click_vector$y < max(df_b$y) & click_vector$y > min(df_a$y)){
        state <- piece_boundary(data_main, data_click, click_vector, col_spec)

      }
    }
  } else if (min(click_x) < min(click_y)){
    # Evaluate by latitude
    if (max(df_a$x) < max(df_b$x)){
      if (click_vector$x < max(df_a$x) & click_vector$x < min(df_b$x)){
        state <-  df_a[1, col_spec]
      } else if (click_vector$x > max(df_a$x) & click_vector$x > min(df_b$x)){
        state <-  df_b[1, col_spec]
      } else if (click_vector$x < max(df_a$x) & click_vector$x > min(df_b$x)){
        state <- piece_boundary(data_main, data_click, click_vector, col_spec)
      }
    } else if (max(df_a$x) > max(df_b$x)){
      if (click_vector$x < max(df_b$x) & click_vector$x < min(df_a$x)){
        state <-  df_b[1, col_spec]
      } else if (click_vector$x > max(df_b$x) & click_vector$x > min(df_a$x)){
        state <-  df_a[1, col_spec]
      } else if (click_vector$x < max(df_b$x) & click_vector$x > min(df_a$x)){
        state <- piece_boundary(data_main, data_click, click_vector, col_spec)
      }
    }
  }
  return(state)
}

# FUNCTION THAT CLASSIFIES MORE THAN 2 POLYGONS
mingiPolygons <- function(data_main, click_vector, col_spec, thresh=50, max_pts=4){
  # get nearPoints dataframe
  data_click <- nearPoints(data_main, click_vector, threshold=thresh, maxpoints = max_pts, xvar="x", yvar="y")
  # make column arrangement
  data_click <- data_click[,c(col_spec, "x", "y")]
  # unique values in list
  unique_states <- unique(data_click[,col_spec])
  if (length(unique_states) >= 2){
    for (i in 1:length(unique_states)){
      # copy unique_states vector
      unique_copy <- unique_states
      # remove specific value from vector
      unique_copy <- unique_copy[!unique_copy == unique_states[[i]]]
      # create copies for data_main and data_click
      dm_copy <- data.frame(data_main)
      dc_copy <- data.frame(data_click)
      # copy values in col_spec before change
      dm_copy$copying <- dm_copy[,col_spec]
      #dc_copy$copying <- dc_copy[,"county"]
      # manipulate values in data
      dm_copy[dm_copy[,col_spec] %in% unique_copy, col_spec] <- "major"
      dc_copy[dc_copy[,col_spec] %in% unique_copy, col_spec] <- "major"
      dreamville <- piece_boundary(dm_copy, dc_copy, click_vector, col_spec)

      if (dreamville != "major"){
        break
      }
    }
  }else if (length(unique_states) == 1){
    df_kul <- data_main[data_main[,col_spec] == as.character(unique_states[1]),]
    dreamville <- df_kul[1, col_spec]}
  return (dreamville)
}
