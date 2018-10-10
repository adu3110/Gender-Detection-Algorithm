library(rmarkdown)
library(knitr)
library(ggplot2)
library(DT)
library(jpeg)
library(data.table)


WhiteBalance <- function(img){
  
  dim_img <- dim(img)
  img_wb <- array(data=0, dim = c(dim_img[1], dim_img[2], dim_img[3]))
  
  Rav <- mean(img[,,1])
  Gav <- mean(img[,,2])
  Bav <- mean(img[,,3])
  Greyav <- (Rav + Gav + Bav)/3
  Kr <- Greyav / Rav;
  Kg <- Greyav / Gav;
  Kb <- Greyav / Bav;
  img_wb[,, 1] <- Kr * img[, , 1]
  img_wb[,, 2] <- Kg * img[, , 2]
  img_wb[,, 3] <- Kb * img[, , 3]
  
  return(img_wb)
}


DetectFace <- function(img){
  
  img_wb <- WhiteBalance(img)
  
  dim_img <- dim(img)
  
  img_face_o <- array(data=0, dim = c(dim_img[1], dim_img[2], dim_img[3]))
  img_face <- array(data=0, dim = c(dim_img[1], dim_img[2], dim_img[3]))
  
  sapply(1:dim_img[1], function(i){
    sapply(1:dim_img[2], function(j){
      curr_rgb_o <- img[i, j, ]
      curr_rgb <- img_wb[i,j,]
      
      if((curr_rgb[1] > 95/255) & 
         (curr_rgb[2] > 40/255) & 
         (curr_rgb[3] > 20/255) & 
         (max(curr_rgb)-min(curr_rgb) > 15/255) & 
         abs((curr_rgb[1]-curr_rgb[2]) > 15/255) & 
         (curr_rgb[1] > curr_rgb[2]) & 
         (curr_rgb[1] > curr_rgb[3])){
        img_face[i,j, ] <<- 1
      }
      
      if((curr_rgb_o[1] > 95/255) & 
         (curr_rgb_o[2] > 40/255) & 
         (curr_rgb_o[3] > 20/255) & 
         (max(curr_rgb_o)-min(curr_rgb_o) > 15/255) & 
         abs((curr_rgb_o[1]-curr_rgb_o[2]) > 15/255) & 
         (curr_rgb_o[1] > curr_rgb_o[2]) & 
         (curr_rgb_o[1] > curr_rgb_o[3])){
        img_face_o[i,j, ] <<- 1
      }
      
      if (img_face_o[i,j, ] == 0 || img_face[i,j, ] == 0){
        img_face[i,j, ] <<- 0
      }
      
      
    })
  })
  
  return(img_face)
  
}

writeJPEG(DetectFace(readJPEG('/Users/apple/Desktop/GenderDetection/raw_images/Aamir_Khan.jpg')),
          '/Users/apple/Desktop/GenderDetection/Aamir_segmented.jpg')


ExtractFace <- function(img){
  dim_img <- dim(img)
  search_range <- c(floor(0.35*dim_img[2]), floor(0.65*dim_img[2]))
  search_range <- c(floor(0.35*dim_img[2]), floor(0.65*dim_img[2]))
  
  img_face_shaded <- DetectFace(img)
  
  row_means <- apply(img_face_shaded[,,1], 2, mean)
  img_centre <- search_range[1] + which.max(row_means[search_range[1]:search_range[2]]) - 1
  
  
  
  top_margin <- min(which(img_face_shaded[,img_centre,1]==1))
  bottom_margin <- max(which(img_face_shaded[,img_centre,1]==1))
  left_margin <- img_centre - min(which(row_means[img_centre:1] < 0.02))
  right_margin <- img_centre + min(which(row_means[img_centre:dim_img[2]] < 0.02))
  
  if((bottom_margin - top_margin) > 1.28*(right_margin - left_margin)){
    bottom_margin <- top_margin + floor(1.28 * (right_margin - left_margin))
  }
  
  print(c(top_margin, bottom_margin, left_margin, right_margin))
  img_face <- img[top_margin:bottom_margin, left_margin:right_margin, ]
  
  return(img_face)
  
}


ExtractAllFaces <- function(input_path, output_path){
  raw_image_files <- list.files(input_path, 
                                pattern = "*.jpg")
  
  lapply(raw_image_files, function(file_name){
    print(file_name)
    img <- readJPEG(paste0(input_path, file_name))
    curr_face <- ExtractFace(img)
    print(paste0(output_path, strsplit(file_name, ".", fixed=T)[[1]][1], "_face.jpg"))
    writeJPEG(curr_face, 
              paste0(output_path, strsplit(file_name, ".", fixed=T)[[1]][1], "_face.jpg"))
  })
  
}

ExtractAllFaces('/Users/apple/Desktop/GenderDetection/raw_images/males/',
                '/Users/apple/Desktop/GenderDetection/faces_test/males/')

ExtractAllFaces('/Users/apple/Desktop/GenderDetection/raw_images/females/',
                '/Users/apple/Desktop/GenderDetection/faces_test/females/')


Rescale4040 <- function(img){
  dim_img <- dim(img)
  img_grey <- matrix(data=0, nrow = dim_img[1], ncol = dim_img[2])
  
  sapply(1:dim_img[1], function(i){
    sapply(1:dim_img[2], function(j){
      img_grey[i, j] <<- mean(img[i, j, ])
    })
  })
  
  row_seq <- c(seq(1, dim_img[1], round(dim_img[1]/40)), dim_img[1]+1)
  col_seq <- c(seq(1, dim_img[2], round(dim_img[2]/40)), dim_img[2]+1)
  
  
  
  row_mult_matrix <- matrix(data=0, nrow=40, ncol=dim_img[1])
  
  
  sapply(1:min(40, length(row_seq)-1), function(i){
    if(row_seq[i] < dim_img[1]){
      row_mult_matrix[i,(row_seq[i]:min(row_seq[i+1]-1, dim_img[1]))] <<- 1/(round(dim_img[1]/40))
    }
      
  })
  
  col_mult_matrix <- matrix(data=0, nrow=dim_img[2], ncol=40)
  
  
  sapply(1:min(40, length(col_seq)-1), function(i){
    if(col_seq[i] < dim_img[2]){
      col_mult_matrix[(col_seq[i]:min(col_seq[i+1]-1, dim_img[2])),i] <<- 1/(round(dim_img[2]/40))  
    }
    
  })
  
  img_4040 <- row_mult_matrix %*% img_grey %*% col_mult_matrix
  
  image_grey_4040 <- array(data=0, dim = c(40, 40, 3))
  
  sapply(1:40, function(i){
    sapply(1:40, function(j){
      image_grey_4040[i, j, ] <<- img_4040[i, j]
    })
  })
  
  return(image_grey_4040)
}

RescaleAllFaces <- function(input_path, output_path){
  raw_image_files <- list.files(input_path, 
                                pattern = "*.jpg")
  
  lapply(raw_image_files, function(file_name){
    print(file_name)
    img <- readJPEG(paste0(input_path, file_name))
    curr_face <- Rescale2020(img)
    print(paste0(output_path, strsplit(file_name, ".", fixed=T)[[1]][1], "_4040.jpg"))
    writeJPEG(curr_face, 
              paste0(output_path, strsplit(file_name, ".", fixed=T)[[1]][1], "_4040.jpg"))
  })
  
}


RescaleAllFaces('/Users/apple/Desktop/GenderDetection/faces_test/males/',
                '/Users/apple/Desktop/GenderDetection/faces_test_40by40/males/')

RescaleAllFaces('/Users/apple/Desktop/GenderDetection/faces_test/females/',
                '/Users/apple/Desktop/GenderDetection/faces_test_40by40/females/')

RescaleAllFaces('/Users/apple/Desktop/GenderDetection/faces_reference/males/',
                '/Users/apple/Desktop/GenderDetection/faces_reference_40by40/males/')

RescaleAllFaces('/Users/apple/Desktop/GenderDetection/faces_reference/females/',
                '/Users/apple/Desktop/GenderDetection/faces_reference_40by40/females/')

library(dtw)

ImageAvg <- function(img){
  dim_img <- dim(img)
  img_grey <- matrix(data=0, nrow = dim_img[1], ncol = dim_img[2])
  
  sapply(1:dim_img[1], function(i){
    sapply(1:dim_img[2], function(j){
      img_grey[i, j] <<- mean(img[i, j, ])
    })
  })
  return(img_grey)
}

ImageAvgAll <- function(input_path){
  
  raw_image_files <- list.files(input_path, 
                                pattern = "*.jpg")
  
  res <- list()
  
  sapply(1:length(raw_image_files), function(i){
    file_name <- raw_image_files[[i]]
    print(file_name)
    img <- readJPEG(paste0(input_path, file_name))
    curr_avg <- ImageAvg(img)
    res[[i]] <<- curr_avg
    
  })
  
  return(res)
  
}


males_image_avg_train <- ImageAvgAll('/Users/apple/Desktop/GenderDetection/faces_reference_40by40/males/')
females_image_avg_train <- ImageAvgAll('/Users/apple/Desktop/GenderDetection/faces_reference_40by40/females/')

test_image_files_males <- list.files('/Users/apple/Desktop/GenderDetection/faces_test_40by40/males/')
test_image_files_females <- list.files('/Users/apple/Desktop/GenderDetection/faces_test_40by40/females/')

test_male_dist <- do.call('rbind',
                          lapply(test_image_files_males, function(filename){
                            curr_avg <- ImageAvg(readJPEG(paste0('/Users/apple/Desktop/GenderDetection/faces_test_40by40/males/', 
                                                                 filename)))
                            
                            curr_dist_male <- sapply(1:length(males_image_avg_train), function(i){
                              curr_dtw <- dtw(curr_avg, males_image_avg_train[[i]], 
                                              dist.method='Manhattan')
                              return(curr_dtw$distance)
                            })
                            
                            curr_dist_female <- sapply(1:length(females_image_avg_train), function(i){
                              curr_dtw <- dtw(curr_avg, females_image_avg_train[[i]], 
                                              dist.method='Manhattan')
                              return(curr_dtw$distance)
                            })
                            
                            curr_avg_male_dist <- mean(sort(curr_dist_male)[1:10])
                            curr_avg_female_dist <- mean(sort(curr_dist_female)[1:10])
                            
                            curr_dt <- data.table(filename = filename,
                                                  avg_male_dist = curr_avg_male_dist,
                                                  avg_female_dist = curr_avg_female_dist,
                                                  pred = ifelse(curr_avg_male_dist < curr_avg_female_dist,
                                                                'male',
                                                                'female'))
                            return(curr_dt)
                          }))


test_female_dist <- do.call('rbind',
                          lapply(test_image_files_females, function(filename){
                            curr_avg <- ImageAvg(readJPEG(paste0('/Users/apple/Desktop/GenderDetection/faces_test_40by40/females/', 
                                                                 filename)))
                            
                            curr_dist_male <- sapply(1:length(males_image_avg_train), function(i){
                              curr_dtw <- dtw(curr_avg, males_image_avg_train[[i]], 
                                              dist.method='Manhattan')
                              return(curr_dtw$distance)
                            })
                            
                            curr_dist_female <- sapply(1:length(females_image_avg_train), function(i){
                              curr_dtw <- dtw(curr_avg, females_image_avg_train[[i]], 
                                              dist.method='Manhattan')
                              return(curr_dtw$distance)
                            })
                            
                            curr_avg_male_dist <- mean(sort(curr_dist_male)[1:10])
                            curr_avg_female_dist <- mean(sort(curr_dist_female)[1:10])
                            
                            curr_dt <- data.table(filename = filename,
                                                  avg_male_dist = curr_avg_male_dist,
                                                  avg_female_dist = curr_avg_female_dist,
                                                  pred = ifelse(curr_avg_male_dist < curr_avg_female_dist,
                                                                'male',
                                                                'female'))
                            return(curr_dt)
                          }))


