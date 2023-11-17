# ------------------------------------------------------------------------------

#' A custom infix operator performing "not in" action
#'
#' @description
#' An operator that performs an inverse action of %in%
#'
#' @details
#' Returns TRUE if element is not within the target vector
#'
#' @return
#' Boolean vector
#'
#' @param x A vector of objects.
#' @param y A target vector to compare to.
#' @examples
#'
#' a = 1:10
#' b = 1:20
#' a %!in% b
#' @export
#' 
'%!in%' = function(x,y){!('%in%'(x,y))}

# ------------------------------------------------------------------------------
#' A function to generate plate layouts
#'
#' @description
#' `generate_well_plate_template()` This function is used to generate commonly used well plate templates in 
#' life sciences. Supports "24 well", "48 well", "96 well", "384 well" layouts. Last columns of the plate
#' could be reserved
#'
#' @details
#' Generates a layout matrix based on the specified format and if the last column should be reserved (NA values).
#' 
#' @return
#' Character matrix resembling specified plate
#'
#' @param plate_type The type of the plate to generate. Could be: "24w", "48w", "96w", "384w"
#' @param reserve_last_column Optional. Defaults to TRUE. Last column is reserved for control samples or something else
#' @examples
#'
#' plate_matrix = generate_well_plate_template(plate_type = "48w")
#' print(plate_matrix)
#' @export
#' 
generate_well_plate_template = function(plate_type, reserve_last_column = TRUE){
  
  supported_formats = c("24w", "48w", "96w", "384w")
  
  if (!(plate_type %in% supported_formats)){
    stop("Please provide one of the supported plate types: 24w, 48w, 96w, 384w")
  }
  
  if (plate_type == "24w"){
    plate_template = sapply(1:6, function(x) paste0(x, LETTERS[1:4]))
    colnames(plate_template) = 1:6
    rownames(plate_template) = LETTERS[1:4]
  }
  
  if (plate_type == "48w"){
    plate_template = sapply(1:8, function(x) paste0(x, LETTERS[1:6]))
    colnames(plate_template) = 1:8
    rownames(plate_template) = LETTERS[1:6]
  }
  
  if (plate_type == "96w"){
    plate_template = sapply(1:12, function(x) paste0(x, LETTERS[1:8]))
    colnames(plate_template) = 1:12
    rownames(plate_template) = LETTERS[1:8]
  }
  
  if (plate_type == "384w"){
    plate_template = sapply(1:24, function(x) paste0(x, LETTERS[1:16]))
    colnames(plate_template) = 1:24
    rownames(plate_template) = LETTERS[1:16]
  }
  
  if (reserve_last_column){
    plate_template[, ncol(plate_template)] = NA
  }
  
  return(plate_template)
}

# ------------------------------------------------------------------------------

#' A function to download trials for a set of diseases and drugs
#'
#' @description
#' `visualize_table_layout()` This function is used to visualize table layout based on the specified variable
#' @details
#' Generated PNG image showing distribution of samples on the plate based on the specified variable and its 
#' threshold if needed. Expected to be used internally. DOES NOT WORK IF INPUT MATRIX HAS NO NAMES FOR ROWS
#' AND COLUMNS
#'
#' @return
#' Nothing is returned. The function is expected to create an image in the specified path
#'
#' @param table A table matrix to visualize
#' @param condtion Optional. Character expression for a threshold value in a form ">5" or "==3"
#' @param levels_var Optional. Character vector of levels for a categorical variable
#' @param file_name_full A full path to store figure
#' @param table_label A character name for the table
#' @param gwidth Width of a column. Default is 100
#' @param gheight Height of a column. Default is 35
#' @examples
#'
#' matrix_sample = 1:16
#' matrix_sample = matrix(matrix_sample, ncol=4, nrow=4)
#' rownames(matrix_sample) = LETTERS[1:4]
#' colnames(matrix_sample) = as.character(1:4)
#' # All values above 5 will be in a different color
#' visualize_table_layout(table = matrix_sample, 
#'                        condtion = ">5",
#'                        file_name_full = "test.png", 
#'                        table_label = "Test table")
#' @export
#' @import "grid"
#' @import "gridExtra"
#' @import "openxlsx"
#' @import "gtable"
visualize_table_layout = function(table, condtion = NULL, levels_var = NULL, file_name_full, table_label, gwidth = 100, gheight = 35){
  
  #  condition should be given as character expression without indicating a value
  # levels_var should be given as a character vector
  
  find_cell = function(table, row, col, name="core-bg"){
    l =  table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  Grob_table = tableGrob(table)
  
  Combined_params = c(condtion, levels_var)
  
  if (all(is.null(Combined_params))){
    
    for (column in 1:ncol(table)){
      
      for (row in 1:nrow(table)){
        
        if (is.na(table[row,column])){
          
          Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
          Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= "#D3D3D3", lwd=1)
          Grob_table <- gtable_add_grob(Grob_table,
                                        grobs = grobTree(
                                          segmentsGrob( # diagonal line ul -> lr
                                            x0 = unit(0,"npc"),
                                            y0 = unit(1,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(0,"npc"),
                                            gp = gpar(lwd = 2.0)),
                                          segmentsGrob( # diagonal line ll -> ur
                                            x0 = unit(0,"npc"),
                                            y0 = unit(0,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(1,"npc"),
                                            gp = gpar(lwd = 2.0))),
                                        t = row+1, b = row +1, l = column+1, r = column+1)
        }
      }
    }
    png(filename =  file_name_full, units="px", width=gwidth*ncol(table), height=gheight*nrow(table))
    grid.newpage()
    grid.arrange(Grob_table,top = textGrob(label = table_label, gp = gpar(fontface = "bold",cex = 1.5)))
    dev.off()
    
  }
  
  if (!is.null(condtion)){
    
    condtion = paste0("table[row,column] ", condtion)
    
    for (column in 1:ncol(table)){
      
      for (row in 1:nrow(table)){
        
        if (is.na(table[row,column])){
          
          Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
          
          Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= "#D3D3D3", lwd=1)
          
          Grob_table <- gtable_add_grob(Grob_table,
                                        grobs = grobTree(
                                          segmentsGrob( # diagonal line ul -> lr
                                            x0 = unit(0,"npc"),
                                            y0 = unit(1,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(0,"npc"),
                                            gp = gpar(lwd = 2.0)),
                                          segmentsGrob( # diagonal line ll -> ur
                                            x0 = unit(0,"npc"),
                                            y0 = unit(0,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(1,"npc"),
                                            gp = gpar(lwd = 2.0))),
                                        t = row+1, b = row +1, l = column+1, r = column+1)
        } else if (eval(parse(text = condtion))){
          
          Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
          Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= RColorBrewer::brewer.pal("Set1", n = 3)[1], lwd=1)
          
        } else {
          Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
          Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= RColorBrewer::brewer.pal("Set1", n = 3)[2], lwd=1)
        }
      }
    }
    png(filename =  file_name_full, units="px", width=gwidth*ncol(table), height=gheight*nrow(table))
    grid.newpage()
    grid.arrange(Grob_table,top = textGrob(label = table_label, gp = gpar(fontface = "bold",cex = 1.5)))
    dev.off()
  }
  
  if (!is.null(levels_var)){
    Fills_palette = RColorBrewer::brewer.pal("Set1", n = 9)
    Fills_palette = c(Fills_palette, RColorBrewer::brewer.pal("Set3", n = 12))
    
    for (column in 1:ncol(table)){
      
      for (row in 1:nrow(table)){
        
        if (is.na(table[row,column])){
          
          Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
          
          Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= "#D3D3D3", lwd=1)
          
          Grob_table <- gtable_add_grob(Grob_table,
                                        grobs = grobTree(
                                          segmentsGrob( # diagonal line ul -> lr
                                            x0 = unit(0,"npc"),
                                            y0 = unit(1,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(0,"npc"),
                                            gp = gpar(lwd = 2.0)),
                                          segmentsGrob( # diagonal line ll -> ur
                                            x0 = unit(0,"npc"),
                                            y0 = unit(0,"npc"),
                                            x1 = unit(1,"npc"),
                                            y1 = unit(1,"npc"),
                                            gp = gpar(lwd = 2.0))),
                                        t = row+1, b = row +1, l = column+1, r = column+1)
        } else {
          
          index_palette = which(levels_var == table[row,column])
          
          if (length(index_palette) >=1){
            Current_index = find_cell(Grob_table, row + 1, column + 1, "core-bg")
            Grob_table$grobs[Current_index][[1]][["gp"]] = gpar(fill= Fills_palette[index_palette], lwd=1)
            
          }
        }
      }
    }
    png(filename =  file_name_full, units="px", width=gwidth*ncol(table), height=gheight*nrow(table))
    grid.newpage()
    grid.arrange(Grob_table,top = textGrob(label = table_label, gp = gpar(fontface = "bold",cex = 1.5)))
    dev.off()
  }
}

# ------------------------------------------------------------------------------

#' A function to generate random layout for a dataset with samples
#'
#' @description
#' `plate_layout_randomizer()` This function is used to generate randomized layouts for specified plates
#' @details
#' Randomly distributes samples on the specified plate format.Tries to allocate samples from same study and with the same confounder on one plate.
#' Distribution of samples may be sub-optimal. If sizes of studies are less than size of a plate and empty cells are not an issue, the
#' best practice is to use randomization per study and not mix samples from different studies.
#'
#' @return
#' A list with randomized samples
#'
#' @param df_all_studies A dataframe containing information on Sample ID, Study, Type, Confounder (should not be named as "Confounder")
#' @param column_study A number indicating a column with study of origin
#' @param column_sample_type A number indicating a column with sample types
#' @param column_sample_ID A number indicating a column with sample IDs
#' @param column_conf A number indicating a column with confounders
#' @param plate_template A matrix where each cell has a character name, and rows and columns are named. Cells with NA will be skipped
#' @examples
#'
#' data("example_data")
#' dataset = example_data
#' head(dataset)
#' plate_template = generate_well_plate_template(plate_type = "96w", 
#' reserve_last_column = TRUE)
#' plate_template
#' test_randomization = plate_layout_randomizer(df_all_studies = dataset,  
#' column_study = 5, 
#' column_sample_type = 2, 
#' column_sample_ID = 1, 
#' column_conf = 7, 
#' plate_template = plate_template)
#' test_randomization
#' @export
#' @import "grid"
#' @import "gridExtra"
#' @import "openxlsx"
#' @import "gtable"
#' @import "stringi"
#' @import "dplyr"
plate_layout_randomizer = function(df_all_studies,
                                   column_study, 
                                   column_sample_type, 
                                   column_sample_ID, 
                                   column_conf, 
                                   plate_template){
  
  # renaming columns for convenience
  colnames(df_all_studies)[column_study] = "Study"
  colnames(df_all_studies)[column_sample_type] = "Sample_type"
  colnames(df_all_studies)[column_sample_ID] = "Sample_ID"
  colnames(df_all_studies)[column_conf] = "Confounder"
  
  if (any(duplicated(df_all_studies$Sample_ID))){
    stop("There are duplicated samples in the data! Revise it!")
  }
  
  # generating plate template
  plate_capacity = length(plate_template[!is.na(plate_template)])
  
  # generating plate list
  plates_required = ceiling(nrow(df_all_studies)/plate_capacity)
  templates_list = 1:plates_required
  templates_list = lapply(templates_list, function(x){
    plate = plate_template
    return(plate)
  })
  
  # preparing lists
  names(templates_list) = paste0("Plate_", 1:plates_required)
  study_pool = df_all_studies[,column_study]
  study_pool = unique(study_pool)
  current_study_DF = df_all_studies[0,]
  
  # iterating through plates
  for (i in 1:length(templates_list)){
    
    current_Plate = templates_list[[i]]
    
    # current study is over
    if (nrow(current_study_DF) < 1){
      
      if (length(study_pool) < 1) {break}
      
      current_study_DF = df_all_studies[df_all_studies$Study == study_pool[1],]
      study_pool = study_pool[-1]
      
    }
    
    # current study size is equal to the plate capacity
    if (nrow(current_study_DF) == plate_capacity){
      
      current_study_DF = current_study_DF[sample(x = 1:nrow(current_study_DF), size = plate_capacity),]
      current_study_DF = dplyr::arrange(current_study_DF, Sample_type)
      
      for (row in 1:nrow(current_Plate)){
        
        for (column in 1:ncol(current_Plate)){
          
          if (is.na(current_Plate[row,column])){next}
          
          random_sample = current_study_DF[1,]
          random_sample = random_sample$Sample_ID
          current_study_DF = current_study_DF[current_study_DF$Sample_ID != random_sample,]
          
          if (stri_detect_fixed(random_sample, pattern = "REPLACEMENT_")){random_sample = NA}
          current_Plate[row,column] = random_sample
          
        }
      }
      
      templates_list[[i]] = current_Plate
      
    } else if (nrow(current_study_DF) < plate_capacity){
      
      # current study size is less than the plate capacity
      
      if (length(study_pool) < 1){
        
        # this study is the last, and plate as well
        
        index_replacement = nrow(current_study_DF)
        current_study_DF[(index_replacement + 1):plate_capacity,] = NA
        current_study_DF$Sample_ID[(index_replacement+1):plate_capacity] = paste0("REPLACEMENT_", 1:(plate_capacity-index_replacement))
        current_study_DF = current_study_DF[sample(x = 1:nrow(current_study_DF), size = plate_capacity),]
        current_study_DF = dplyr::arrange(current_study_DF, Sample_type)
        
        for (row in 1:nrow(current_Plate)){
          
          for (column in 1:ncol(current_Plate)){
            
            if (is.na(current_Plate[row,column])){next}
            
            random_sample = current_study_DF[1,]
            random_sample = random_sample$Sample_ID
            current_study_DF = current_study_DF[current_study_DF$Sample_ID != random_sample,]
            if (stri_detect_fixed(random_sample, pattern = "REPLACEMENT_")){random_sample = NA}
            current_Plate[row,column] = random_sample
          }
        }
        templates_list[[i]] = current_Plate
        
      } else {
        
        # this study is not the last, and plate as well
        
        while (nrow(current_study_DF) < plate_capacity){
          
          # This part builds up the current_study_DF
          supplementary_Df = current_study_DF
          current_study_DF = df_all_studies[df_all_studies$Study == study_pool[1],]
          study_pool = study_pool[-1]
          current_study_DF = rbind(supplementary_Df, current_study_DF)
          
        }
        
        studies_to_distribute = unique(current_study_DF$Study)
        study_to_split = studies_to_distribute[length(studies_to_distribute)]
        distrib_DF = current_study_DF[current_study_DF$Study != study_to_split,]
        df_to_pass = current_study_DF[current_study_DF$Study == study_to_split,]
        
        while (nrow(distrib_DF) < plate_capacity){
          
          # This part attaches missing rows in distrib DF
          attachment = df_to_pass[sample(x = 1:nrow(df_to_pass), size = 1),]
          if (is.na(attachment$Confounder)){
            
            distrib_DF = rbind(distrib_DF, attachment)
            df_to_pass = df_to_pass[df_to_pass$Sample_ID != attachment$Sample_ID,]
            
          } else {
            
            attachment = df_to_pass[df_to_pass$Confounder == attachment$Confounder,]
            
            if ((nrow(distrib_DF) + nrow(attachment)) > plate_capacity){
              excess_rows = (nrow(distrib_DF) + nrow(attachment)) - plate_capacity
              attachment = attachment[sample(1:nrow(attachment), size = nrow(attachment), replace = FALSE),] # additional randomization for big attachments
              attachment = attachment[1:(nrow(attachment) - excess_rows),]
              
            }
            
            distrib_DF = rbind(distrib_DF, attachment)
            df_to_pass = df_to_pass[df_to_pass$Sample_ID %!in% distrib_DF$Sample_ID,]
            
          }
        }
        
        distrib_DF = distrib_DF[sample(x = 1:nrow(distrib_DF), size = plate_capacity),]
        distrib_DF = dplyr::arrange(distrib_DF, Sample_type)
        
        for (row in 1:nrow(current_Plate)){
          for (column in 1:ncol(current_Plate)){
            if (is.na(current_Plate[row,column])){next}
            random_sample = distrib_DF[1,]
            random_sample = random_sample$Sample_ID
            distrib_DF = distrib_DF[distrib_DF$Sample_ID != random_sample,]
            if (stri_detect_fixed(random_sample, pattern = "REPLACEMENT_")){random_sample = NA}
            current_Plate[row,column] = random_sample
          }
        }
        templates_list[[i]] = current_Plate
        current_study_DF = df_to_pass
      }
      
    } else {
      
      # This means that current_study_DF is > plate_capacity, and it is possible only within a big study
      
      distrib_DF = current_study_DF[1,]
      current_study_DF = current_study_DF[-1,]
      
      if (!is.na(distrib_DF$Confounder)){
        attachment = current_study_DF[current_study_DF$Confounder == distrib_DF$Confounder,]
        
        if ((nrow(distrib_DF) + nrow(attachment)) > plate_capacity){
          excess_rows = (nrow(distrib_DF) + nrow(attachment)) - plate_capacity
          attachment = attachment[sample(1:nrow(attachment), size = nrow(attachment), replace = FALSE),] # additional randomization for big attachments
          attachment = attachment[1:(nrow(attachment) - excess_rows),]
        }
        
        if (nrow(attachment) >= 1){
          distrib_DF = rbind(distrib_DF, attachment)
        }
        current_study_DF = current_study_DF[current_study_DF$Sample_ID %!in% distrib_DF$Sample_ID,]
      }
      
      while (nrow(distrib_DF)<plate_capacity){
        
        # this part attaches missing rows in distrib DF
        attachment = current_study_DF[sample(x = 1:nrow(current_study_DF), size = 1),]
        
        if (is.na(attachment$Confounder)){
          distrib_DF = rbind(distrib_DF, attachment)
          current_study_DF = current_study_DF[current_study_DF$Sample_ID != attachment$Sample_ID,]
          
        } else {
          
          attachment = current_study_DF[current_study_DF$Confounder == attachment$Confounder,]
          
          if ((nrow(distrib_DF) + nrow(attachment)) > plate_capacity){
            excess_rows = (nrow(distrib_DF) + nrow(attachment)) - plate_capacity
            attachment = attachment[sample(1:nrow(attachment), size = nrow(attachment), replace = FALSE),] # Additional randomization for big attachments
            attachment = attachment[1:(nrow(attachment) - excess_rows),]
          }
          
          distrib_DF = rbind(distrib_DF, attachment)
          current_study_DF = current_study_DF[current_study_DF$Sample_ID %!in% distrib_DF$Sample_ID,]
        }
      }
      
      distrib_DF = distrib_DF[sample(x = 1:nrow(distrib_DF), size = plate_capacity),]
      distrib_DF = dplyr::arrange(distrib_DF, Sample_type)
      
      for (row in 1:nrow(current_Plate)){
        
        for (column in 1:ncol(current_Plate)){
          
          if (is.na(current_Plate[row,column])){next}
          
          random_sample = distrib_DF[1,]
          random_sample = random_sample$Sample_ID
          distrib_DF = distrib_DF[distrib_DF$Sample_ID != random_sample,]
          
          if (stri_detect_fixed(random_sample, pattern = "REPLACEMENT_")){random_sample = NA}
          
          current_Plate[row,column] = random_sample
        }
      }
      templates_list[[i]] = current_Plate
    }
  }
  return(templates_list)
}

# ------------------------------------------------------------------------------

#' A function to generate random layout for a dataset with samples and visualize variable across plates
#'
#' @description
#' `randomizer_main()` This function is used to generate randomized layouts for specified plates specified number of times. Saves
#' outputs as excel. Saves visualizations of variables for each layout
#' 
#' @details
#' Randomly distributes samples on the specified plate format.Tries to allocate samples from same study and with the same confounder on one plate.
#' Distribution of samples may be sub-optimal. If sizes of studies are less than size of a plate and empty cells are not an issue, the
#' best practice is to use randomization per study and not mix samples from different studies. 
#' 
#' Generates visualization of layouts by specified columns in column_numbers_vis
#'
#' @return
#' Nothing. All outputs are saved in the specified directories
#'
#' @param df_studies A dataframe containing information on Sample ID, Study, Type, Confounder (should not be named as "Confounder")
#' @param col_num_study A number indicating a column with study of origin
#' @param col_num_sample_ID A number indicating a column with sample IDs
#' @param col_num_sample_type A number indicating a column with sample types
#' @param col_num_conf A number indicating a column with confounders
#' @param column_numbers_vis A numeric vector of integers indicating columns with variables for visualization
#' @param plate_template A matrix where each cell has a character name, and rows and columns are named. Cells with NA will be skipped
#' @param randomiz_number Number of randomizations to perform (default is 10)
#' @param main_dir The path to main directory for raw figures and excel files (default: "randomiz_vis")
#' @param dir_combined_img The path to directory for combined images (default: "randomiz_vis_combined_img")
#' @examples
#'
#' data("example_data")
#' dataset = example_data
#' head(dataset)
#' plate_template = generate_well_plate_template(plate_type = "96w", 
#' reserve_last_column = TRUE)
#' plate_template
#' randomizer_main(df_studies = dataset,  
#' col_num_study = 5, 
#' col_num_sample_type = 2, 
#' col_num_sample_ID = 1, 
#' col_num_conf = 7, 
#' column_numbers_vis = c(2,7,3,4,5),
#' plate_template = plate_template)
#' @export
#' @import "grid"
#' @import "gridExtra"
#' @import "openxlsx"
#' @import "gtable"
#' @import "stringi"
#' @import "dplyr"
randomizer_main = function(df_studies,
                           col_num_study,
                           col_num_sample_ID,
                           col_num_sample_type,
                           col_num_conf,
                           column_numbers_vis,
                           plate_template,
                           randomiz_number = 10,
                           main_dir = "randomiz_vis",
                           dir_combined_img = "randomiz_vis_combined_img"){
  # making directories
  dir.create(main_dir)
  dir.create(dir_combined_img)
  
  # helper function to alter layout
  alter_layout = function(layout_list_ID, col_ID, column_replacement, ref_df){
    changed_layouts = lapply(layout_list_ID, function(x){
      
      gen_matrix = apply(x, 2, function(z){
        
        matrix_column = sapply(z, function(value){
          
          if(is.na(value)){return(NA)}
          IDs = ref_df[,col_ID]
          index = which(IDs == value)
          value = ref_df[index, column_replacement]
          return(value)
          
        })
        
        return(matrix_column)
      })
      
      return(gen_matrix)
    })
    
    return(changed_layouts)
  }
  
  # prepare dataset
  colnames(df_studies)[col_num_study] = "Study"
  colnames(df_studies)[col_num_sample_ID] = "Sample_ID"
  colnames(df_studies)[col_num_sample_type] = "Sample_type"
  colnames(df_studies)[col_num_conf] = "Confounder"
  
  # make randomizations
  randomized_templates = 1:randomiz_number
  randomized_templates = lapply(randomized_templates, FUN = function(x){
    x = plate_layout_randomizer(df_all_studies = df_studies,
                                column_sample_type = col_num_sample_type,
                                column_study = col_num_study,
                                column_sample_ID = col_num_sample_ID, 
                                column_conf = col_num_conf,
                                plate_template = plate_template)
    return(x)
  })
  
  
  # visualizations of randomization and saving outputs
  column_numbers_vis = c(NA, column_numbers_vis) # First element is base
  
  for (layout in 1:length(randomized_templates)){
    
    curruent_layouts = randomized_templates[[layout]]
    
    # saving excel files
    openxlsx::write.xlsx(x = curruent_layouts, file = paste0(main_dir, "/", layout,"_radom_table.xlsx"), rowNames = TRUE, colNames = TRUE)
    
    data_list = list()
    
    for (i in 1:length(curruent_layouts)){
      simplif_layout = as.vector(curruent_layouts[[i]])
      data_list[[i]] = data.frame(Plate = names(curruent_layouts)[i], Well = as.vector(plate_template), Sample_ID = simplif_layout)
    }
    data_list = do.call(rbind, data_list)
    openxlsx::write.xlsx(x = data_list, file = paste0(main_dir, "/", layout,"_radom_table_simplif.xlsx"), colNames = TRUE)
    
    # visualizing variables
    base_layouts_names = paste0(main_dir, "/",layout, "_1_", 1:length(curruent_layouts), "_", ".png")
    
    # "1_1_1_.png" first number = number of random layout, second number = vis index (base is always 1), third number shows plate
    
    mapply(function(x,y,z){
      visualize_table_layout(x, file_name_full = y, table_label = z,gheight = 30, gwidth = 105)
    }, curruent_layouts, base_layouts_names, names(curruent_layouts))
    
    for (vis in 2:length(column_numbers_vis)){
      
      current_index_column = column_numbers_vis[vis]
      current_column = df_studies[,current_index_column]
      current_levels = unique(current_column)
      visualiz_names = paste0(main_dir, "/", layout, "_", vis,"_", 1:length(curruent_layouts), "_", ".png")
      new_layout = alter_layout(layout_list_ID = curruent_layouts, col_ID = col_num_sample_ID, column_replacement = current_index_column, ref_df = df_studies)
      
      mapply(function(x,y,z){
        visualize_table_layout(x, file_name_full = y, table_label = z,gheight = 30, gwidth = 105, levels_var = current_levels)
      }, new_layout, visualiz_names, names(curruent_layouts))
    }
    
    
    # generating combined image
    layout_images_in_folder = list.files(main_dir)
    layout_images_in_folder = layout_images_in_folder[stringi::stri_detect_fixed(layout_images_in_folder, pattern = ".png")]
    layout_images_in_folder = layout_images_in_folder[sapply(layout_images_in_folder, function(x){
      Image = x
      Image = stringi::stri_split_fixed(Image, pattern = "_")
      Image = unlist(Image)[1]
      Response = ifelse(Image == layout, TRUE, FALSE)
      return(Response)
    })]
    
    # Explicit sorting of files
    layout_images_in_folder_df = data.frame(File = layout_images_in_folder)
    layout_images_in_folder_df$Layout = sapply(layout_images_in_folder, function(x){
      x = stringi::stri_split_fixed(x, pattern = "_")
      x = unlist(x)[1]
      return(x)})
    layout_images_in_folder_df$Vis = sapply(layout_images_in_folder, function(x){
      x = stringi::stri_split_fixed(x, pattern = "_")
      x = unlist(x)[2]
      return(x)})
    layout_images_in_folder_df$Plate = sapply(layout_images_in_folder, function(x){
      x = stringi::stri_split_fixed(x, pattern = "_")
      x = unlist(x)[3]
      return(x)})
    layout_images_in_folder_df$Plate = as.numeric(layout_images_in_folder_df$Plate)
    layout_images_in_folder_df = dplyr::group_by(layout_images_in_folder_df, Vis)
    layout_images_in_folder_df =  dplyr::arrange(layout_images_in_folder_df, Plate, .by_group = TRUE)
    layout_images_in_folder = layout_images_in_folder_df$File
    
    layout_images_in_folder = paste0(main_dir, "/", layout_images_in_folder)
    image_list  =  lapply(layout_images_in_folder, png::readPNG)
    image_grobs = lapply(image_list, grid::rasterGrob)
    combined_file_name = paste0(dir_combined_img, "/", layout, "_combined_image.png")
    height = nrow(image_list[[1]])
    width = ncol(image_list[[1]])
    
    # generating PNG
    png(filename = combined_file_name, units = "px", width = width*length(curruent_layouts), height = height*length(column_numbers_vis))
    grid::grid.newpage()
    gridExtra::grid.arrange(grobs = image_grobs, nrow = length(column_numbers_vis), ncol = length(curruent_layouts))
    dev.off()
  }
}