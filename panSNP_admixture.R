# ------------------------------------------------------
# File: ADMIXTURE with PanSNP database
# Purpose: Brief description of the script
# Author: Pirada Naewkam
# Date: 2025-08-22
#
# Description:
#   Detailed description or steps
#
# Input: input files or data
# Output: output files or results
# ------------------------------------------------------


# แบ่งตาม region ----------------------------------------------------------

# Load library
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Parameters
k_values <- 2:10

# Paths
q_dir <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/panSNP_admixture/results/"
fam_path <- file.path(q_dir, "PanSNPdb_fig2_ancient_geno0.1.fam")
metadata_path <- "C:/Users/patch/Downloads/Book2.xlsx"
genetic_id_path <- "C:/Users/patch/Downloads/1928_inds_ID.txt"

# Paths for missingness data
# missingness_file_path <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/start_all_over_again/dedup_new_selected/maf/missingness_report.imiss"
# missingness_data <- read.table(missingness_file_path, header = TRUE, stringsAsFactors = FALSE) %>% 
#   select(`Genetic ID` = IID, F_MISS) %>% 
#   mutate(
#     `Genetic ID` = case_when(
#       `Genetic ID` == "human1" ~ "Human1",
#       `Genetic ID` == "human2" ~ "Human2",
#       TRUE ~ `Genetic ID` # ถ้าไม่ใช่ก็คงค่าเดิมไว้
#     ),
#     F_MISS_percent = sprintf("%.2f%%", F_MISS * 100)
#   ) %>% 
#   as_tibble()

# Load fam and metadata
fam <- read.table(fam_path, header = FALSE, stringsAsFactors = FALSE)
fam$V2[fam$V2 == "human1"] <- "Human1"
fam$V2[fam$V2 == "human2"] <- "Human2"
fam[fam$V2 == "human2_dup|human2", 2] <- "Human2"
colnames(fam) <- c("FID", "Genetic ID", "Father ID", "Mother ID", "Sex", "Phenotype")

col_names_for_metadata <- c(
  "Group_ID",
  "Political Entity",
  "Human",
  "Language",          # คอลัมน์ Japanese ตัวที่สอง (อาจจะเป็น Language หรือ Subgroup ก็ได้)
  "Language_family",
  "Number?",
  "Region"
)

metadata <- read_xlsx(metadata_path, col_names = col_names_for_metadata)

# time slot for label
# df_date <- read_xlsx(metadata_path) %>%
#   select(`Genetic ID`, `Full Date`) %>%
#   rename("ori full date" = `Full Date`) %>%
#   mutate(`ori full date` = ifelse(`ori full date` == "present", "present",
#                                   sub("\\(.*", "", `ori full date`) %>%
#                                     trimws()
#   )
# )

# Merge fam and metadata together
fam_merged <- fam %>%
  left_join(read.table(genetic_id_path, header = TRUE) %>% select(c(sample.id, population.abbreviation)),
            by = c(`Genetic ID` = "sample.id")) %>% 
  select(-c(`Father ID`, `Mother ID`, Sex, Phenotype)) %>% 
  left_join(metadata, by = c(population.abbreviation = "Group_ID")) %>% 
  rename("Group ID" = population.abbreviation) %>% 
  distinct()


# Formatting Data ---------------------------------------------------------

# Define region groups
china_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "China"]
mainland_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Thailand","Cambodia","Myanmar","Laos","Vietnam")]
island_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Indonesia","Philippines","Singapore","Brunei","Malaysia")]
eastasia_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Mongolia","Taiwan","South Korea","Japan")]
southasia_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("India","Pakistan","Bangladesh","Nepal","Sri Lanka")]
oceania_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "Papua New Guinea"]
europe_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Russia", "USA")]
africa_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "Nigeria"]

region_order <- c("Human1", "Human2", "NW China", "NE China", "Central China", "South China",
                  "Mainland SEA", "Island SEA", "South Asia", "Oceania", "European", "Africa", "Other")

# Process fam_merged_final
fam_merged_final <- fam_merged %>%
  mutate(
    Region = case_when(
      `Genetic ID` == "Human1" ~ "Human1",
      `Genetic ID` == "Human2" ~ "Human2",
      
      `Group ID` %in% c("Bonan.HO", "Qiang.HO", "Dongxiang.HO", "Yugur.HO", "Tibetan.HO",
                        "Uyghur.DG", "Uyghur.HO", "Uyghur.SG", "Kazakh_China.HO",
                        "Kyrgyz_China.HO", "Salar.HO", "Xibo.DG", "Xibo.HO",
                        "Tu.DG", "Tu.HO", "Tibetan_Yunnan.HO") ~ "NW China",
      
      `Group ID` %in% c("Oroqen.DG", "Oroqen.HO", "Daur.HO", "Hezhen.DG", "Hezhen.HO",
                        "Mongola.DG", "Mongola.HO") | `Group ID` %in% eastasia_ids ~ "NE China",
      
      `Group ID` %in% c("Han.DG", "Han.HO", "Tujia.DG", "Tujia.HO", "CHB.DG", "CHB.SG",
                        "China_Lahu.DG", "China_Lahu.HO", "She.DG", "She.HO") ~ "Central China",
      
      `Group ID` %in% c("Mulam.HO", "Li.HO", "Dong.HO", "Gelao.HO", "CHS.DG", "CHS.SG",
                        "CDX.SG", "CDX.DG", "Miao.DG", "Miao.HO", "Yi.DG", "Yi.HO",
                        "Dai.HO", "Naxi.DG", "Naxi.HO", "Maonan.HO", "Zhuang.HO") ~ "South China",
      
      `Group ID` %in% mainland_ids  ~ "Mainland SEA",
      `Group ID` %in% island_ids    ~ "Island SEA",
      `Group ID` %in% southasia_ids ~ "South Asia",
      `Group ID` %in% oceania_ids   ~ "Oceania",
      `Group ID` %in% europe_ids    ~ "European",
      `Group ID` %in% africa_ids    ~ "Africa",
      
      TRUE ~ "Other"
    ),
    Region = factor(Region, levels = region_order),   # ถ่าไม่ทำเป็น factor มันจะเรียงแบบอักษรศาสตร์ทันที
    
    # SampleType = case_when(`ori full date` == "present" ~ "Modern",
    #                        TRUE ~ "Ancient"),
    # SampleType = factor(SampleType, levels = c("Ancient", "Modern")),
    
    # GID_LanSampleType = case_when(
    #   `Genetic ID` %in% c("Human1", "Human2") ~ paste0("**", `Genetic ID`, "**"),
    #   SampleType == "Ancient" ~ paste0("**", `Group ID`, "_", `Language family`, "**"),
    #   SampleType == "Modern" ~ paste0(`Group ID`, "_", `Language family`)
    # ),
    
  ) %>%
  arrange(Region, `Political Entity`) %>%
  mutate(SampleOrder = row_number()) %>%
  distinct() %>%
  as_tibble()

# กำหนด constant color ก่อนเข้า function
ancestry_colors <- c(
  Ancestry1 = "#FF0000",
  Ancestry2 = "#FFFF00",
  Ancestry3 = "#80FF00",
  Ancestry4 = "#0000FF",
  Ancestry5 = "#FF00FF",
  Ancestry6 = "#800080",
  Ancestry7 = "#00FA9A",
  Ancestry8 = "#1E90FF",
  Ancestry9 = "#FF4500",
  Ancestry10 = "#008080",
  Ancestry11 = "#8A2BE2",
  Ancestry12 = "#4B0082",
  Ancestry13 = "#00FFFF",
  Ancestry14 = "#8B4513",
  Ancestry15 = "#FF1493",
  Ancestry16 = "#FFD700",
  Ancestry17 = "#808080",
  Ancestry18 = "#6B8E23",
  Ancestry19 = "#87CEEB",
  Ancestry20 = "#FF6347"
)

# สร้าง ggplot แต่ละค่า k ตาม region -------------------------------------

create_ggplot <- function(k) {
  q_file <- file.path(q_dir, sprintf("PanSNPdb_fig2_ancient_geno0.1.%d.Q", k))
  q_data <- read.table(q_file, header = FALSE)
  
  # ดึง proportion จาก q file ออกมา
  q_subset <- cbind(fam[, "Genetic ID", drop = FALSE], q_data)
  colnames(q_subset)[2:(k+1)] <- paste0("Ancestry", 1:k)
  
  # สร้าง dataframe เพื่อดึง fam_merged_final มารวมกับ proportion ของ q file
  df <- fam_merged_final %>%
    left_join(q_subset, by = "Genetic ID") %>%
    select(SampleOrder, `Genetic ID`, Region, starts_with("Ancestry"))
  
  # เปลี่ยน dataframe ให้เป็น long format เพื่อเตรียมพล็อตใน ggplot
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Ancestry"),
      names_to = "Ancestry",
      values_to = "Proportion"
    )
  
  # แก้ลำดับ legend ให้ถูกต้อง (Ancestry1, 2, 3, ... k)
  df_long$Ancestry <- factor(df_long$Ancestry, levels = rev(paste0("Ancestry", 1:k)))
  
  # สร้างเส้นแบ่ง region
  region_boundaries <- fam_merged_final %>%
    group_by(Region) %>%
    summarise(
      start = min(SampleOrder),
      end = max(SampleOrder),
      mid = (start + end) / 2
    ) %>%
    ungroup()
  
  # plotting
  ggplot(df_long, aes(x = SampleOrder, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity", width = 1) +
    
    geom_vline(
      data = region_boundaries,
      aes(xintercept = end + 0.5),
      color = "black",
      linewidth = 0.7
    ) +
    
    scale_x_continuous(
      breaks = region_boundaries$mid,
      labels = region_boundaries$Region,
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    
    scale_fill_manual(values = rev(ancestry_colors[1:k]),
                      breaks = paste0("Ancestry", 1:k)   # คุม legend ให้ตามลำดับเหมือนเดิม
    ) +
    
    labs(
      title = paste("K =", k),
      x = "Region",
      y = "Ancestry Proportion"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}

# เก็บ ggplot แต่ละค่า k ผ่าน create_ggplot function ไว้ใน list
ggplot_list <- lapply(k_values, create_ggplot)

# รวมกราฟ ggplot ในภาพเดียวแบบแนวตั้ง
combined_ggplot <- grid.arrange(grobs = ggplot_list, ncol = 1, heights = rep(1, length(k_values)))

# บันทึกรูปภาพ
ggsave(
  filename = "C:/Users/patch/Desktop/structureK_all.png",
  plot = combined_ggplot,
  width = 80,
  height = 6 * length(k_values),
  dpi = 300,
  limitsize = FALSE,
  device = "png"
)

cat("Your plot is ready on the Desktop! 🎉")


# สร้าง ggplot ด้วย custom k ตาม region -----------------------------------

k_new <- 12
custom_plot <- create_ggplot(k_new)

# บันทึกรูปภาพ
ggsave(
  filename = file.path("C:/Users/patch/Desktop/", paste0("structureK_", k_new, ".png")),
  plot = custom_plot,
  width = 12.5,
  height = 7.5,
  dpi = 300,
  limitsize = FALSE,
  device = "png",
  bg = "white"
)

cat("Your plot is ready on the Desktop! 🎉")


# พล็อต base R barplot ---------------------------------------------

# สร้างฟังก์ชันกลาง
create_base_r_barplot_original <- function(k, df_meta, label_var, plot_title, cex_names, cex_legend) {
  
  # --- Logic หลักในการพล็อต ---
  q_file <- file.path(q_dir, sprintf("PanSNPdb_fig2_ancient_geno0.1.%d.Q", k))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  colnames(mat) <- fam$`Genetic ID`
  
  ordered_ids <- df_meta$`Genetic ID`
  label_ids <- df_meta[[label_var]] # ใช้ [[...]] เพื่อดึงคอลัมน์จากชื่อที่เป็น string
  
  mat_sorted <- mat[, ordered_ids, drop = FALSE]
  
  # พล็อตโดยใช้สีตามจำนวน k ที่ถูกต้อง
  barplot_heights <- barplot(
    mat_sorted,
    col = ancestry_colors[1:k],
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = cex_names,
    main = plot_title,
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  # Legend
  legend(
    x = max(barplot_heights) + 5,
    y = 1,
    legend = paste0("Ancestry", 1:k),
    fill = ancestry_colors[1:k],
    border = "black",
    cex = cex_legend,
    bty = "n"
  )
}

# พล็อตทุกค่า k
png("C:/Users/patch/Desktop/barK_all.png",
    width = 40000,
    height = 800 * length(k_values),
    res = 300)
par(mfrow = c(length(k_values), 1), mar = c(4, 5, 4, 8))

for (k in k_values) {
  create_base_r_barplot_original(
    k = k,
    df_meta = fam_merged_final,
    label_var = "GID_LanSampleType",
    plot_title = paste("Base R barplot: K =", k),
    cex_names = 0.4,
    cex_legend = 0.6
  )
}

dev.off()
cat("Base R plot (all samples) is ready on the Desktop! 🎉\n")


# พล็อต base R barplot ด้วย custom k --------------------------------------

png(file.path("C:/Users/patch/Desktop/", paste0("barK", k_new, ".png")),
    width = 48000,
    height = 1200,
    res = 300)
par(mfrow = c(1, 1), mar = c(4, 5, 4, 8))

create_base_r_barplot_original(
  k = k_new,
  df_meta = fam_merged_final,
  label_var = "GID_LanSampleType",
  plot_title = paste("Base R barplot with borders: K =", k_new),
  cex_names = 0.2,
  cex_legend = 0.8
)

dev.off()
cat(paste("Your custom plot for K=", k_new, " is ready on the Desktop! 🎉\n"))


# เอา Ancient ขึ้นมาข้างหน้าให้หมด ----------------------------------------------------------

# กรองมาแค่ ancient sample จาก SampleType
ancient_only <- fam_merged_final %>%
  filter(SampleType == "Ancient") %>%
  
  # จัด label ใน GID_LanSampleType ใหม่
  mutate(
    GID_LanSampleType = case_when(
      `Genetic ID` %in% c("Human1", "Human2") ~ `Group ID`,
      TRUE ~ paste0(`Group ID`, " (", `ori full date`, ")")
    )
  ) %>%
  arrange() %>% 
  
  # เรียงลำดับ SampleOrder ใหม่
  mutate(SampleOrder = row_number()) %>%
  distinct()


# sort แค่ Ancient ใหม่ ให้เป็นระบบ ---------------------------------------

get_start_year <- function(date_string) {
  
  # ถ้า date_string เป็น NA หรือ "present" ให้ถือว่าเป็นปีใหม่สุด
  if (is.na(date_string) || date_string == "present") {
    return(2024)
  }
  
  # ดึงเฉพาะส่วนที่เป็นตัวเลขออกมา (จัดการกับเคสที่ไม่มีตัวเลข)
  year_str <- regmatches(date_string, regexpr("^[0-9]+", date_string))
  
  # ถ้าไม่เจอตัวเลขเลย (เช่น "Unknown") ให้กำหนดค่า default (เช่น ปีเก่ามากๆ)
  if (length(year_str) == 0) {
    
    # สำหรับ "Unknown (8000-1 BCE)" จะถูกจัดเป็น Neolithic_Early อยู่แล้ว
    # การให้ค่าเป็น -8000 จะช่วยให้เรียงลำดับได้ถูกต้อง
    if (grepl("Unknown", date_string)) {
      return(-8000)
    }
    return(NA) # หรือค่าอื่นที่เหมาะสม
  }
  
  year_val <- as.numeric(year_str)
  
  # ถ้าเป็น BCE ให้เป็นค่าลบ
  if (grepl("BCE", date_string, ignore.case = TRUE)) {
    return(-year_val)
  } else {
    return(year_val)
  }
}

# 1. กรองเฉพาะ Ancient และสร้างคอลัมน์ใหม่ ๆ เพื่อใช้จัดเรียง
# Sort order is chronological (Oldest to Newest): Jomon (~10000-300 BCE) -> Hoabinhian (~12000-2000 BCE)
# -> Neolithic (~2500-500 BCE) -> Bronze Age (~1500-500 BCE) -> Iron Age (~500 BCE - 500 CE) -> Historical (after ~500 CE)
ancient_only_sorted <- fam_merged_final %>%
  filter(SampleType == "Ancient") %>%
  
  # 1. นำข้อมูล missingness มารวมกับตารางนี้
  left_join(missingness_data, by = "Genetic ID") %>%
  
  # 2. สร้างคอลัมน์สำหรับจัดเรียงและสร้าง Label ใหม่
  mutate(
    SortGroup = case_when(
      `Genetic ID` == "Human1" ~ "01_Human1",
      `Genetic ID` == "Human2" ~ "02_Human2",
      grepl("Jomon", `Group ID`) ~ "03_Jomon",
      grepl("Hoabinhian", `Group ID`) ~ "04_Hoabinhian",
      grepl("Unknown", `Group ID`) ~ "05_Neolithic_Early",
      grepl("historical", `Group ID`) ~ "09_Historical",
      grepl("_IA_|_IA$|^IA_", `Group ID`) ~ "08_IronAge",
      grepl("_BA_|_BA$|^BA_|_DongSon", `Group ID`) ~ "07_BronzeAge",
      grepl("_LN_|_LN$|^LN_|_N_|_N$|^N_|_HaLong|RedSlipped", `Group ID`) ~ "06_Neolithic_Late",
      TRUE ~ "99_Other"
    ),
    
    StartDate = sapply(`ori full date`, get_start_year),
    
    # สร้าง PlotLabel ที่มีดีไซน์แบบ Professional
    # ใช้ case_when เพื่อจัดการทุกเงื่อนไขในที่เดียว
    PlotLabel = case_when(
      # เงื่อนไขที่ 1: ถ้าเป็น Human1 หรือ Human2
      `Genetic ID` %in% c("Human1", "Human2") ~ paste0(`Group ID`, " (", `ori full date`, " | ", F_MISS_percent, ")"),
      
      # เงื่อนไขที่ 2: ถ้าไม่ใช่ Human1/2 แต่ไม่มีข้อมูล missingness
      is.na(F_MISS_percent) ~ paste0(`Group ID`, " (", `ori full date`, ")"),
      
      # เงื่อนไขที่ 3 (TRUE): กรณีอื่นๆ ทั้งหมด (มีข้อมูล missingness)
      TRUE ~ paste0(`Group ID`, " (", `ori full date`, " | ", F_MISS_percent, ")")
    )
  ) %>%
  
  # 4. จัดเรียงและสร้าง SampleOrder (เหมือนเดิม)
  arrange(SortGroup, StartDate) %>%
  mutate(SampleOrder = row_number())


# พล็อต base R barplot แค่ ancient (new sort) -----------------------------

create_base_r_barplot_original <- function(k, file_prefix, df_meta, label_var, plot_title, cex_names, cex_legend) {
  
  q_file <- file.path(q_dir, sprintf("%s.%d.Q", file_prefix, k))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  colnames(mat) <- fam$`Genetic ID`
  
  ordered_ids <- df_meta$`Genetic ID`
  label_ids <- df_meta[[label_var]]
  
  mat_sorted <- mat[, ordered_ids, drop = FALSE]
  
  barplot_heights <- barplot(
    mat_sorted,
    col = ancestry_colors[1:k],
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = cex_names,
    main = plot_title,
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  legend(
    "right",
    inset = c(-0.15, 0),
    xpd = TRUE,
    legend = paste0("Ancestry ", 1:k),
    fill = ancestry_colors[1:k],
    cex = cex_legend,
    bty = "n"
  )
}

# กำหนด prefix ของไฟล์สำหรับชุดข้อมูลนี้
file_prefix_ancient <- "merge_0.05_human1-2_plink_allen_v62_with_SEA"

png(file.path("C:/Users/patch/Desktop/", "barK_all_ancient_sorted.png"),
    width = 2000,
    height = 900 * length(k_values),
    res = 300)
par(mfrow = c(length(k_values), 1), mar = c(11, 5, 4, 8))

# *** เรียกใช้ฟังก์ชันที่นิยามไว้แล้ว ภายใน for loop ***
for (k in k_values) {
  create_base_r_barplot_original(
    k = k,
    file_prefix = file_prefix_ancient,
    df_meta = ancient_only_sorted,
    label_var = "PlotLabel",
    plot_title = paste("Ancient sample plotting for K =", k, "(Cultural/Chronological Sort)"),
    cex_names = 0.4,
    cex_legend = 0.8
  )
}
dev.off()
cat("Your plot with all K values (ancient sorted) is ready on the Desktop! 🎉\n")


# พล็อต base R barplot ด้วย custom k แค่ ancient (new sort) --------

k_new <- 5

png(file.path("C:/Users/patch/Desktop/", paste0("barK", k_new, "_ancient_sorted.png")),
    width = 3000,  # คุณอาจจะอยากให้กราฟเดี่ยวกว้างขึ้น
    height = 1500, # และสูงขึ้น
    res = 300)

# ตั้งค่าให้มีแค่ 1 กราฟต่อ 1 รูป
par(mfrow = c(1, 1), mar = c(11, 5, 4, 8))

# 3. เรียกใช้ฟังก์ชันเดิมเป๊ะๆ แค่เปลี่ยนค่า k
create_base_r_barplot_original(
  k = k_new, # <--- ใช้ค่า k_new ที่กำหนดไว้
  file_prefix = file_prefix_ancient, # ใช้ prefix เดิม
  df_meta = ancient_only_sorted,
  label_var = "PlotLabel",
  plot_title = paste("Ancient sample plotting for K =", k_new, "(Cultural/Chronological Sort)"),
  cex_names = 0.4, # อาจจะปรับขนาดตัวอักษรตามความเหมาะสม
  cex_legend = 0.8
)

# 4. ปิดการเขียนไฟล์
dev.off()
cat(paste("Your custom plot for K=", k_new, " (ancient sorted) is ready on the Desktop! 🎉\n"))
