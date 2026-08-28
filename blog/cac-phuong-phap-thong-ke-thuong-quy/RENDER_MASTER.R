library(quarto) 
# quarto:::quarto_render("index.qmd")
# quarto:::quarto_render("about.qmd")
# render lại lần lượt các file để đảm bảo mọi thứ working good.

system('E:/GITHUB/thongkevavedothi/blog/cac-phuong-phap-thong-ke-thuong-quy/export.cmd')
system('E:/GITHUB/thongkevavedothi/blog/cac-phuong-phap-thong-ke-thuong-quy/export_png.cmd')

list.files("./",
           recursive = T,
           pattern = "\\.qmd",
           full.names = T) -> render_qmd

for(i in 1:length(render_qmd)){
  
  quarto:::quarto_render(render_qmd[i])
  
}

list.files("./",
           recursive = T,
           pattern = "\\.knit",
           full.names = T) -> render_rm

file.remove(render_rm)

# list.files("posts/",
#            recursive = T,
#            pattern = "render.R",
#            full.names = T) -> render_lock
# 
# 
# for(j in 1:length(render_lock)){
#   
#   source(render_lock[j])
#   
# }
