#library(flfishltm)
library(ggplot2)
library(hexSticker)
library(magick)

sysfonts::font_add_google("Righteous")
showtext::showtext_auto()

plt <- cpue.plot(newn_sum,
                 speciesList = "BLUE")
plt <- len.dist(newn_sum,
                speciesList = "BLUE",
                year = 2009)
axcol = "#0e2a30"
p <- ggplot(data = plt, aes(x = TL_CM_Group, y = percent)) +
  geom_bar(stat = "identity", just = 0, width=1, fill = axcol, color = axcol) +
  coord_cartesian(expand = FALSE) +
  theme_classic() +
  labs(x = "Length", y = "Frequency") +
  theme_transparent() +
  theme(axis.line = element_line(color = axcol),
        axis.ticks = element_line(color = axcol),
        axis.text = element_text(color = axcol),
        axis.title = element_text(color = axcol))
print(p)

sub_wid = 1.5
sub_ht = sub_wid*2/3
sticker(p,
        package="flfishltm",
        s_width = sub_wid,
        p_family = "Righteous",
        p_size = 24,
        p_color = "#006a4e",
        s_x = 0.975,
        s_y = 0.725,
        s_height = sub_ht,
        h_fill = "#addfad",
        h_color = "#0e2a30",
        spotlight = F,
        l_y = 1.5,
        l_width = 5,
        white_around_sticker = FALSE,
        dpi = 300,
        filename="man/figures/logo.png")

# magick::image_read("man/figures/logo.png")

rstudioapi::restartSession()