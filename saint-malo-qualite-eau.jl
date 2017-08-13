using DataFrames
df = readtable("mesures-de-la-qualite-de-lair.csv")
names(df)
describe(df)
summary(df)
using Gadfly
using Cairo
using Fontconfig
function sumstat(df, col)
             d = [mean(df[:,col]),
                  median(df[:,col]),
                  std(df[:,col])]
         return d
end
sumstat(df, :Ozone_O3_moyenne_annuelle_µg_m3)
sumstat(df, :Ozone_O3_maximum_horaire)
sumstat(df, :Dioxyde_d_azote_NO2_moyenne_annuelle_µg_m3)
sumstat(df, :Dioxyde_d_azote_NO2_maximum_horaire)

Gadfly.push_theme(:default)
p = plot(df,
         x = :Année,
         y = :Ozone_O3_moyenne_annuelle_µg_m3,
        Geom.point,
        Geom.line,
        Guide.title("Ozone (O3) - moyenne annuelle µg/m3 à Saint Malo"),
        Guide.xlabel("Année"),
        Guide.ylabel("Ozone"),
        Theme(background_color = "white",
                  major_label_font="CMU Serif",
                  major_label_font_size = 25pt,
                  minor_label_font_size = 20pt,
                  line_width = 5pt,
                  point_size = 7pt,
                  default_color=color("#973232")))
img = PNG("Ozone_O3_moyenne_annuelle_µg_m3.png", 16inch, 8inch)
draw(img, p)

p = plot(df,
         x = :Année,
         y = :Ozone_O3_maximum_horaire,
        Geom.point,
        Geom.line,
        Guide.title("Ozone (O3) - maximum horaire µg/m3 à Saint Malo"),
        Guide.xlabel("Année"),
        Guide.ylabel("Maximum horaire"),
        Theme(background_color = "white",
        major_label_font="CMU Serif",
        major_label_font_size = 25pt,
        minor_label_font_size = 20pt,
        line_width = 5pt,
        point_size = 7pt,
                  default_color=color("#1E5B5B")))
img = PNG("ozone_annuel_saint_malo.png", 16inch, 8inch)
draw(img, p)

p = plot(df,
         x = :Année,
         y = :Ozone_O3_jours_de_dépassement_du_seuil_d_information,
        Geom.bar,
        Guide.title("Ozone (O3) - jours de dépassement du seuil d'information"),
        Guide.xlabel("Année"),
        Guide.ylabel("Jours de dépassement dans l'année"),
        Theme(background_color = "white",
        major_label_font="CMU Serif",
        major_label_font_size = 25pt,
        minor_label_font_size = 20pt,
        line_width = 5pt,
        point_size = 7pt,
                  default_color=color("#6D8D2F")))
img = PNG("Ozone_O3_jours_de_dépassement_du_seuil_d_information.png", 16inch, 8inch)
draw(img, p)

p = plot(df,
         x = :Année,
         y = :Dioxyde_d_azote_NO2_moyenne_annuelle_µg_m3,
        Geom.point,
        Geom.line,
        Guide.title("Dioxyde d'azote (NO2) - moyenne annuelle µg/m3 à Saint Malo"),
        Guide.xlabel("Année"),
        Guide.ylabel("Dioxyde d'azote"),
        Theme(background_color = "white",
        major_label_font="CMU Serif",
        major_label_font_size = 25pt,
        minor_label_font_size = 20pt,
        line_width = 5pt,
        point_size = 7pt,
                  default_color=color("#287928")))
img = PNG("Dioxyde_d_azote_NO2_moyenne_annuelle_µg_m3.png", 16inch, 8inch)
draw(img, p)

p = plot(df,
         x = :Année,
         y = :Dioxyde_d_azote_NO2_maximum_horaire,
        Geom.point,
        Geom.line,
        Guide.title("Dioxyde d'azote (NO2) - maximum horaire à Saint Malo"),
        Guide.xlabel("Année"),
        Guide.ylabel("Maximum horaire"),
        Theme(background_color = "white",
        major_label_font="CMU Serif",
        major_label_font_size = 25pt,
        minor_label_font_size = 20pt,
        line_width = 5pt,
        point_size = 7pt,
                  default_color=color("#E18C8C")))
img = PNG("Dioxyde_d_azote_NO2_maximum_horaire.png", 16inch, 8inch)
draw(img, p)

p = plot(df,
         x = :Année,
         y = :Dioxyde_d_azote_NO2_jours_de_dépassement_du_seuil_d_information,
        Geom.bar,
        Guide.title("Dioxyde d'azote (NO2) - jours de dépassement du seuil d'information"),
        Guide.xlabel("Année"),
        Guide.ylabel("Jours de dépassement"),
        Theme(background_color = "white",
        major_label_font="CMU Serif",
        major_label_font_size = 25pt,
        minor_label_font_size = 20pt,
        line_width = 5pt,
        point_size = 7pt,

                  default_color=color("#548787")))
img = PNG("Dioxyde_d_azote_NO2_jours_de_dépassement_du_seuil_d_information.png", 8inch, 4inch)
draw(img, img)
