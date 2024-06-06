#########################
# Analisis exploratorio #
#########################

# Frecuencias de las clases
png(paste0(rute_img, "HTRU2_freq.png"), width = 1600, height = 1600)
print(ggplot(data_htru2, aes(x=Class, fill=Class )) +  
        geom_bar( ) +
        xlab("Class") +
        ylab("Frecuencia") +
        scale_fill_manual(values = c("blue", "red"), name = "Class", labels = c("No es pulsar", "Es pulsar") ) +
        theme(legend.position = "right"))
dev.off()

# Graficamos por pares
png(paste0(rute_img, "HTRU2_pairplots.png"), width = 1600, height = 1600)
pairs.panels(data_htru2[, 1:8],
             ellipses = FALSE,
             smooth = FALSE,
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             bg = colors,
             pch = 21
)
dev.off()

# Histogramas: comparacion visual de distribuciones
histogramas<-list()
for (i in 1:8){
  x = data_htru2[,i]
  histogramas[[i]] <- ggplot(data.frame(x),
                             aes(x = x, fill = colors, colour = colors)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    guides(fill = guide_legend(title = "Pulsar"), colour = guide_legend(title = "Pulsar")) +
    scale_fill_manual(values=c("blue", "red"), labels = c("0", "1")) + scale_color_manual(values=c("blue", "red"), labels = c("0", "1")) +
    ggtitle(colnames(data_htru2[i])) +
    xlab(colnames(data_htru2[i])) +
    ylab("Frecuencia")
}

png(paste0(rute_img, "HTRU2_histogramas.png"), width = 1600, height = 1600)
grid.arrange(histogramas[[1]], histogramas[[2]], histogramas[[3]], histogramas[[4]],
             histogramas[[5]], histogramas[[6]], histogramas[[7]], histogramas[[8]],
             ncol=3)
dev.off()

# Graficas de caja: deteccion de outliers
gcajas<-list()
for (i in 1:8){
  x = data_htru2[,i]
  gcajas[[i]] <- ggplot(data.frame(x),
                        aes(y = x, x = data_htru2[,9], fill = colors, colour = colors)) +
    geom_boxplot(alpha = 0.5, position = "identity") +
    scale_fill_manual(values=c("blue", "red"), labels = c("0", "1")) + scale_color_manual(values=c("blue", "red"), labels = c("0", "1")) +
    ggtitle(colnames(data_htru2[i])) +
    ylab(colnames(data_htru2[i])) +
    xlab('¿Es pulsar?')
}

png(paste0(rute_img, "HTRU2_gcajas.png"), width = 1600, height = 1600)
grid.arrange(gcajas[[1]], gcajas[[2]], gcajas[[3]], gcajas[[4]],
             gcajas[[5]], gcajas[[6]], gcajas[[7]], gcajas[[8]],
             ncol=3)
dev.off()

# Prueba de correlacion de Spearman: correlacion no lineal
sp_13 <- cor.test(data_htru2[ , 1], data_htru2[ , 3],  method = "spearman")
sp_34 <- cor.test(data_htru2[ , 3], data_htru2[ , 4],  method = "spearman")
sp_56 <- cor.test(data_htru2[ , 5], data_htru2[ , 6],  method = "spearman")
sp_57 <- cor.test(data_htru2[ , 5], data_htru2[ , 7],  method = "spearman")
sp_78 <- cor.test(data_htru2[ , 7], data_htru2[ , 8],  method = "spearman")

