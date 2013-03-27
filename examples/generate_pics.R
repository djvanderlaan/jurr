
library(jurr)
library(RColorBrewer)

WIDTH  <- 500
HEIGHT <- 400
IMG    <- TRUE

# ================================= HCL_PALETTE ===============================
if (IMG) png("hcl_palette.png", width=WIDTH, height=HEIGHT)
hcl_palette(10, plot = TRUE)
if (IMG) dev.off()

# ================================= HISTPLOT2 =================================
n <- 1000
x <- rweibull(n, shape=0.8)
y <- rnorm(n)
h2 <- histw2(x, y, count_x=30, count_y=20)

if (IMG) png("histplot2_area.png", width=WIDTH, height=HEIGHT)
plot(h2, xlab="Draws from Weibull distribution", 
    ylab="Draws from normal distribution")
if (IMG) dev.off()

if (IMG) png("histplot2_colour.png", width=WIDTH, height=HEIGHT)
n <- 1000
plot(h2, type="color", xlab="Draws from Weibull distribution", 
    ylab="Draws from normal distribution", background_col="gray")
plot(h2, type="text", add=TRUE, background_col=NA, add_lines=FALSE)
if (IMG) dev.off()


# ================================= HISTPLOT ==================================
n <- 10000
x <- rweibull(n, shape=1.2)
y <- rnorm(n, 3, 1)
h1a <- histw(x, count=50)
h1b <- histw(y, count=50)

if (IMG) png("histplot.png", width=WIDTH, height=HEIGHT)
plot(h1a, col=add_alpha("steelblue", 0.6), border="black", xaxs='i')
plot(h1b, col=add_alpha("indianred", 0.6), border="black", add=TRUE)
legend("topright", legend=c("Weibull", "Normal"), 
    fill=c("steelblue", "indianred"), bty='n')
if (IMG) dev.off()

# ================================= LINESBY ===================================
col <- unique(ChickWeight[c("Chick", "Diet")])
col <- brewer.pal(length(unique(col$Diet)), "Dark2")[col$Diet]

if (IMG) png("linesby.png", width=WIDTH, height=HEIGHT)
plot(ChickWeight$Time, ChickWeight$weight, type='n', xlab="Time", ylab="Weight", 
    main="ChickWeight")
linesby(ChickWeight$Time, ChickWeight$weight, ChickWeight$Chick,
    col = add_alpha(col, 0.7))
legend("topleft", legend=1:4, col=brewer.pal(4, "Dark2"), lty=1, title="Diet", 
    bty='n')
if (IMG) dev.off()

