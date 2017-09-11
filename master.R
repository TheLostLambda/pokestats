library("ggplot2")
library("reshape2")

pokemon <- read.csv("assets/pokedata.csv", na.strings = "")
palette <- c("#A6B91A", "#705746", "#6F35FC", "#F7D02C", "#D685AD", "#C22E28", "#EE8130", "#A98FF3", "#735797", "#7AC74C", "#E2BF65", "#96D9D6", "#A8A77A", "#A33EA1", "#F95587", "#B6A136", "#B7B7CE", "#6390F0")

legendaries <- subset(pokemon, isLegendary == "True")
pokemelt <- na.omit(melt(legendaries, id.vars = "Name", measure.vars = c("Type_1", "Type_2")))

bar <- ggplot(pokemelt, aes(x = value, fill = value))
bar <- bar + geom_bar() + scale_fill_manual(values=palette[c(-1, -14)])
bar <- bar + labs(title = "Count and Types of Legendary Pokemon", x = "Type", y = "Number of Pokemon")
bar <- bar + guides(fill = FALSE)

#Curiously there are no legendary pokemon with a type of Poison, or Bug. Only secondary for Fighting
ggsave(filename = "renders/Legendary Bar.png", plot=bar, width = 10)

pokemelt <- na.omit(melt(pokemon, id.vars = "Total", measure.vars = c("Type_1", "Type_2")))

hist <- ggplot(pokemelt, aes(x = Total, fill = value))
hist <- hist + geom_histogram(binwidth = 25) + scale_fill_manual(values=palette)
hist <- hist + labs(title = "Total Stat for All Pokemon Types", x = "Total Stat", y = "Number of Pokemon", fill = "Type")

ggsave(filename = "renders/Total Hist.png", plot=hist, width = 10)

pokeattr <- colnames(pokemon)[5:11]

for(attr in pokeattr) {
  pokemelt <- na.omit(melt(pokemon, id.vars = attr, measure.vars = c("Type_1", "Type_2")))

  box <- ggplot(pokemelt, aes_string(x = "value", y = attr, fill = "value"))
  box <- box + geom_boxplot() + scale_fill_manual(values=palette) + scale_x_discrete(name = "Type")
  box <- box + scale_y_continuous(name = paste(attr,"Stat"), breaks = seq(0, 1000, 25)) + guides(fill = FALSE)
  box <- box + ggtitle(paste("Pokemon", attr, "Stats by Type"))

  ggsave(filename = paste("renders/", attr, " Box.png", sep = ""), plot=box, width=10)
}

#Okay, you have picked a favorite type, so how common is it?
pokemelt <- na.omit(melt(pokemon, id.var = "Name", measure.vars = c("Type_1", "Type_2")))

nrows <- 10
grid <- expand.grid(y = 1:nrows, x = 1:nrows)
counts <- rle(sort(pokemelt$value))
grid$type <- rep(counts[[2]], round(nrows^2 * (counts[[1]] / sum(counts[[1]]))))
waffle <- ggplot(grid, aes(x = x, y = y, fill = type))
waffle <- waffle + geom_tile(color = "white", size = 1) + scale_fill_manual(values = palette)
waffle <- waffle + labs(title = "Frequencies of Pokemon Types", fill = "Type")
waffle <- waffle + scale_y_continuous(trans = 'reverse') + coord_equal() + theme_void()

ggsave(filename = "renders/Type Count.png", plot = waffle)
