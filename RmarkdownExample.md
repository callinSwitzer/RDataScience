---
title: "Untitled"
author: "Callin Switzer"
date: "10/30/2016"
output:
  md_document:
    variant: markdown_github
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r}

png("carplt.png")
plot(cars)
dev.off()
```

![carplot](https://github.com/callinSwitzer/RDataScience/blob/master/carplt.png)

