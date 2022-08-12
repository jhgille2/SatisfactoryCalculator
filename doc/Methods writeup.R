---
title: "Designing Satisfactory factories with linear programming"
author: "Jay Gillenwater"
date: "2022-08-12"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

Satisfactory is a game where you process raw resources into increasingly complicated refined items. As the game progresses, the manner in which these raw resources are processed and combined becomes more complicated and introduces some problems that are difficult to tackle by hand or with mental math alone (at least for me). This is especially true in the case that often arises where a resource is required for several items at the same time that are all necessary to procude in order to advance in the game. Linear programming offers a easy solution to many of these problems and has been effectively used by many popular calculators for the game which help with factory design. 

In this document, I want to describe the specific problems I am trying to solve with this repository and keep track of how I have attempted to solve them. 

## The basic problem

Raw resoruces in satisfactory come from a finite number of raw resource **nodes**. Each of these nodes produce an infinite amount of some raw resource at a set rate that depends on the building that is placed on the node, and any upgrades that have been made to that building. 

Progress is made in the game by completing **milestones** which each require some amount of a product to be made. These products are made by transforming the raw resource into intermediary products, and combining or further transforming these intermediaries to produce the final product. The way in which these final products is produced from the raw resources is called a **recipe**. For an example, consider the early game recipes for iron plates and iron rods:

```{r, Iron_Diagram, echo = FALSE}


```
