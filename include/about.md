## Purpose

In my quest for interesting health data I stumbled upon the [National Immunization Survey—Child 2014](https://www.cdc.gov/nchs/nis/data_files.htm) raw data files made public by the [CDC](http://cdc.gov). Someone at the National Immunization Survey office has been nice enough to make an R document that builds a pretty good data file from a very scary flat file. I would not have been able to build the R data file without their instructions. 

Because there are more than 400 variables collected from the survey, getting an idea of what the data looks like and finding the variables of interest was proving difficult. I wished I had a quick way of visualizing some of the variables. Since I don't have access to any fancy software, I looked for ways of building the quick visualizer I wished I had. This app is the result of that attempt.

This application was built **exclusively** as an exercise to learn R and Shiny without any communication with those formally studying the immunization survey data.

## Challenges

The challenges in building the app were all tied to the quirks and limitations of Shiny and some of the other packages I decided to use. 

#### Shiny keeps removed input objects

I used Shiny's `insertUI()` and `removeUI()` functions to dynamically add input objects that would allow users to filter data on selected variables. According to Shiny, "the UI generated with `insertUI()` is persistent: once it's created, it stays there until removed by `removeUI()`."  However, `removeUI()` only removes the object from the dom not from  Shiny's `input` list. This is documented [here](https://github.com/rstudio/shiny/issues/2374) and [here](https://github.com/rstudio/shiny/issues/2439). The workarounds are very hacky, they depend on access to what I think are the "internals" of Shiny by using `.subset2()`. I opted for not messing with it, which means I end up with hundreds of unused `input` objects in a session. I was going to subset `names(input)` to create a list of inputs, but had to find another way since that list is useless as it contains objects already removed.

#### Limited DT Package selection options

The [DT package](https://rstudio.github.io/DT/) is full of features and I think it provides one of the best [options to dynamically interact with tables in Shiny](https://clarewest.github.io/blog/post/making-tables-shiny/). I'm using DT's [selection feature](https://datatables.net/extensions/select/) in my app and wanted to match the color of the selected row to my theme's primary color. There are two ways of accessing DT's selection options, one is through the  `selection` argument and the other by specifying `extension = "Select"`. You can only set the css class of the selected row if you use the extension. However, the DT package for R warns against using the extension: "Note that DT has its own selection implementation and doesn't use the Select extension because the latter doesn't support the server-side processing mode well." And it's true, I tried using the extension and things broke. 

I finally opted for using the [bslib package](https://rstudio.github.io/bslib/) and [Bootstrap's root css variables](https://getbootstrap.com/docs/5.0/customize/css-variables/) to match the selected row's color to the theme's primary color. All I had to do was to set `td.active {background-color: var(--bs-primary)!important;}` in my css settings. You can see the [solution with a working example StackOverflow](https://stackoverflow.com/questions/72368164/r-shiny-dt-datatable-how-can-i-change-the-selected-class/).  

