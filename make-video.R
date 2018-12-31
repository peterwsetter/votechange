#' Copyright Peter W Setter 2018
#' Create video of election analysis

library(ari)

ari_narrate(script = 'video-presentation.Rmd',
            slides = 'video-presentation.html',
            output = 'data-products/democrats-won-with-2016-vote-totals.mp4',
            voice = 'Matthew',
            capture_method = 'iterative'
            )
