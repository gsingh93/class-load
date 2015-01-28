UM Class Load Calculator
========================

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/gsingh93/class-load?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A website for calculating class load for EECS classes at the University of Michigan. Backend written in Haskell with the [Yesod Web Framework](http://www.yesodweb.com/). Front-end written with [Elm](http://elm-lang.org/). The main files to look at if you would like to contribute are `Handler/Home.hs` and `elm/index.elm`. Feel free to discuss the project or ask questions in the Gitter chat. Pull requests are welcome.

To run the site locally, [install Yesod](http://www.yesodweb.com/page/quickstart) and run `yesod devel` in the main directory. Then navigate to `localhost:3000`.

The current build can be found here: https://course-load.herokuapp.com/. As this project is still under development, the site can't be guaranteed to always be up. If there are any problems, please file an issue.
