# Trello clone

### Demo clone of popular app, w/o final styling (yet)

Basic Elm project demonstrating Elm architecture, CSS styling with external file, component composition and HTML5 Drag&Drop API implementation.

Following libraries were used:

* norpan/elm-html5-drag-drop - Elm client for HTML5 Drag and Drop API,
* seanhess/elm-style - for styling, though 3rd party libs can be abandoned. Class attributes with outer CSS styling are fine

To see this app in action on your local machine just clone the repo and open index.html file, located at it's root, with your favourite browser. For this you don't need to install Elm.

If you want to play with code, first make sure you have Elm on your local machine. Shell will reply with man page, if you type `elm` in it, in case you have Elm installed. Otherwise, get it from official website http://elm-lang.org/. First run `elm-package install` in the root of project, so that Elm can install necessary packages from `elm-package.json`. Every time you make change to Main.elm file, make sure to recompile it with `elm-make Main.elm --output="main.js"`, so that index.html can pick up the latest updates and display things correctly.

### The Elm Architecture boilerplate

If you want to build something awesome from scratch, you can grab the skeleton boilerplate from this gist: https://gist.github.com/grigor-aramyan/b29a6272f5656d9ddc56f591a3e03063. It has the structure of all necessary elements, so you just need to fill in as you wish. Or just play with it to understand some fundamental concepts of it's workings.