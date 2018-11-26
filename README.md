# About / Purpose

This DRA demonstrates how one would write a simple simple-page app using the Elm ecosystem.

Elm is a functional, strongly-typed, domain specific programming language focused on front-end development. It features a unique syntax that is similar to Haskell, and compiles to Javascript for deployment on the web. [Learn more](https://elm-lang.org/)

We are interested in trying out elm (as opposed to a JS framework) because it features an extremely robust architecture (no runtime exceptions in practice), high performance, as well as a core set of tools that are wonderful to use.

# App

This is a simple weather viewing app which determines a user's location by IP address and displays the current temperature and weather condition based on the locality. Users can also open a menu to select a different location (by ZIP code) to view far away weather.

In addition, the weather for the following 6 days are shown for the selected locality.


# Setup

To run this project on your local machine, you will need to follow a few quick steps. We use create-elm-app to simplify the process of compiling the source and running the dev server/hot module reloading.

Install some global modules:

```
npm install -g elm
npm install -g create-elm-app
```

Note: Some people have trouble installing elm via npm. You might need elevated permissions, or you might need to try installing from the [elm website](https://guide.elm-lang.org/install.html).

Point your terminal at the root level of this repository:

```
cd dra-web-elm
```

Then, get the project running via create-elm-app:

```
elm-app start
```

You should then be able to point your web browser at [localhost:3000](http://localhost:3000) to begin checking out the app.

# Caveats

+ Routing is a bit funky at the moment. Having trouble discerning between navigating to a route requiring a reload, and a route that's just part of the SPA. For now, use the following URL to see an example of routing: [http://localhost:3000/#/settings](http://localhost:3000/#/settings) 

+ You may notice that source files are long (several hundred lines). This is because Elm's creator has advised that for small project, you might not need to split files up as much as you do for Javascript, because of the clear flow provided by the elm architecture. See Evan Czaplicki's [famous video](https://www.youtube.com/watch?v=XpDsk374LDE) on the topic.

---

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

Below you will find some information on how to perform basic tasks.
You can find the most recent version of this guide [here](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md).

