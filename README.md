# Clarion Date Time Converter

This is a simple little app to help you convert between

- Clarion date and time
- ISO 8601 date strings, and
- human readable date times.

Written in Elm, licensed under [BSD-3](LICENSE).

## Prerequisites

- elm and elm-format

## Build and deploy to GitHub Pages

Switch to the `gh-pages` branch and merge the current `main`, then

```shell
elm make src/Main.elm --output=main.js
```

commit the built file and push the branch.
