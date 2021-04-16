# formula-example
Example repo for "Deriving your Frontend" tutorial

## Running Locally

1. `sbt ~fastLinkJS` in another tab.
2. `yarn install`
3. `yarn exec vite`
4. open `http://localhost:3000`

## Weird Errata
If you get a message surrounding unescaped strings from vite - try deleting the line with %main% in the index.html file. Run vite - it should work. Then put the line back. Seems to be a one time only thing.
