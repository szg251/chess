build:
	elm make ./src/Main.elm --output=./dist/index.html

dev:
	elm-live ./src/Main.elm --start-page=./tmp/index.html -- --output=./tmp/index.html --debug