
all: github.deps flash


github.deps:
	bash libs.sh > github.deps

flash:
	ant -e -s vidclient/build.xml


# End
