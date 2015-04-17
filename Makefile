all: project
	corebuild project.native

project:
	corebuild project.native

clean:
	rm -rf _build *.native
