include ./make/project.mk

shell: apps
	cd lib/ech && make shell
