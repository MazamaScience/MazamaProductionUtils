BASE_PATH:=$(realpath ..)

clean:
	rm -rf logs output ../R ../data ../pptx

create_dirs:
	- mkdir logs
	- mkdir output

cpu_hogs: create_dirs
	docker run production-utils cpuHogs_exec.R
