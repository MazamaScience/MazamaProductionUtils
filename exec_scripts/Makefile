BASE_PATH:=$(realpath .)

# crontab version:
# docker run --rm -v $(BASE_PATH):/app -w /app docker run production-utils /app/cpuHogs_exec.R

cpu_hogs:
	docker run --rm -v $(BASE_PATH):/app \
	-w /app \
	docker run production-utils /app/cpuHogs_exec.R


