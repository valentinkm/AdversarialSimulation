# Variables
IMAGE_NAME := simulation-image
CONTAINER_NAME := simulation-container
USER_ID := $(shell id -u)
GROUP_ID := $(shell id -g)
PWD := $(shell pwd)

# Adjust GROUP_ID for macOS if it's less than 1000 (common on macOS systems)
ifeq ($(shell uname), Darwin)
    ifeq ($(shell [ $(GROUP_ID) -lt 1000 ] && echo yes), yes)
        OVERRIDE_GROUP_ID := $(shell expr $(GROUP_ID) + 10000)
    else
        OVERRIDE_GROUP_ID := $(GROUP_ID)
    endif
else
    OVERRIDE_GROUP_ID := $(GROUP_ID)
endif

# Default target
.PHONY: all
all: build run

# Build the Docker image
.PHONY: build
build:
	docker build \
		--build-arg USER_ID=$(USER_ID) \
		--build-arg GROUP_ID=$(OVERRIDE_GROUP_ID) \
		-t $(IMAGE_NAME) .

# Prepare results directory
.PHONY: prepare
prepare:
	mkdir -p $(PWD)/results_replic
	chmod 777 $(PWD)/results_replic # Ensure it's writable by any container user

# Run the Docker container
.PHONY: run
run: prepare
	docker run --rm \
		--name $(CONTAINER_NAME) \
		--user $(USER_ID):$(OVERRIDE_GROUP_ID) \
		-v $(PWD)/results_replic:/simulation/results_replic \
		--env TMPDIR=/simulation/results_replic \
		$(IMAGE_NAME)

# Clean up Docker images
.PHONY: clean
clean:
	docker rmi -f $(IMAGE_NAME)