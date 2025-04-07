.PHONY: test

docker-build-86_64_cache:
	docker build --target dependencies --cache-from y/app-dependencies:latest -f ./x86_64/Dockerfile -t y/app-dependencies .
	docker push y/app-dependencies:latest

docker-build-x86_64:
	docker build --target build --cache-from y/app-dependencies:latest -t y_x86_64 -f ./x86_64/Dockerfile .

docker-x86_64:
	cd x86_64; ./start_docker.sh

test:
	stack test --file-watch --fast 

test-specific:
	stack test --file-watch --fast --ta '-p "liveness for ex1"'

compile-watch:
	stack build --file-watch --fast
