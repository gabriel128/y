.PHONY: test

docker-build-x86_64:
	cd x86_64; docker build -t y-x86_64 .

docker-x86_64:
	cd x86_64; ./start_docker.sh

test:
	stack test --file-watch --fast 

test-specific:
	stack test --file-watch --fast --ta "-p /liveness for ex1/
