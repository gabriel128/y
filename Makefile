.PHONY: test

docker-build:
	docker build -t haskell-yacll .

docker-bash:
	docker run -it -w /home/stackage/yacll -v /Users/gabriel/dev/yacll:/home/stackage/yacll haskell-yacll

test:
	stack test --file-watch --fast 

# test-specific:
# stack test --file-watch --fast --ta "-p /liveness for ex1/
