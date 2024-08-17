set -e
set -x

name=y-x86_64

if [ "$( docker container inspect -f '{{.State.Running}}' "$name" )" = "true" ]; then
    docker exec -it $name /bin/bash
elif [ "$( docker container inspect -f '{{.State.Running}}' "$name" )" = "false" ]; then
    docker start $name
    docker exec -it $name /bin/bash
else
    docker run -d --name $name -v /Users/gabriel/dev/yacll:/home/stackage/yacll $name
    docker exec -it $name /bin/bash
fi
