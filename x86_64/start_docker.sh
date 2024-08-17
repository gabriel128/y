set -e
set -x

name=y_x86_64

if [ "$( docker container inspect -f '{{.State.Running}}' "$name" )" = "true" ]; then
    docker exec -it $name /bin/bash
elif [ "$( docker container inspect -f '{{.State.Running}}' "$name" )" = "false" ]; then
    docker start $name
    docker exec -it $name /bin/bash
else
    docker run -d -w /home/stackage/y --name $name -v $HOME/dev/y:/home/stackage/y $name
    docker exec -it $name /bin/bash
fi

