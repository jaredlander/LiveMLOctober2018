library(useful)

boros <- tibble::tribble(
    ~ Boro, ~ Pop, ~ Size, ~ Random,
    'Manhattan', 1600000, 23, 7,
    'Brooklyn', 2600000, 78, 24,
    'Queens', 2330000, 104, pi,
    'Bronx', 1455000, 42, 21,
    'Staten Island', 475000, 60, 3
)

boros

y ~ x

build.x( ~ Pop, data=boros)
build.x( ~ Pop + Size, data=boros)
build.x( ~ Pop * Size, data=boros)
1600000*23
build.x( ~ Pop : Size, data=boros)
build.x( ~ Pop + Pop:Size, data=boros)
build.x( ~ Pop * Size * Random, data=boros)

build.x( ~ Pop + Size - 1, data=boros)

build.x( ~ Boro, data=boros)
build.x( ~ Boro, data=boros, contrasts=FALSE)

build.x( ~ Pop + Size + Boro - 1, data=boros, contrasts=FALSE)
build.x( ~ Pop + Size + Boro - 1, data=boros, contrasts=FALSE, sparse=TRUE)
