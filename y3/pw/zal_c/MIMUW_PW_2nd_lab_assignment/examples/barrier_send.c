/*
The purpose of this example is to test whether MIMPI_Recv can swap
messages delivered in reversed order.
Refers to <usprawnienie2> TODO: przenazwać to.
*/

#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include "../mimpi.h"
#include "mimpi_err.h"
#include <unistd.h>

int main(int argc, char **argv)
{
    MIMPI_Init(false);

    // Find out rank, size
    int const world_rank = MIMPI_World_rank();
    int const world_size = MIMPI_World_size();

    int const tag = 17;

    int const reader_rank = world_size - 1;
    int d = 0;
    for(int turn = 0; turn < 10; ++turn) {
        for(int i = 0; i < world_size; i++) {
            for(int j = 0; j < world_size; j++) {
                if(i == j) {
                    continue;
                }
                if(world_rank == i) {
                    ASSERT_MIMPI_OK(MIMPI_Send(&d, 1, j, tag));
                    printf("proc %d sent to %d\n", world_rank, j);
                }
            }
        }
    }
        // sleep(20);
    printf("proc %d waiting for barrier\n", world_rank);
    ASSERT_MIMPI_OK( MIMPI_Barrier() );

    MIMPI_Finalize();
    return 0;
}
