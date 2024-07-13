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

int main(int argc, char **argv)
{
    MIMPI_Init(false);

    // Find out rank, size
    int const world_rank = MIMPI_World_rank();
    int const world_size = MIMPI_World_size();

    int const tag = 17;

    int const reader_rank = world_size - 1;
    for (int turn = 0; turn < world_size; ++turn)
    {
        // if(world_rank == 0)
        //     printf("turn %d\n", turn);
        /*
        We have (n-1) writers and 1 reader (process with the last rank).
        Everyone acts in turns.
        Writers, starting from the first one, send their ranks to reader.
        The reader reads and prints ranks of others, but in reverse order.
        */
        if (world_rank == turn)
        {

            if (world_rank != reader_rank)
            {
                // printf("Writer %d sends its rank to reader\n", world_rank);
                ASSERT_MIMPI_OK(MIMPI_Send(&world_rank, 1, reader_rank, tag));
                // printf("Writer %d sent its rank to reader\n", world_rank);

            }
            else
            {
                // printf("Reader receives ranks of writers\n");
                for (int src_rank = reader_rank - 1; src_rank >= 0; --src_rank)
                {
                    char number = 0;
                    ASSERT_MIMPI_OK(MIMPI_Recv(&number, 1, src_rank, tag));
                    printf("Reader received number %d from process with rank %d\n",
                           number, src_rank);
                }
            }
        }
        // printf("waiting proc %d\n", world_rank);
        // So that we really act in turns
        ASSERT_MIMPI_OK(MIMPI_Barrier());
    }

    MIMPI_Finalize();
    return 0;
}
