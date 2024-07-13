#ifdef __linux__
#include <malloc.h>
#endif
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <threads.h>

#include "HazardPointer.h"

thread_local int _thread_id = -1;
int _num_threads = -1;

//  rejestruje wątek o identyfikatorze thread_id.
void HazardPointer_register(int thread_id, int num_threads)
{
    
    _thread_id = thread_id;
    _num_threads = num_threads;
}

//inicjalizuje (zaalokowaną już) strukturę: zarezerwowane adresy są wszystkie NULL.
void HazardPointer_initialize(HazardPointer* hp)
{
    

    for (int i = 0; i < _num_threads; ++i) {
        atomic_init(&hp->pointer[i], NULL);
        for (int j = 0; j < RETIRED_THRESHOLD; ++j) {
            atomic_init(&hp->retired[i][j], NULL);
        }
    }

}
//czyście wszelkie rezerwacje, zwalnia pamięć zaalokowaną przez metody struktury oraz zwalnia wszystkie
//adresy z tablicy wycofanych (nie zwalnia samej struktury HazardPointer).
void HazardPointer_finalize(HazardPointer* hp)
{
    for (int i = 0; i < _num_threads; ++i) {
        atomic_store(&hp->pointer[i], NULL);
        for (int j = 0; j < RETIRED_THRESHOLD; ++j) {
            void* ptr = atomic_load(&hp->retired[i][j]);
            // if(ptr != NULL)
            free(ptr);
            atomic_store(&hp->retired[i][j], NULL);
        }
    }
}
 //zapisuje w tablicy zarezerwowanych adresów adres odczytany z atom pod 
 //indeksem thread_id i zwraca go (nadpisuje istniejącą rezerwację, jeśli taka była dla thread_id).

void* HazardPointer_protect(HazardPointer* hp, const _Atomic(void*)* atom)
{   while(1) {
    void* ptr = atomic_load(atom);
    atomic_store(&hp->pointer[_thread_id], ptr);
    if(atomic_load(atom) == ptr)
        return ptr;
    }
}
//usuwa rezerwację, tzn. zamienia adres pod indeksem thread_id na NULL.
void HazardPointer_clear(HazardPointer* hp)
{
    atomic_store(&hp->pointer[_thread_id], NULL);
}

//dodaje ptr do zbioru adresów wycofanych, za których zwolnienie odpowiedzialny jest wątek thread_id. 
//Następnie jeśli rozmiar zbióru wycofanych przekracza próg zdefiniowany stałą RETIRED_THRESHOLD (równą np. MAX_THREADS), 
//to przegląda wszystkie adresy w swoim zbiorze i zwalnia (free()) te, które nie są zarezerwowane przez żaden wątek (usuwając je też ze zbioru).

void HazardPointer_retire(HazardPointer* hp, void* ptr)
{
    while(1) {
        for (int i = 0; i < RETIRED_THRESHOLD; ++i) {
            if (atomic_load(&hp->retired[_thread_id][i]) == NULL) {
                atomic_store(&hp->retired[_thread_id][i], ptr);
                return;
            }
        }

        for (int i = 0; i < RETIRED_THRESHOLD; i++) 
        {
            bool is_free = true;
            void *other = atomic_load(&hp->retired[_thread_id][i]);
            for(int j = 0; j < _num_threads; j++)
            {
                if(atomic_load(&hp->pointer[j]) == other)
                {
                    is_free = false;
                    break;
                }
            }
            if(is_free)
            {
                if(other != NULL) {
                    free(other);
                    other = NULL;
                }
                atomic_store(&hp->retired[_thread_id][i], NULL);
            }
        }

    }
}
