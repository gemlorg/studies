## Zadanie

Wykonaj implementacje kilku kolejek nieblokujących z wieloma czytelnikami i pisarzami: `SimpleQueue`, `RingsQueue`, `LLQueue`, `BLQueue`. Dwie implementacje używają zwykłych mutexów, a dwie kolejne atomowych operacji, w tym `compare_exchange`.

### Implementacja kolejek

Każda implementacja składa się z struktury `<kolejka>` oraz metod:

- `<kolejka>* <kolejka>_new(void)` - alokuje (`malloc`) i inicjalizuje nową kolejkę.
- `void <kolejka>_delete(<kolejka>* queue)` - zwalnia wszelką pamięć zaalokowaną przez metody kolejki.
- `void <kolejka>_push(<kolejka>* queue, Value value)` - dodaje wartość na koniec kolejki.
- `Value <kolejka>_pop(<kolejka>* queue)` - pobiera wartość z początku kolejki albo zwraca `EMPTY_VALUE` jeśli kolejka jest pusta.
- `bool <kolejka>_is_empty(<kolejka>* queue)` - sprawdza, czy kolejka jest pusta.

Przykładowo pierwsza implementacja powinna definiować strukturę `SimpleQueue` oraz metody `SimpleQueue* SimpleQueue_new(void)` itp.

### Typ Value

Wartości w kolejce mają typ `Value` równy `int64_t` (dla wygodnego testowania, normalnie trzymalibyśmy tam raczej `void*`). Użytkownicy kolejki nie używają wartości `EMPTY_VALUE=0` ani `TAKEN_VALUE=-1` (można je wykorzystać jako specjalne). Użytkownicy kolejki gwarantują, że wykonują `new/delete` dokładnie raz, odpowiednio przed/po wszystkich operacjach `push/pop/is_empty` ze wszystkich wątków.

### Wymagania implementacyjne

Wszystkie implementacje powinny zachowywać się tak, jakby operacje `push`, `pop`, `is_empty` były niepodzielne (patrząc na wartości zwracane użytkownikom kolejki). Używanie kolejki nie może prowadzić do wycieków pamięci ani zakleszczeń. Implementacje nie muszą w pełni gwarantować sprawiedliwości i braku zagłodzenia każdego wątku z osobna.

### Lock-free Implementacje

Implementacje kolejek lock-free (`LLQueue` i `BLQueue`) powinny gwarantować, że w każdym równoległym wykonaniu operacji:

- Co najmniej jedna operacja `push` zakończy się w skończonej liczbie kroków.
- Co najmniej jedna operacja `pop` zakończy się w skończonej liczbie kroków.
- Co najmniej jedna operacja `is_empty` zakończy się w skończonej liczbie kroków, jeśli takie operacje się zaczęły i nie zostały wstrzymane.

### Wydajność

Rozwiązania będą sprawdzane także pod kątem wydajności (ale odchylenia rzędu 10% od wzorcowego rozwiązania nie wpłyną na ocenę).

## Implementacje

### SimpleQueue

Kolejka z listy jednokierunkowej, z dwoma mutexami. Struktura `SimpleQueue` składa się z:

- Listy jednokierunkowej węzłów, gdzie węzeł zawiera:
  - Atomowy wskaźnik `next` na następny węzeł w liście.
  - Wartość typu `Value`.
- Wskaźnika `head` na pierwszy węzeł w liście, wraz z mutexem do zabezpieczania dostępu do niego.
- Wskaźnika `tail` na ostatni węzeł w liście, wraz z mutexem do zabezpieczania dostępu do niego.

### RingsQueue

Kolejka z listy buforów cyklicznych, łącząca nieograniczony rozmiar listy jednokierunkowej z wydajnością bufora cyklicznego. Struktura `RingsQueue` składa się z:

- Listy jednokierunkowej węzłów, gdzie węzeł zawiera:
  - Atomowy wskaźnik `next` na następny węzeł w liście.
  - Bufor cykliczny w postaci tabeli `RING_SIZE` wartości typu `Value`.
  - Atomowy licznik `push_idx` wykonanych w tym węźle push-ów.
  - Atomowy licznik `pop_idx` wykonanych w tym węźle pop-ów.
- Wskaźnika `head` na pierwszy węzeł w liście.
- Wskaźnika `tail` na ostatni węzeł w liście.
- Mutex `pop_mtx` do zamykania na czas całej operacji `pop`.
- Mutex `push_mtx` do zamykania na czas całej operacji `push`.

### LLQueue

Kolejka lock-free z listy jednokierunkowej. Struktura `LLQueue` składa się z:

- Listy jednokierunkowej węzłów, gdzie węzeł zawiera:
  - Atomowy wskaźnik `next` na następny węzeł w liście.
  - Wartość typu `Value`, równa `EMPTY_VALUE` jeśli wartość z węzła została już pobrana.
- Atomowego wskaźnika `head` na pierwszy węzeł w liście.
- Atomowego wskaźnika `tail` na ostatni węzeł w liście.
- Struktury `HazardPointer`.

### BLQueue

Kolejka lock-free z listy buforów. Struktura `BLQueue` składa się z:

- Listy jednokierunkowej węzłów, gdzie węzeł zawiera:
  - Atomowy wskaźnik `next` na następny węzeł w liście.
  - Bufor z `BUFFER_SIZE` atomowych wartości typu `Value`.
  - Atomowy indeks `push_idx` następnego miejsca w buforze do wypełnienia przez `push`.
  - Atomowy indeks `pop_idx` następnego miejsca w buforze do opróżnienia przez `pop`.
- Atomowego wskaźnika `head` na pierwszy węzeł w liście.
- Atomowego wskaźnika `tail` na ostatni węzeł w liście.
- Struktury `HazardPointer`.

## Hazard Pointer

Hazard Pointer to technika do radzenia sobie z problemem bezpiecznego zwalniania pamięci w strukturach danych współdzielonych przez wiele wątków oraz z problemem ABA. Struktura `HazardPointer` powinna składać się z:

- Tablicy zawierającej atomowy wskaźnik dla każdego wątku – zawierający "zarezerwowany" adres węzła przez dany wątek.
- Tablicy zbiorów wskaźników dla każdego wątku – adresy "wycofane" (`retired`), czyli do późniejszego zwolnienia.

### Metody HazardPointer

- `void HazardPointer_register(int thread_id, int num_threads)` – rejestruje wątek o identyfikatorze `thread_id`.
- `void HazardPointer_initialize(HazardPointer* hp)` – inicjalizuje (zaalokowaną już) strukturę: zarezerwowane adresy są wszystkie NULL.
- `void HazardPointer_finalize(HazardPointer* hp)` – czyści wszelkie rezerwacje, zwalnia pamięć zaalokowaną przez metody struktury oraz zwalnia wszystkie adresy z tablicy wycofanych (nie zwalnia samej struktury `HazardPointer`).
- `void* HazardPointer_protect(HazardPointer* hp, const AtomicPtr* atom)` – zapisuje w tablicy zarezerwowanych adresów adres odczytany z `atom` pod indeksem `thread_id` i zwraca go (nadpisuje istniejącą rezerwację, jeśli taka była dla `thread_id`).
- `void HazardPointer_clear(HazardPointer* hp)` – usuwa rezerwację, tzn. zamienia adres pod indeksem `thread_id` na NULL.
- `void HazardPointer_retire(HazardPointer* hp, void* ptr)` – dodaje `ptr` do zbioru adresów wycofanych, za których zwolnienie odpowiedzialny jest wątek `thread_id`. Następnie, jeśli rozmiar zbioru wycofanych przekracza próg zdefiniowany stałą `RETIRED_THRESHOLD` (np. `MAX_THREADS`), to przegląda wszystkie adresy w swoim zbiorze i zwalnia (`free()`) te, które nie są zarezerwowane przez żaden wątek (usuwając je też ze zbioru).

Użytkownicy kolejek używających `HazardPointer` gwarantują, że każdy wątek wywoła `HazardPointer_register` z unikalnym `thread_id` (liczba całkowita z przedziału `[0, num_threads)
