extern int answer;
int* answer_ptr = &answer;

int set_answer(void) {
    *answer_ptr = 42;
    return 0;
}
