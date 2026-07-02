long long buf[13];
long long size = 13;

int check(void) {
  long long i = 0;
  long long tmp1 = 0;
  long long tmp2 = 0;
  while (i < size) {
    long long offset = i;
    tmp1 = offset;
    tmp2 = tmp1 + offset;
    offset = tmp2;
    tmp1 = offset;
    tmp2 = tmp1 + offset;
    offset = tmp2;
    tmp1 = offset;
    tmp2 = tmp1 + offset;
    offset = tmp2;

    long long *buf_tmp = buf;
    long long *elem = (long long *)(((char *)buf_tmp) + offset);
    if (*elem != i)
      return -1;
    tmp1 = i + 1;
    i = tmp1;
  }
  return 0;
}
