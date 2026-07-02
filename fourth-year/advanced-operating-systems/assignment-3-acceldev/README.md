## Implementation 
This implementation is not complete and has a few significant problems when it comes to locking. I'm pretty sure there are *many* possible cases of race conditions, and if I were smart enough to start working on this assignment earlier I'm sure they would've been fixed. 

I used an additional worker thread to submit all tasks to the feed, however I don't include additional fence commands to be able to properly release the buffers (the assignment definition mentions that the buffer may be released only after all tasks on the ctx up to this point are completed).
Any error inside the device results in it's restart and all contexts being set to *failed*. This fails one test, but I believe the concept of ctx configs being read-only doesn't allow one to properly reset a single context when it has failed, so this was the most logical solution at first, and now i simply don't have the time to fix it. 
Apart from that, the driver seems to be working, although I have no idea if the probe/release functions install it properly. 3 tests are failing, but the rest seems to be ok.
## Tests 
there's a tests folder with ./run.sh which excludes the few tests that don't pass.
## Make
The makefile is nearly identical to the one from the labs:
```bash
make && make install && reboot
```
```
```
