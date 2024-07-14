CC = g++
CPPFLAGS = -std=c++20 -Wall -O2  
LDFLAGS=  -lboost_program_options -lpthread

.PHONY: all sikradio-sender  sikradio-receiver clean

sikradio-sender: sikradio-sender.cpp sikradio-receiver
	$(CC) $(CPPFLAGS) sikradio-sender.cpp  -o sikradio-sender $(LDFLAGS)
sikradio-receiver: sikradio-receiver.cpp
	$(CC) $(CPPFLAGS) sikradio-receiver.cpp  -o sikradio-receiver $(LDFLAGS)




clean:
	rm  sikradio-sender sikradio-receiver
