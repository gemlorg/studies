#ifndef COMMON_HPP
#define COMMON_HPP
#include <fcntl.h>  
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <signal.h>
#include "err.h"
#include <string>
#include <iostream>
#define htonll(x) ((((uint64_t)htonl(x)) << 32) + htonl((x) >> 32))
#define ntohll(x) ((((uint64_t)ntohl(x)) << 32) + ntohl((x) >> 32))

namespace common{

using byte_t = uint8_t;
template <typename T>
  ssize_t read_message(int socket_fd, struct sockaddr_in* client_address,
                    T* buffer, size_t max_length) {
  socklen_t address_length = (socklen_t)sizeof(*client_address);
  int flags = 0;
  errno = 0;
  ssize_t len = recvfrom(socket_fd, buffer, max_length, flags,
                         (struct sockaddr*)client_address, &address_length);
  /* if (len < 0) { */
  /*   PRINT_ERRNO(); */
  /* } */
  return len;
}


void unblock_socket(int socket_fd) {
  int flags = fcntl(socket_fd, F_GETFL, 0);
  fcntl(socket_fd, F_SETFL, flags | O_NONBLOCK);
}

void disable_telnet_buffering(int fd) {
    send(fd, "\xff\xfd\x22\xff\xfa\x22\x01\x00\xff\xf0", 10, 0);
}

uint64_t byte_to_uint(byte_t * arr) {
  uint64_t n;
  memcpy(&n, arr, sizeof(n));
  return ntohll(n);
}

template<typename T>
void send_message(int socket_fd, const struct sockaddr_in* send_address,
                  uint64_t message_length, const T* message) {
  int send_flags = 0;
  socklen_t address_length = (socklen_t)sizeof(*send_address);
  errno = 0;
  ssize_t sent_length = sendto(socket_fd, message, message_length, send_flags,
                               (struct sockaddr*)send_address, address_length);

  if (sent_length < 0) {
    PRINT_ERRNO();
  }

  ENSURE(sent_length == (ssize_t)message_length);
}

void broadcast_socket(int sock) {
  int optval = 1;
  setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (void*)&optval, sizeof (optval));

}

void reuse_port(int socket_fd) {
    int optval = 1;
    setsockopt(socket_fd, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval));
}

void reuse_adress(int socket_fd) {
    int optval = 1;
    setsockopt(socket_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval));
}


int bind_socket(uint16_t port, std::string ip_addr) {
  int socket_fd = socket(AF_INET, SOCK_DGRAM, 0);  // creating IPv4 UDP socket
  reuse_port(socket_fd);
  reuse_adress(socket_fd);
  unblock_socket(socket_fd);
  ENSURE(socket_fd >= 0);
  struct sockaddr_in server_address;
  server_address.sin_family = AF_INET;  // IPv4
  server_address.sin_addr.s_addr = std::string("INADDR_ANY") == ip_addr ? htonl(INADDR_ANY) : inet_addr(ip_addr.c_str());  // listening on requested interfaces
  server_address.sin_port = htons(port);
  CHECK_ERRNO(bind(socket_fd, (struct sockaddr*)&server_address,
                   (socklen_t)sizeof(server_address)));

  return socket_fd;
}

int bind_mulitcast_listen_socket(uint16_t port, std::string ip_addr) {
  std::cerr << "connecting to " << ip_addr << " " << port << '\n';
  int socket_fd = socket(AF_INET, SOCK_DGRAM, 0);  // creating IPv4 UDP socket
  reuse_port(socket_fd);
  reuse_adress(socket_fd);

  unblock_socket(socket_fd);
  ENSURE(socket_fd >= 0);
  struct sockaddr_in server_address;
  server_address.sin_family = AF_INET;  // IPv4
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);  // listening on requested interfaces
  server_address.sin_port = htons(port);
  CHECK_ERRNO(bind(socket_fd, (struct sockaddr*)&server_address,
                   (socklen_t)sizeof(server_address)));

  /* struct ip_mreq mreq{}; */
  /* mreq.imr_multiaddr.s_addr = inet_addr(ip_addr.c_str()); */
  /* mreq.imr_interface.s_addr = htonl(INADDR_ANY); */
  /* CHECK_ERRNO(setsockopt(socket_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *)&mreq, sizeof(mreq))); */
  /**/
struct ip_mreq ip_mreq;
ip_mreq.imr_interface.s_addr =  htonl(INADDR_ANY);
ip_mreq.imr_multiaddr.s_addr =  inet_addr(ip_addr.c_str());

setsockopt(socket_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void*)&ip_mreq, sizeof(ip_mreq)); 


  return socket_fd;
}


uint64_t current_time_ms() {
  return duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}

struct sockaddr_in get_send_address(const char* host, uint16_t port) {
  struct addrinfo hints;
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_INET;  // IPv4
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_protocol = IPPROTO_UDP;
  struct addrinfo* address_result;
  CHECK(getaddrinfo(host, NULL, &hints, &address_result));
  struct sockaddr_in send_address;
  send_address.sin_family = AF_INET;
  send_address.sin_addr.s_addr =
      ((struct sockaddr_in*)(address_result->ai_addr))->sin_addr.s_addr;

  send_address.sin_port = htons(port);
  freeaddrinfo(address_result);
  return send_address;
}



}
#endif
