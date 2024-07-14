#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <mutex>

#include <algorithm>
#include <boost/program_options.hpp>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <deque>
#include <iostream>
#include <istream>
#include <list>
#include <queue>
#include <regex>
#include <set>
#include <thread>
#include <vector>

#include "common.hpp"
#include "err.h"

#define MAX_UDP_LEN 1000

namespace po = boost::program_options;
using namespace common;
namespace {

class package_holder {
 public:
  uint64_t session_id;
  uint64_t first_byte_num;
  std::vector<byte_t> data;
};

class connection_manager;
using manager_t = class connection_manager;

using package_t = class package_holder;
using retransmission_set = std::set<uint64_t>;
using package_queue_t = std::list<byte_t*>;
void send_to_mcast(manager_t* manager, byte_t* package);

class connection_manager {
 public:
  std::string mcast_addr;
  uint16_t data_port;
  uint16_t ctrl_port;
  uint64_t psize;
  uint64_t fsize;
  std::string name;
  uint64_t start;
  uint64_t current;
  uint64_t rtime;
  int control_socket_fd;
  int mcast_socket_fd;
  int reply_socket_fd;
  package_queue_t* package_queue;
  retransmission_set* to_resend;
  char* buffer;
  std::mutex set_free;
  bool continue_work;

  void push_with_check(byte_t* p) {
    package_queue->push_back(p);
    if (package_queue->size() > fsize / (psize + 16)) {
      free(package_queue->front());
      package_queue->pop_front();
    }
  }

  void resend_package(uint64_t p) {
    while (!this->package_queue->empty() &&
           byte_to_uint(this->package_queue->front() + 8) < p) {
      this->package_queue->pop_front();
    }
    if (!this->package_queue->empty() &&
        byte_to_uint(this->package_queue->front() + 8) == p) {
      send_to_mcast(this, this->package_queue->front());
    }
  }
};

void pass_params_to_manager(manager_t* manager, int argc, char** argv) {

  boost::program_options::options_description desc(
      "\nMandatory arguments marked with '*'.\n"
      "Invocation : <program> -a <mcast_addr> -P <data_port> -C <ctrl_port> -p "
      "<psize> "
      "-f <fsize> -R <rtime> -n <name> \nAgruments");

  desc.add_options()("MCAST_ADDR,a", po::value<std::string>()->required(),
                     "* Receiver adress")(
      "DATA_PORT,P", po::value<uint64_t>()->default_value(26366),
      "UDP port used for data transfer")(
      "CTRL_PORT,C", po::value<uint64_t>()->default_value(36366),
      "UDP port for control package")("PSIZE,p",
                                      po::value<uint64_t>()->default_value(512),
                                      "Package size, in B")(
      "FSIZE,f", po::value<uint64_t>()->default_value(128),
      "Queue size, in kB")("RTIME,R", po::value<uint64_t>()->default_value(250),
                           "Package collection intervali, in ms")(
      "NAZWA,n", po::value<std::string>()->default_value("Nenazwany Nadajnik")

  );

  po::variables_map vm;
  try {
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
  } catch (po::error& e) {
    std::cout << "ERROR: " << e.what() << "\n";
    std::cout << desc << "\n";
    exit(1);
  }
  if (vm["DATA_PORT"].as<uint64_t>() > UINT16_MAX) {
    std::cout << "Invalid port number. \n";
    exit(1);
  }
  if (vm["CTRL_PORT"].as<uint64_t>() > UINT16_MAX) {
    std::cout << "Invalid port number. \n";
    exit(1);
  }

  manager->mcast_addr = (vm["MCAST_ADDR"].as<std::string>());
  manager->data_port = (uint16_t)vm["DATA_PORT"].as<uint64_t>();
  manager->ctrl_port = (uint16_t)vm["CTRL_PORT"].as<uint64_t>();
  manager->psize = vm["PSIZE"].as<uint64_t>();
  manager->fsize = vm["FSIZE"].as<uint64_t>() * 8000;
  manager->rtime = vm["RTIME"].as<uint64_t>();
  if (manager->psize <= 0 || manager->fsize <= 0 || manager->rtime <= 0 ||
      manager->data_port <= 0 || manager->ctrl_port <= 0) {
    perror("input shouldn't contain negative numbers.\n");
    exit(1);
  }
  manager->name = (vm["NAZWA"].as<std::string>());
  manager->control_socket_fd =
      bind_mulitcast_listen_socket(manager->ctrl_port, manager->mcast_addr);
  manager->reply_socket_fd = socket(PF_INET, SOCK_DGRAM, 0);
  manager->to_resend = new retransmission_set;
  manager->buffer = (char*)calloc(MAX_UDP_LEN, 1);
  manager->package_queue = new package_queue_t;
  manager->mcast_socket_fd = socket(PF_INET, SOCK_DGRAM, 0);
  manager->continue_work = true;
  broadcast_socket(manager->mcast_socket_fd);
  if (manager->mcast_socket_fd < 0) {
    PRINT_ERRNO();
  }
}

void send_to_mcast(manager_t* manager, byte_t* package) {
  struct sockaddr_in send_address =
      get_send_address(manager->mcast_addr.c_str(), manager->data_port);

  send_message<byte_t>(manager->mcast_socket_fd, &send_address,
                       manager->psize + 16, package);
}

void resend_packages(manager_t* manager) {
  manager->set_free.lock();

  uint64_t first_byte_num;
  std::for_each(manager->package_queue->begin(), manager->package_queue->end(),
                [&](byte_t* current_package) {
                  memcpy(&first_byte_num, current_package + 8, 8);
                  first_byte_num = ntohll(first_byte_num);
                  if (manager->to_resend->find(first_byte_num) !=
                      manager->to_resend->end()) {
                    send_to_mcast(manager, current_package);
                  }
                });
  manager->to_resend->clear();
  manager->set_free.unlock();
}

sockaddr_in receive_control_message(manager_t* manager) {
  struct sockaddr_in client_address;
  ssize_t message_length;
  while ((message_length =
              read_message<char>(manager->control_socket_fd, &client_address,
                                 manager->buffer, MAX_UDP_LEN)) == -1 &&
         manager->continue_work)
    ;
  return client_address;
}


void parse_control_message(manager_t* manager, struct sockaddr_in client) {
  struct sockaddr_in client_address = client;
  client_address.sin_port = htons(manager->ctrl_port);
  std::regex lookup("^ZERO_SEVEN_COME_IN$");
  std::regex louder("^LOUDER_PLEASE \\d+(,\\d+)*$");
  std::string message(manager->buffer);
  if (message.find('\n') == std::string::npos) {
    return;
  }
  message = message.substr(0, message.find('\n'));
  if (std::regex_search(message, lookup)) {
    std::string reply("BOREWICZ_HERE " + manager->mcast_addr + " " +
                      std::to_string(manager->data_port) + " " + manager->name +
                      "\n");
    send_message<char>(manager->reply_socket_fd, &client_address, reply.size(),
                       reply.c_str());
  }
  if (std::regex_search(message, louder)) {
    message = message.substr(13, message.length());
    std::stringstream ss(message);

    while (ss.good()) {
      std::string substr;
      getline(ss, substr, ',');
      uint64_t v = std::strtoull(substr.c_str(), NULL, 0);
      manager->to_resend->insert(v);
    }
  }
}

void handle_messages_connection(manager_t* manager) {
  struct sockaddr_in client = receive_control_message(manager);

  if (manager->continue_work)
    parse_control_message(manager, client);
}

void reply_messages(manager_t* manager) {
  resend_packages(manager);
}

}  // namespace

int main(int argc, char** argv) {
  connection_manager* manager = new connection_manager();
  pass_params_to_manager(manager, argc, argv);

  byte_t* package = (byte_t*)calloc(manager->psize + 16, 1);
  uint64_t first_byte = 0;
  std::vector<std::thread*> threads;
  uint64_t session_id = current_time_ms();

  std::thread send_music([&] {
    while (fread(package + 16, manager->psize, 1, stdin)) {
      memcpy(package, &session_id, sizeof session_id);
      uint64_t first_byte_rev = htonll(first_byte);
      memcpy(package + 8, &first_byte_rev, sizeof first_byte_rev);
      manager->set_free.lock();
      send_to_mcast(manager, package);
      manager->push_with_check(package);
      package = (byte_t*)calloc(manager->psize + 16, 1);
      manager->set_free.unlock();
      first_byte += manager->psize;
    }
    manager->continue_work = false;
  });

  std::thread handle_messages([&] {
    while (manager->continue_work) {
      handle_messages_connection(manager);
    }
  });

  std::thread reply([&] {
    while (manager->continue_work) {
      std::this_thread::sleep_for(std::chrono::milliseconds(manager->rtime));
      reply_messages(manager);
    }
  });

  threads.push_back(&send_music);
  threads.push_back(&handle_messages);
  threads.push_back(&reply);

  for (auto thread : threads) {
    (*thread).join();
  }

  /* free(package); */
  CHECK_ERRNO(close(manager->mcast_socket_fd));
  CHECK_ERRNO(close(manager->control_socket_fd));

  return 0;
}
