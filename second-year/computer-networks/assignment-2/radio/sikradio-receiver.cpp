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

#include <array>
#include <boost/program_options.hpp>
#include <chrono>
#include <cstdio>
#include <iostream>
#include <istream>
#include <iterator>
#include <mutex>
#include <regex>
#include <set>
#include <string>
#include <thread>

#include "common.hpp"
#include "err.h"

#define MAX_UDP_LENGTH 1000

namespace {

using namespace common;
namespace po = boost::program_options;

struct station {
  std::string name;
  uint16_t port;
  std::string mcast_addr;
  mutable uint64_t time;
  bool operator<(const station& rstation) const {
    return (this->mcast_addr == rstation.mcast_addr
                ? this->port < rstation.port
                : this->mcast_addr < rstation.mcast_addr);
  }
};
using station_t = struct station;

struct connection_manager {
  uint64_t session_id;
  uint64_t first_byte_num;
  uint64_t psize;
  struct sockaddr_in client_address;
  uint64_t current_first_byte;
  uint64_t max_first_byte;
  uint64_t current_id;
  std::set<uint64_t> lost_packages;
  byte_t* temp;
  bool overflow;
};

using connection_manager_t = struct connection_manager;

struct options {
  std::string dest_addr;
  std::string discover_addr;
  uint16_t data_port;
  uint16_t ctrl_port;
  uint16_t ui_port;
  uint64_t bsize;
  uint64_t rtime;
  std::string favorite_radio;

  uint64_t last_session;
  byte_t* buffer;
  int socket_fd;
  int ctrl_socket_fd;
  int ctrl_listen_fd;
  int ui_fd;
  std::set<station_t> available_stations;
  std::mutex ui_mutex;
  std::mutex set_mutex;
  std::mutex ctrl_mutex;

  connection_manager_t* manager;

  void add_new_station(station_t station) {

    std::pair<std::set<station_t>::iterator, bool> it =
        this->available_stations.insert(station);
    if (!it.second) {
      this->available_stations.erase(it.first);
      this->available_stations.insert(station);
    }
  }
  bool choose_station_if_available() {
    if (this->favorite_radio == std::string("UNDEFINED")) {

      for (auto it = available_stations.begin(); it != available_stations.end();
           it++) {
        if (current_time_ms() - it->time < 20 * 1000) {
          this->dest_addr = it->mcast_addr;
          this->data_port = it->port;
          return 1;
        }
      }
      return 0;
    }
    for (auto it = available_stations.begin(); it != available_stations.end();
         it++) {
      if (it->name == this->favorite_radio &&
          current_time_ms() - it->time < 20 * 1000) {
        this->dest_addr = it->mcast_addr;
        this->data_port = it->port;
        return 1;
      }
    }
    return 0;
  }
  bool current_station_is_available() {
    station_t current_station;
    current_station.mcast_addr = this->dest_addr;
    current_station.port = this->data_port;
    return (this->available_stations.find(current_station) !=
                this->available_stations.end() &&
            current_time_ms() -
                    this->available_stations.find(current_station)->time <
                20 * 1000);
  }
};
using options_t = struct options;

void pass_params_to_options(options_t* options, int argc, char** argv) {

  boost::program_options::options_description desc(
      "\nMandatory arguments marked with '*'.\n"
      "Invocation : <program> -d <discover_addr> -P <data_port> -C <ctrl_port> "
      "-U <ui_port> -b "
      "<bsize> -R <rtime> \nAgruments");

  desc.add_options()("DISCOVER_ADDR,d",
                     po::value<std::string>()->default_value("255.255.255.255"),
                     "Sender adress")(
      "DATA_PORT,P", po::value<uint64_t>()->default_value(26366),
      "Port used for data transfer")(
      "CTRL_PORT,C", po::value<uint64_t>()->default_value(36366),
      "Port used for control data transfer")(
      "UI_PORT,U", po::value<uint64_t>()->default_value(16366),
      "Port used for user interface")(
      "BSIZE,b", po::value<uint64_t>()->default_value(64), "Buffer size")(
      "RTIME,R", po::value<uint64_t>()->default_value(250),
      "Interval between package requests")(
      "NAZWA,n", po::value<std::string>()->default_value("UNDEFINED"),
      "The name of your favorito radio");

  po::variables_map vm;

  try {
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
  } catch (po::error& e) {
    std::cerr << "ERROR: " << e.what() << "\n";
    std::cout << desc << "\n";
    exit(1);
  }
  if (vm["DATA_PORT"].as<uint64_t>() > UINT16_MAX) {
    std::cerr << "Invalid port number. \n";
    exit(1);
  }
  if (vm["CTRL_PORT"].as<uint64_t>() > UINT16_MAX) {
    std::cerr << "Invalid port number. \n";
    exit(1);
  }
  if (vm["UI_PORT"].as<uint64_t>() > UINT16_MAX) {
    std::cerr << "Invalid port number. \n";
    exit(1);
  }

  options->dest_addr = "";
  options->discover_addr = vm["DISCOVER_ADDR"].as<std::string>();
  options->data_port = (uint16_t)vm["DATA_PORT"].as<uint64_t>();
  options->ctrl_port = (uint16_t)vm["CTRL_PORT"].as<uint64_t>();
  options->ui_port = (uint16_t)vm["UI_PORT"].as<uint64_t>();
  options->bsize = vm["BSIZE"].as<uint64_t>() * 8000;
  options->rtime = vm["RTIME"].as<uint64_t>();
  if (options->bsize <= 0 || options->rtime <= 0) {
    std::cerr << "Input should contain no negative numbers.\n";
    exit(1);
  }
  options->buffer = (byte_t*)calloc((options->bsize + 16), sizeof(byte_t));
  options->ctrl_listen_fd = bind_socket(options->ctrl_port, "INADDR_ANY");
  options->favorite_radio = vm["NAZWA"].as<std::string>();
  options->manager = new connection_manager();
  options->ctrl_socket_fd = socket(PF_INET, SOCK_DGRAM, 0);
  broadcast_socket(options->ctrl_socket_fd);
}

bool receive_first_message(struct connection_manager* manager,
                           options_t* options) {
  options->set_mutex.lock();
  manager->lost_packages.clear();
  options->set_mutex.unlock();
  memset(options->buffer, 0, options->bsize + 16);
  ssize_t message_length;
  do {
    if (!options->current_station_is_available()) {
      return 0;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(1));

  } while ((message_length = read_message<byte_t>(
                options->socket_fd, &(manager->client_address), options->buffer,
                options->bsize + 16)) == -1);
  manager->psize = (uint64_t)message_length - 16;
  memcpy(&(manager->session_id), options->buffer, sizeof manager->session_id);
  memcpy(&(manager->first_byte_num), options->buffer + 8,
         sizeof manager->first_byte_num);
  manager->session_id = ntohll(manager->session_id);
  if (manager->session_id < options->last_session) {
    return 0;
  }

  manager->first_byte_num = ntohll(manager->first_byte_num);
  manager->max_first_byte = manager->first_byte_num;
  manager->current_first_byte = manager->first_byte_num;
  return 1;
}

bool receive_next_message(struct connection_manager* manager,
                          options_t* options) {
  ssize_t message_length;
  do {
    if (!options->current_station_is_available()) {
      return 0;
    }
  } while ((message_length = read_message<byte_t>(
                options->socket_fd, &manager->client_address, manager->temp,
                manager->psize + 16)) == -1);
  memcpy(&manager->current_id, manager->temp, sizeof manager->current_id);
  manager->current_id = ntohll(manager->current_id);
  if (manager->current_id < manager->session_id)
    return 1;
  if (manager->current_id > manager->session_id) {
    free(manager->temp);
    options->last_session = manager->current_id;
    return 0;
  }
  memcpy(&manager->current_first_byte, manager->temp + 8,
         sizeof manager->current_first_byte);
  manager->current_first_byte = ntohll(manager->current_first_byte);
  options->set_mutex.lock();
  for (size_t i = manager->max_first_byte + manager->psize;
       i < manager->current_first_byte; i += manager->psize) {
    manager->lost_packages.insert(i);
  }

  for (auto it = manager->lost_packages.begin();
       it != manager->lost_packages.end() && *it < manager->current_first_byte;
       it++) {}
  manager->lost_packages.erase(manager->current_first_byte);
  options->set_mutex.unlock();
  if (manager->current_first_byte + manager->psize - manager->first_byte_num <
      options->bsize) {
    memcpy(options->buffer + 16 + manager->current_first_byte -
               manager->first_byte_num,
           manager->temp + 16, manager->psize);
    manager->max_first_byte =
        std::max(manager->max_first_byte, manager->current_first_byte);

  } else {
    manager->overflow = true;
  }
  return 1;
}

void send_lookup_message(options_t* options) {
  std::string message = "ZERO_SEVEN_COME_IN\n";
  struct sockaddr_in send_address =
      get_send_address(options->discover_addr.c_str(), options->ctrl_port);
  options->ctrl_mutex.lock();
  send_message<char>(options->ctrl_socket_fd, &send_address, message.size(),
                     message.c_str());
  options->ctrl_mutex.unlock();
}

void receive_control_message(options_t* options) {
  char* buffer = (char*)malloc(MAX_UDP_LENGTH - 900);
  std::regex lookup("BOREWICZ_HERE *");
  struct sockaddr_in client_address;

  while (read_message<char>(options->ctrl_listen_fd, &client_address, buffer,
                            MAX_UDP_LENGTH) == -1)
    ;
  std::string message(buffer);
  if (message.find('\n') == std::string::npos) {
    return;
  }

  message = message.substr(0, message.find('\n'));

  if (std::regex_search(message, lookup)) {
    station_t station;
    message =
        message.substr(message.find("BOREWICZ_HERE") + 14, std::string::npos);
    station.mcast_addr = message.substr(0, message.find(' '));
    message = message.substr(message.find(' ') + 1, std::string::npos);
    station.port = stoi(message.substr(0, message.find(' ')));
    station.name = message.substr(message.find(' ') + 1, std::string::npos);
    station.time = current_time_ms();
    options->add_new_station(station);
  }

  return;
}

void find_suitable_station(options_t* options) {
  if (!options->current_station_is_available()) {

    while (!options->choose_station_if_available())
      std::this_thread::sleep_for(std::chrono::seconds(1));
    close(options->socket_fd);
    options->socket_fd =
        bind_mulitcast_listen_socket(options->data_port, options->dest_addr);
  }
}

bool handle_connection(options_t* options) {
  struct connection_manager* manager = options->manager;
  manager->current_id = 0;
  bool start = true;
  manager->overflow = false;

  for (;;) {
    manager->overflow = false;

    if (!receive_first_message(manager, options)) {
      return 1;
    }

    if (start) {
      manager->temp = (byte_t*)malloc((manager->psize + 16) * sizeof(byte_t));
      start = false;
    }

    while (manager->current_first_byte + manager->psize -
               manager->first_byte_num <
           options->bsize * 3 / 4) {
      if (!receive_next_message(manager, options)) {
        return 1;
      }
    }

    fwrite(options->buffer + 16, sizeof(byte_t),
           manager->max_first_byte + manager->psize - manager->first_byte_num,
           stdout);
    if (manager->overflow) {
      fwrite(manager->temp + 16, sizeof(byte_t), manager->psize, stdout);
    }
    fflush(stdout);
  }
  options->last_session = manager->session_id;
  free(manager->temp);
  return 1;
}

void open_ui_socket(options_t* options) {
  struct addrinfo hints, *res;
  int sockfd;

  // first, load up address structs with getaddrinfo():

  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;  // use IPv4 or IPv6, whichever
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;  // fill in my IP for me

  getaddrinfo(NULL, std::to_string(options->ui_port).c_str(), &hints, &res);

  // make a socket, bind it, and listen on it:

  sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  bind(sockfd, res->ai_addr, res->ai_addrlen);
  listen(sockfd, 5);

  // now accept an incoming connection:

  options->ui_fd = sockfd;

  // ready to communicate on socket descriptor new_fd!/ then have an accept() loop down here somewhere
}

void accept_ui(options_t* options) {
  struct sockaddr_storage their_addr;
  socklen_t addr_size;
  /* uint8_t* buffer = (uint8_t*)calloc(3, sizeof(uint8_t)); */
  char* buffer2 = (char*)calloc(1, sizeof(char));
  int new_fd;
  addr_size = sizeof their_addr;
  for (;;) {
    new_fd = accept(options->ui_fd, (struct sockaddr*)&their_addr, &addr_size);

    int flags = fcntl(new_fd, F_GETFL, 0);
    fcntl(new_fd, F_SETFL, flags | O_NONBLOCK);
    /* unblock_socket(new_fd);  */
    /* errno = 0; */
    /* options->send_message_to_ui(); */
    for (;;) {
      std::this_thread::sleep_for(std::chrono::seconds(1));

      options->ui_mutex.lock();
      /* fsync(new_fd); */

      read(new_fd, buffer2, 1);
      /* if(errno == 0) { */
      /* } */
      /* send_message_to_ui() */
      options->ui_mutex.unlock();
    }

    close(new_fd);
  }
}

void send_louder_message(options_t* options) {
  options->set_mutex.lock();
  if (options->manager->lost_packages.size() != 0) {
    std::string message("LOUDER_PLEASE ");
    int i = 0;
    for (uint64_t num : options->manager->lost_packages) {
      if (i < 10) {
        message += std::to_string(num) + ",";
      }
      i++;
    }
    message = message.substr(0, message.length() - 2);
    message += "\n";
    struct sockaddr_in send_address =
        get_send_address(options->discover_addr.c_str(), options->ctrl_port);
    options->ctrl_mutex.lock();
    send_message<char>(options->ctrl_socket_fd, &send_address, message.size(),
                       message.c_str());
    options->ctrl_mutex.unlock();
  }
  options->set_mutex.unlock();
}

}  // namespace

int main(int argc, char** argv) {
  options_t* options = new options_t();
  pass_params_to_options(options, argc, argv);
  std::vector<std::thread*> threads;
  std::thread receive_music([&] {
    for (;;) {
      find_suitable_station(options);
      options->last_session = 0;
      //find available station
      handle_connection(options);
    }
  });

  std::thread send_lookup([&] {
    //every 5sec send lookup message to discover_addr
    for (;;) {
      std::this_thread::sleep_for(std::chrono::seconds(5));
      send_lookup_message(options);
    }
  });
  std::thread receive_lookup([&] {
    //listen on a ctrl_port
    //when a message is received, update the stations listen
    while (1) {
      std::this_thread::sleep_for(
          std::chrono::milliseconds(options->rtime / 100));
      receive_control_message(options);
    }
  });

  std::thread send_louder([&] {
    //once in rtime send rexmit based on lost packages set.
    for (;;) {
      std::this_thread::sleep_for(std::chrono::milliseconds(options->rtime));
      send_louder_message(options);
    }
  });

  //somehow configure UI idk yet how to implenet it>>>
  std::thread user_iface([&] {
    /* int ui_socket = bind_socket(options->ui_port, "INADDR_ANY"); */
    /* listen(ui_socket, 10); */
    open_ui_socket(options);
    accept_ui(options);
  });

  threads.push_back(&receive_music);
  threads.push_back(&send_lookup);
  threads.push_back(&receive_lookup);
  threads.push_back(&send_louder);
  threads.push_back(&user_iface);

  for (auto thread : threads) {
    (*thread).join();
  }

  delete options->buffer;
  delete options;
  CHECK_ERRNO(close(options->socket_fd));
  return 0;
}
