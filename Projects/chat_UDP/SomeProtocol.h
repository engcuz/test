//
// Created by Nathan Evans
//

#ifndef SIMPLE_UDP_2022_SOMEPROTOCOL_H
#define SIMPLE_UDP_2022_SOMEPROTOCOL_H




enum MsgType {
  CLIENT_HELLO = 1705,
  CLIENT_MSG,
  CLIENT_SET_NICKNAME,
  CLIENT_BYEBYE,
  SERVER_ACK,
  SERVER_MSG
};

struct ChatMessage {
  uint16_t type;
  uint16_t msg_len;
  uint32_t message_uid;
  // Message data follows!
};
#endif //SIMPLE_UDP_2022_SOMEPROTOCOL_H
