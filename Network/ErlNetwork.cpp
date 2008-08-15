// Network module for a node in the oe+erlang network.
// -------------------------------------------------------------------
// Copyright (C) 2007 OpenEngine.dk (See AUTHORS) 
// 
// This program is free software; It is covered by the GNU General 
// Public License version 2 or any later version. 
// See the GNU General Public License for more details (see LICENSE). 
//--------------------------------------------------------------------

#include <Network/ErlNetwork.h>
#include <Network/NetworkException.h>

#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

#include <list>

namespace OpenEngine {
namespace Network {

using std::list;
using OpenEngine::Core::IListener;

void ErlNetwork::Connect() {
    sockaddr_in sa;
    hostent* hp;
    
    if ((hp = gethostbyname(this->host.c_str())) == NULL)
        throw NetworkException("Could not resolve host '"+this->host + "' "
                               + string(strerror(errno)));

    memset(&sa, 0, sizeof(sa));
    memcpy((char*)&sa.sin_addr, hp->h_addr, hp->h_length);
    sa.sin_family = hp->h_addrtype;
    sa.sin_port = htons(port);

    if ((this->sock = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0)
        throw NetworkException("Socket error: " + string(strerror(errno)));

    if (connect(this->sock, (sockaddr*)&sa, sizeof(sa)) < 0) {
        close(this->sock);
        throw NetworkException("Connect error: " + string(strerror(errno)));
    }
}

ErlNetwork::ErlNetwork(string host, unsigned short port) {
    this->host = host;
    this->port = port;
    this->Connect();
}

ErlNetwork::~ErlNetwork() {
    close(this->sock);
}

  void ErlNetwork::Handle(OpenEngine::Core::InitializeEventArg arg) 
  {
  }
  
  void ErlNetwork::Handle(OpenEngine::Core::ProcessEventArg arg) 
  {
    timeval tv;
    fd_set fds;
    tv.tv_sec = 0;
    tv.tv_usec = 1;
    FD_ZERO(&fds);
    FD_SET(this->sock, &fds);
    select(this->sock+1, &fds, NULL, NULL, &tv);
    // while we have network input read it
    while(FD_ISSET(this->sock, &fds)) {
        // read in header
        char        hd[6];
        EventLength nlen;
        EventId     nid;
        Read(hd, 6);
        memcpy(&nlen, hd,     4);
        memcpy(&nid,  &hd[4], 2);

        // pack in to a network arg
        NetworkEventArg arg;
        arg.id      = ntohs(nid);
        arg.length  = ntohl(nlen)-2;
        char buf[arg.length];
        arg.payload = buf;
        Read(arg.payload, arg.length);

        // notify all
        list<IListener<NetworkEventArg>*>::iterator itr;
        for (itr=ls.begin(); itr != ls.end(); itr++)
            (*itr)->Handle(arg);

        // reset the fds and select again (is all this necessary?)
        FD_ZERO(&fds);
        FD_SET(this->sock, &fds);
        select(this->sock+1, &fds, NULL, NULL, &tv);        
    }

  }
  
  void ErlNetwork::Handle(OpenEngine::Core::DeinitializeEventArg arg) 
  {
  }
   
/**
 * Notify over the network.
 * This will broadcast the event to all other nodes on the network.
 */
void ErlNetwork::Notify(NetworkEventArg arg) {
    // convert header information to network order
    char hd[6];
    EventLength nlen = htonl(arg.length + 2); // @todo: possible overflow?!
    EventId     nid  = htons(arg.id);
    memcpy(hd,     &nlen, 4);
    memcpy(&hd[4], &nid,  2);

    // send the header
    Write(hd, 6);

    // send the payload
    Write(arg.payload, arg.length);
}

void ErlNetwork::Write(char* buf, EventLength len) {
    EventLength bs, bt;  // bytes sent and total
    bs = bt = 0;
    while (bt < len) {
        if ((bs = send(this->sock, buf, len-bt, 0)) < 0)
            throw NetworkException("Write error: " + string(strerror(errno)));
        bt  += bs;       // add to total bytes and
        buf += bs;       // increment buffer pointer
    }
}

void ErlNetwork::Read(char* buf, EventLength len) {
    EventLength br, bt;  // bytes received and total
    br = bt = 0;
    while (br < len) {
        if ((br = recv(this->sock, buf, len-bt, 0)) < 1)
            throw NetworkException("Read error: " + string(strerror(errno)));
        bt  += br;       // add to total bytes and
        buf += br;       // increment buffer pointer
    }
}

} // NS Network
} // NS OpenEngine
