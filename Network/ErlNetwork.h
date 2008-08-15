// Network module for a node in the oe+erlang network.
// -------------------------------------------------------------------
// Copyright (C) 2007 OpenEngine.dk (See AUTHORS) 
// 
// This program is free software; It is covered by the GNU General 
// Public License version 2 or any later version. 
// See the GNU General Public License for more details (see LICENSE). 
//--------------------------------------------------------------------

#ifndef _OE_ERL_NETWORK_H_
#define _OE_ERL_NETWORK_H_

#include <Core/Event.h>
#include <Core/IModule.h>

#include <string>

namespace OpenEngine {
namespace Network {

using OpenEngine::Core::IModule;
using OpenEngine::Core::Event;

using std::string;

typedef uint16_t EventId;
typedef uint32_t EventLength;
typedef char* Payload;

struct NetworkEventArg {
    EventId id;
    EventLength length;
    Payload payload;
};

class ErlNetwork : public IModule, public Event<NetworkEventArg> {
protected:
    string         host;
    unsigned short port;
    int            sock;

    virtual void Connect();
    virtual void Write(Payload buf, EventLength len);
    virtual void Read(Payload buf, EventLength len);

public:

    ErlNetwork(string host, unsigned short port);
    virtual ~ErlNetwork();

    void Handle(OpenEngine::Core::InitializeEventArg arg);
    void Handle(OpenEngine::Core::ProcessEventArg arg);
    void Handle(OpenEngine::Core::DeinitializeEventArg arg);

    /**
     * Notify over the network.
     * This will broadcast the event to all other nodes on the network.
     */
    virtual void Notify(NetworkEventArg arg);

    // I think we should make this the number of network nodes?
    // int Size();

};

} // NS Network
} // NS OpenEngine

#endif // _OE_ERL_NETWORK_H_
