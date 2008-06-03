// Network exception.
// -------------------------------------------------------------------
// Copyright (C) 2007 OpenEngine.dk (See AUTHORS) 
// 
// This program is free software; It is covered by the GNU General 
// Public License version 2 or any later version. 
// See the GNU General Public License for more details (see LICENSE). 
//--------------------------------------------------------------------

#include <Core/Exceptions.h>

namespace OpenEngine {
namespace Network {

using OpenEngine::Core::Exception;
using std::string;

/**
 * Network exception.
 *
 * @class NetworkException NetworkException.h Network/NetworkException.h
 */
class NetworkException : public Exception {
public:
    NetworkException(string msg) : Exception(msg) {};
};

} // NS Network
} // NS OpenEngine
