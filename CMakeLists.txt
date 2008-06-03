SET( EXTENSION_NAME "Extensions_ErlNetwork")

ADD_LIBRARY( ${EXTENSION_NAME}
  Network/ErlNetwork.cpp
)

TARGET_LINK_LIBRARIES( ${EXTENSION_NAME}
  OpenEngine_Core
)
