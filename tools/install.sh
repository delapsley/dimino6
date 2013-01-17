#!/bin/bash

# Install pre-requisites
PREQ="ncurses-dev libssl-dev build-essential git-core"
sudo /usr/bin/apt-get -y install ${PREQ}

# Install Erlang/OTP
OTP_SRC=otp_src_R14B02
OTP_DIR=${DIMINO6_ROOT}/lib/dimino6/priv/

pushd ${OTP_DIR}
/bin/tar xzf ${OTP_SRC}.tar.gz
cd ${OTP_SRC}
./configure --prefix=${HOME}
make install
make clean
cd ..
rm -fr ${OTP_SRC}
popd

# Build a node
cd ${DIMINO6_ROOT}
tools/node.sh

# Build a release
tools/generate.sh

