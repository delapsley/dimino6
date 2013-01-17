#!/bin/bash

mkdir ${DIMINO6_ROOT}/rel
pushd ${DIMINO6_ROOT}/rel
${DIMINO6_ROOT}/tools/rebar create-node nodeid=dimino6 force=1
/bin/cp ${DIMINO6_ROOT}/lib/dimino6/src/reltool.config.src \
	${DIMINO6_ROOT}/rel/reltool.config
popd
