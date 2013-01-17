#!/bin/bash

${DIMINO6_ROOT}/tools/rebar compile
/bin/cp ${DIMINO6_ROOT}/lib/dimino6/src/dimino6.app.src \
    ${DIMINO6_ROOT}/lib/dimino6/ebin/dimino6.app
/bin/cp ${DIMINO6_ROOT}/lib/dimino6/src/reltool.config.src \
    ${DIMINO6_ROOT}/rel/reltool.config
/bin/rm -fr ${DIMINO6_ROOT}/rel/dimino6
${DIMINO6_ROOT}/tools/rebar generate force=1
chmod u+x ${DIMINO6_ROOT}/rel/dimino6/bin/dimino6
