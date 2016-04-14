#!/usr/bin/env bash

rm ebin/*.beam

erlc \
	-o ebin \
	-I include -I /opt/ejabberd-16.03/lib/ejabberd-16.03/include/ \
	-I /opt/ejabberd-16.03/lib/p1_xmlrpc-1.15.1/include/ \
        -I /opt/ejabberd-16.03/lib/fast_xml-1.1.3/include/ \
	-pa /opt/ejabberd-16.03/lib/lager-3.0.2/ebin/ \
    -DLAGER -DNO_EXT_LIB \
    src/*erl

cp ebin/*.beam /opt/ejabberd-16.03/lib/ejabberd-16.03/ebin/

chown -R ejabberd:ejabberd /opt/ejabberd-16.03/lib/ejabberd-16.03/ebin/