erlc \
	-o ebin \
	-I include -I /opt/ejabberd-16.02/lib/ejabberd-16.02/include/ \
	-I /opt/ejabberd-16.02/lib/p1_xmlrpc-1.15.1/include/ \
        -I /opt/ejabberd-16.02/lib/fast_xml-1.1.3/include/ \
	-pa /opt/ejabberd-16.02/lib/lager-3.0.2/ebin/ \
    -DLAGER -DNO_EXT_LIB \
    src/*erl