erlc \
	-o ebin \
	-I include -I /opt/ejabberd/lib/ejabberd/include/ \
	-I /opt/ejabberd/lib/p1_xml/include/ \
	-pa /opt/ejabberd/lib/lager/ebin/ \
    -DLAGER -DNO_EXT_LIB \
    src/*erl




