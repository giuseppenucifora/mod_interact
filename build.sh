erlc \
	-o ebin \
	-I include -I /projects/ejabberd/include/ \
    -pa /projects/ejabberd/deps/lager/ebin/ \
    -DLAGER -DNO_EXT_LIB \
    src/*erl




