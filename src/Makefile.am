dist_erl_DATA = \
	gen_server_cluster.erl \
	netapp.erl \
	netapp_app.erl \
	netapp_acceptor.erl \
	netapp_echo_worker.erl \
	netapp_echo_fsm.erl \
	netapp_sup.erl \
	netapp_worker_sup.erl

dist_hrl_DATA = \
	netapp.hrl

beam_DATA = \
	$(dist_erl_DATA:.erl=.beam) \
	netapp.app

#dist_priv_DATA = \
#	netapp.conf

boot_DATA = \
	netapp.boot \
	netapp.script

dist_noinst_DATA = \
	overview.edoc.in

html_DATA = \
	$(dist_erl_DATA:.erl=.html) \
	erlang.png \
	edoc-info \
	stylesheet.css \
	index.html \
	overview-summary.html \
	modules-frame.html \
	packages-frame.html

erldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/src
hrldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/include
beamdir = $(ERLANG_INSTALL_LIB_DIR_netapp)/ebin
privdir = $(ERLANG_INSTALL_LIB_DIR_netapp)/priv
htmldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/doc
bootdir = $(libdir)/erlang/bin
CLEANFILES = $(beam_DATA) $(boot_DATA) $(html_DATA) netapp.rel overview.edoc
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

.rel.boot:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

$(html_DATA): $(dist_erl_DATA) overview.edoc 
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application netapp '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,netapp})" -noshell -s init stop

console:
	@echo "Running application..."
	@$(ERL) -boot start_sasl -sname console-$$$$ 
