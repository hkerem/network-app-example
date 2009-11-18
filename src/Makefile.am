dist_erl_DATA = netapp_app.erl netapp_sup.erl netapp_server.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/src
dist_hrl_DATA = netapp.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) netapp.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_netapp)/ebin
dist_priv_DATA = netapp.conf
privdir = $(ERLANG_INSTALL_LIB_DIR_netapp)/priv
html_DATA = $(dist_erl_DATA:.erl=.html) erlang.png edoc-info stylesheet.css \
	index.html overview-summary.html modules-frame.html packages-frame.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_netapp)/doc
dist_noinst_DATA = overview.edoc.in
boot_DATA = netapp.boot netapp.script
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