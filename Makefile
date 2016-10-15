
DIRS=src/common/config,src/common/utils,src/common,src/engine,src/interface/gui,src/interface,src/script

ATD = src/common/config/Settings.atd\
			src/common/config/Settings_engine.atd\
			src/common/config/Settings_interface.atd\
			src/common/Tile.atd\
			src/common/Unit.atd\
			src/common/Building.atd

ATD_TML=$(ATD:.atd=_t.ml)
ATD_TMLI=$(ATD:.atd=_t.mli)
ATD_JML=$(ATD:.atd=_j.ml)
ATD_JMLI=$(ATD:.atd=_j.ml)
ATD_VML=$(ATD:.atd=_v.ml)
ATD_VMLI=$(ATD:.atd=_v.ml)

ATD_ML=$(ATD_TML) $(ATD_JML) $(ATD_VML)
ATD_FILES=$(ATD_TML) $(ATD_TMLI) $(ATD_JML) $(ATD_JMLI) $(ATD_VML) $(ATD_VMLI)


default: $(ATD_ML)
	ocamlbuild -use-ocamlfind -tag thread -package ogaml.graphics,yojson,atdgen -Is $(DIRS) main.native

%_t.ml:%.atd
	atdgen -t $<

%_j.ml:%.atd
	atdgen -j $<

%_v.ml:%.atd
	atdgen -v $<

clean:
	ocamlbuild -clean;
	rm -f $(ATD_FILES)
