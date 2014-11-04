package = no_name
version = 0.1
tarname = $(package)
distdir = $(tarname)-$(version)

src_root=src
ressources_dir=$(src_root)/ressources
engine_dir=$(src_root)/engine
common_dir=$(src_root)/common
interface_dir=$(src_root)/interface
gui_dir=$(interface_dir)/gui
network_dir=$(src_root)/Reseaux

dirs = $(engine_dir) $(common_dir) $(interface_dir) $(network_dir) $(gui_dir)

engine_src=$(engine_dir),$(common_dir)
interface_src=$(engine_dir),$(common_dir),$(interface_dir),$(gui_dir)
network_src=$(network_dir)

common_dependencies = atdgen
interface_dependencies = ocsfml.graphics,$(common_dependencies)
network_libraries = unix

output_interface = main.native
output_engine = main_engine.native
output_network = Server.native

###########################################
#    Generate .ml files from .atd files   #
###########################################

# atd files
%_t.ml: %.atd
	atdgen -t $<
%_j.ml: %.atd
	atdgen -j $<


###########################################
#  get all the atd files in the project   #
###########################################

find_files_atd = $(wildcard $(sources)/*.atd)

files_atd := $(foreach dir,$(sources),$(find_files_atd))
files_atd_ml := $(files_atd:.atd=_t.ml) $(files_atd:.atd=_j.ml)

#useless ?
files_atd_mli := $(files_atd_ml:.ml=.mli)


interface: $(files_atd_ml) 
	ocamlbuild -use-ocamlfind -Is $(interface_src) -package $(interface_dependencies) $(output_interface)

engine :  $(file_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(engine_src) -package $(engine_dependencies) $(output_engine)

network :
	ocamlbuild -use-ocamlfind -libs $(network_libraries) -Is $(network_src) $(output_network)


dist: $(distdir).tar.gz

#option h is for deference symlinks
$(distdir).tar.gz: $(distdir)
	tar chf - $(distdir) | gzip -9 -c > $@
	rm -rf $(distdir)

#curly braces do not work !			
$(distdir): FORCE
	mkdir -p $(distdir)/src
	mkdir $(distdir)/$(common_src)
	mkdir $(distdir)/$(engine_src)
	mkdir $(distdir)/$(interface_src)
	mkdir $(distdir)/$(network_src)
	mkdir $(distdir)/$(ressources_dir)
	cp Makefile $(distdir)
	cp README.md $(distdir)
	cp configure $(distdir)
	cp $(common_dir)/*.ml $(distdir)/$(common_dir)
	cp $(common_dir)/*.atd $(distdir)/$(common_dir)
	cp $(engine_dir)/*.ml $(distdir)/$(engine_dir)
	cp $(interface_dir)/*.ml $(distdir)/$(interface_dir)
	cp $(gui_dir)/*.ml $(distdir)/$(gui_dir)
	-cp $(network_dir)/*.ml $(distdir)/$(network_dir)
	cp $(ressources_dir)/* $(distdir)/$(ressources_dir)

FORCE:
	-rm $(distdir).tar.gz > /dev/null 2>&1
	-rm -rf $(distdir) > /dev/null 2>&1

clean:
	ocamlbuild -clean

check:
	@echo "To be completed, this is a command that returns 0 for Travis."

.PHONY: FORCE all clean dist engine interface network
