.onLoad = function(libname, pkgname) {
    # library.dynam("sysfonts", pkgname, libname)
    add_default_font_paths()
    add_default_fonts()
}

.onUnload = function(libpath) {
    clean_fonts()
    library.dynam.unload("sysfonts", libpath)
}
