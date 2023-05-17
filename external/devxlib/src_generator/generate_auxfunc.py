#!/usr/bin/env python3

import sys, os, jinja2
from shutil import copyfile

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)

####

types_l=['real', 'complex']
nranks=4
kinds_l= [{'name':'dp', 'val':'selected_real_kind(14,200)'}, 
          {'name':'sp', 'val':'selected_real_kind(6, 37)'}]

# Generate subroutines
with open('device_auxfunc.f90', 'w') as f:
    f.write(render('device_auxfunc.jf90', 
                    {'types' : types_l, 'dimensions': nranks, 'precision' : kinds_l }
                  ))

# Generate interfaces
with open('device_auxfunc_interf.f90', 'w') as f:
    f.write(render('device_auxfunc_interf.jf90', 
                    {'types' : types_l, 'dimensions': nranks, 'precision' : kinds_l }
                  ))
            
## Generate tests
#with open('test_auxfunc.f90', 'w') as f:
#    f.write(render('test_auxfunc.jf90', 
#                    {'types' : types_l, 'dimensions': nranks, 'precision' : kinds_l }
#                  ))

# set multiple extensions
copyfile("device_auxfunc.f90","device_auxfunc.F")
copyfile("device_auxfunc_interf.f90","device_auxfunc_interf.F")

