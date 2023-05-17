#!/usr/bin/env python3

import sys, os, jinja2
from shutil import copyfile

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)

####

types_l=['real', 'complex', 'integer']

sp_d={'name':'sp', 'val':'real32'}
dp_d={'name':'dp', 'val':'real64'}
i4_d={'name':'i4', 'val':'int32'}
i8_d={'name':'i8', 'val':'int64'}

kinds_d={'real':    [sp_d, dp_d] , 
         'complex': [sp_d, dp_d] , 
         'integer': [i4_d] }
#         'integer': [i4_d, i8_d] }

nranks=4

# Generate subroutines
with open('device_memcpy.f90', 'w') as f:
    f.write(render('device_memcpy.jf90', 
                    {'types' : types_l, 'kinds' : kinds_d, 'dimensions': nranks }
                  ))

# Generate interfaces
with open('device_memcpy_interf.f90', 'w') as f:
    f.write(render('device_memcpy_interf.jf90', 
                    {'types' : types_l, 'kinds' : kinds_d, 'dimensions': nranks }
                  ))
            
# Generate tests
with open('test_memcpy.f90', 'w') as f:
    f.write(render('test_memcpy.jf90', 
                    {'types' : types_l, 'kinds' : kinds_d, 'dimensions': nranks }
                  ))
with open('test_memcpy_async.f90', 'w') as f:
    f.write(render('test_memcpy_async.jf90', 
                    {'types' : types_l, 'kinds' : kinds_d, 'dimensions': nranks }
                  ))

# set multiple extensions
copyfile("device_memcpy.f90","device_memcpy.F")
copyfile("device_memcpy_interf.f90","device_memcpy_interf.F")

