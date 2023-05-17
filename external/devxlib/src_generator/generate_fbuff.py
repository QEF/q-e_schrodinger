#!/usr/bin/env python3

import os, glob, jinja2
from shutil import copyfile


def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return jinja2.Environment(undefined=jinja2.StrictUndefined,
        loader=jinja2.FileSystemLoader(path or './')
    ).get_template(filename).render(context)


rankmap = {1:'vector', 2:'matrix', 3:'tensor', 4:'four_dimensional'}

def gen_interface_list(types):
    intrfc_list = []
    for t in types:
        for rank in (1,2,3,4):
            intrfc_list.append({
                      'vtype'     : t,
                      'rank'      : rank,
                      'vrankname' : rankmap[rank],
                      'subname'   : t[0]+rankmap[rank][0],
                      'vsize'     : rankmap[rank][0]+'size',
                      'vsizedims' : "({})".format(rank),
                      'ranks'     : ",".join([':']*rank)
                      })
    return intrfc_list


def gen_module(context):
    fname = 'device_fbuff.jf90'
    bname, _ = os.path.splitext(fname)
    with open(bname + '.f90','a') as f:
        f.write(render(fname, context))

def gen_test(context):
    fname = 'test_fbuff.jf90'
    bname, _ = os.path.splitext(fname)
    with open(bname + '.f90','w') as f:
        f.write(render(fname, context))

def main():
    # clean up
    open('device_fbuff.f90','w').close()
    
    # Types used in the interface
    interfaces = gen_interface_list(['integer', 'real(DP)', 'complex(DP)'])
    context = {'interfaces'  : interfaces, 
               'cudamod'     : 'use cudafor', 
               'attributes'  : 'device',
               'pointer_type': 'c_devptr',
               'modulename'  : 'tb_dev',
               'typename'    : 'tb_dev_t'}
    gen_module(context)
    
    context = {'interfaces'  : interfaces, 
               'cudamod'     : 'use cudafor', 
               'attributes'  : '',
               'pointer_type': 'c_ptr',
               'modulename'  : 'tb_pin',
               'typename'    : 'tb_pin_t'}
    gen_module(context)
    
    gen_test(context)

    # set multiple extensions
    copyfile("device_fbuff.f90","device_fbuff.F")
    

if __name__ == "__main__":
    main()
