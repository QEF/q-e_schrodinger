#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato, Giovanni Borghi
#
"""
Conversion functions for Quantum Espresso input options.
"""

import logging
from .exceptions import ConfigError

# from .utils import set_logger

logger = logging.getLogger('qespresso')


#
# Other derived values
def get_specie_related_values(name, **kwargs):
    """
    Convert XML data for specie related options. Map single values,
    vectors and matrices. Skip 0 values for Hubbard parameters or
    negative values for starting_ns eigenvalues. Skip entire vector
    or matrix when

    :param name: parameter name
    :param kwargs:
    :return: string
    """
    related_tag = kwargs['_related_tag']
    related_data = kwargs[related_tag]

    try:
        atomic_species = kwargs['atomic_species']
        species = atomic_species['species']
    except KeyError as err:
        key = str(err).strip("'")
        if key != '_text':
            logger.error("Missing required arguments when building "
                         "parameter '%s'! %s" % (name, key))
        return []

    lines = []
    for value in iter(related_data if isinstance(related_data, list) else [related_data]):
        tag_specie = value['specie']
        tag_values = value['_text']
        if value.get('label') == 'no Hubbard':
            continue

        specie_index = 1
        for specie in species:
            if specie['name'] == tag_specie:
                break
            specie_index += 1
        else:
            raise ConfigError("Unknown specie '%s' in tag '%s'" % (tag_specie, name))

        if isinstance(tag_values, list):
            for k in range(len(tag_values)):
                # starting_ns case: skip negative values
                if tag_values[k] < 0 or (name == 'Hubbard_J' and tag_values[k] == 0):
                    continue
                lines.append(' {0}({1},{2})={3}'.format(
                    name, k + 1, specie_index, tag_values[k]
                ))
        elif tag_values > 0:
            lines.append(' {0}({1})={2}'.format(name, specie_index, tag_values))
    return lines


def get_starting_magnetization(name, **kwargs):
    """
    Build starting magnetization vector from species data.

    :param name: parameter name
    :param kwargs:
    :return: string
    """
    try:
        atomic_species = kwargs['atomic_species']
        species = atomic_species['species']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    lines = []
    try:
        lines.append(' {0}(1)={1}'.format(name, species.get('starting_magnetization', 0.0)))
    except AttributeError:
        k = 0
        for specie in species:
            k += 1
            lines.append(' {0}({1})={2}'.format(
                name, k, specie.get('starting_magnetization', 0.0)
            ))
    return lines


def get_system_nspin(name, **kwargs):
    """
    Get the value for 'nspin' parameter of the SYSTEM namelist.

    :param name:
    :param kwargs:
    :return:
    """
    try:
        lsda = kwargs['lsda']
        if lsda:
            return [' nspin=2']

        noncolin = kwargs['noncolin']
        if noncolin:
            return [' nspin=4']
        else:
            return [' nspin=1']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

def set_ibrav_to_zero(name,**kwargs):
    line = '  ibrav = 0'
    return [line]

def get_system_eamp(name, **kwargs):
    """

    :param name:
    :param kwargs:
    :return:
    """
    try:
        electric_potential = kwargs['electric_potential']
        if electric_potential in ('Berry_Phase', 'homogenous_field'):
            return []
        electric_field_amplitude = kwargs['electric_field_amplitude']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    if electric_potential == 'sawtooth_potential':
        return [' eamp={0}'.format(electric_field_amplitude)]
    else:
        return []


def get_electrons_efield(name, **kwargs):
    """
    :param name:
    :param kwargs:
    :return:
    """
    try:
        electric_potential = kwargs['electric_potential']
        if electric_potential in ('Berry_Phase', 'sawtooth_potential'):
            return []
        electric_field_amplitude = kwargs['electric_field_amplitude']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    if electric_potential == 'homogenous_field':
        return [' efield={0}'.format(electric_field_amplitude)]
    else:
        return []


def get_system_edir(name, **kwargs):
    """
    :param name:
    :param kwargs:
    :return:
    """
    try:
        electric_potential = kwargs['electric_potential']
        electric_field_direction = kwargs['electric_field_direction']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    if electric_potential == 'sawtooth_potential':
        return [' edir={0}'.format(electric_field_direction)]
    else:
        return []


def get_electric_potential_related(name, **kwargs):
    try:
        electric_potential = kwargs['electric_potential']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    from .converters import to_fortran
    if name == 'tefield':
        return [' %s=%s' % (name, to_fortran(electric_potential == 'sawtooth_potential'))]
    elif name == 'lelfield':
        return [' %s=%s' % (name, to_fortran(electric_potential == 'homogenous_field'))]
    elif name == 'lberry':
        return [' %s=%s' % (name, to_fortran(electric_potential == 'Berry_Phase'))]
    return []


def get_control_gdir(name, **kwargs):
    """
    :param name:
    :param kwargs:
    :return:
    """
    try:
        electric_potential = kwargs['electric_potential']
        electric_field_direction = kwargs['electric_field_direction']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    if electric_potential in ('homogenous_field', 'Berry_Phase'):
        return [' gdir={0}'.format(electric_field_direction)]
    else:
        return []
def get_cell_dofree(name, **kwargs):
    """ 
    :param name:
    :param kwargs:
    :return:
    """
    try:
       fix_volume=kwargs['fix_volume']
    except KeyError:
       fix_volume = False
    try:
       fix_area = kwargs['fix_area']
    except KeyError:
       fix_area = False
    try:
       isotropic = kwargs['isotropic']
    except KeyError:
       isotropic = False
    cell_dofree = "cell_dofree = 'all'" 
    if ( (fix_volume and fix_area) or (fix_volume and isotropic ) or ( fix_area and isotropic)) :
        logger.error("only one of fix_volume fix_area and isotropic can be true")
        return [cell_dofree]
    if fix_volume: cell_dofree = "cell_dofree = 'shape'"
    if fix_area:   cell_dofree = "cell_dofree = '2Dshape'"
    if isotropic:  cell_dofree = "cell_dofree = 'volume' "
    return [cell_dofree]

def neb_set_system_nat(name, **kwargs):
    """
    Extract SYSTEM[nat] from the first element of the list of atomic_structure
    :param name: Variable name
    :param kwargs: list of dictionaries each containing an atomic_structure element
    :return: list containin one string to be printed in system name list nat = nat_value
    """
    images = kwargs.get('atomic_structure', [])
    if len(images) < 1:
        logger.error('No atomic_structure element found !!!')
        return  ''
    image=images[0]
    nat_value = int( image.get('nat',0) )
    if nat_value <= 0:
        logger.error("error reading nat value from atomic_structure !!!")
        return ''
    return [' nat = {0}'.format(nat_value)]

def Ha2Ry(name, **kwargs):
    import pdb
    pdb.set_trace()
    related_tag = kwargs['_related_tag']
    value = kwargs[related_tag]*2.e0
    return [' {} = {:12.8}'.format(name,value)]

