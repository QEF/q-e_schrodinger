#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato, Giovanni Borghi
#
"""
Conversion functions for Quantum Espresso cards.
"""

import logging

# from .utils import set_logger

logger = logging.getLogger('qespresso')


#
# Functions for QE cards
#
def get_atomic_species_card(name, **kwargs):
    """
    Convert XML data to ATOMIC_SPECIES card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    try:
        atomic_species = kwargs['atomic_species']
        species = atomic_species['species']
    except KeyError as err:
        logger.error("Missing required arguments when building ATOMIC_SPECIES card! %s" % err)
        return []

    lines = [name]
    try:
        lines.append(' {0} {1} {2}'.format(species['name'], species['mass'], species['pseudo_file']))
    except TypeError:
        for specie in species:
            lines.append(' {0} {1} {2}'.format(specie['name'], specie['mass'], specie['pseudo_file']))
    return lines


def get_atomic_positions_cell_card(name, **kwargs):
    """
    Convert XML data to ATOMIC_POSITIONS card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    try:
        atomic_structure = kwargs['atomic_structure']
    except KeyError:
        logger.error("Missing required arguments when building ATOMIC_POSITIONS card!")
        return []

    # Find atoms
    atomic_positions = atomic_structure.get('atomic_positions', {})
    wyckoff_positions = atomic_structure.get('wyckoff_positions', {})
    try:
        is_wyckoff = False
        if atomic_positions:
            atoms = atomic_positions['atom']
        elif wyckoff_positions:
            atoms = wyckoff_positions['atom']
            is_wyckoff = True
        else:
            atoms = []
    except KeyError:
        logger.error("Cannot find any atoms for building ATOMIC_POSITIONS!")
        return []
    else:
        if not isinstance(atoms, list):
            atoms = [atoms]

    # Check atoms with position constraints
    free_positions = kwargs.get('free_positions', [])
    if free_positions and len(free_positions) != len(atoms):
        logger.error("ATOMIC_POSITIONS: incorrect number of position constraints!")

    # Add atomic positions
    lines = ['%s %s' % (name, 'crystal_sg' if is_wyckoff else 'bohr')]
    for k in range(len(atoms)):
        name = atoms[k]['name']
        coords = ' '.join([str(value) for value in atoms[k]['_text']])
        if k < len(free_positions):
            free_pos = ' '.join([str(value) for value in free_positions[k]])
            lines.append(' %s %s %s' % (name, coords, free_pos))
        else:
            lines.append(' %s %s' % (name, coords))

    return lines


def get_atomic_constraints_card(name, **kwargs):
    """
    Convert XML data to CONSTRAINTS card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    try:
        num_of_constraints = kwargs['num_of_constraints']
        tolerance = kwargs['tolerance']
        atomic_constraints = kwargs['atomic_constraints']

    except KeyError:
        logger.error("Missing required arguments when building ATOMIC_POSITIONS card!")
        return []

    lines = [name, '{0} {1}'.format(num_of_constraints, tolerance)]
    for constraint in atomic_constraints:
        constr_parms = constraint['constr_parms']  # list with 4 float items
        constr_parms.extend([0] * max(0, 4 - len(constr_parms)))
        constr_type = constraint['constr_type']  # string
        constr_target = constraint['constr_target']  # float
        lines.append('{0} {1} {2}'.format(
            constr_type,
            ' '.join([str(item) for item in constr_parms]),
            constr_target
        ))


def get_k_points_card(name, **kwargs):
    """
    Convert XML data to K_POINTS card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    k_point = nk = monkhorst_pack= None
    gamma_only = kwargs.get('gamma_only', False)
    try:
        if not gamma_only:
            k_points_ibz = kwargs['k_points_IBZ']
            monkhorst_pack = k_points_ibz.get('monkhorst_pack', {})
            if monkhorst_pack:
                k_attrib = 'automatic'
            else:
                k_attrib = None
                k_point = k_points_ibz['k_point']
                nk = k_points_ibz['nk']
        else:
            k_attrib = 'gamma'

    except KeyError as err:
        logger.error("Missing required arguments when building K_POINTS card! %s" % err)
        return []
    else:
        if not isinstance(k_point, list):
            k_point = [k_point]

    lines = [name] if k_attrib is None else ['%s %s' % (name, k_attrib)]
    if k_attrib is None:
        lines.append(' %d' % nk)
        for point in k_point:
            lines.append(' {0} {1}'.format(
                ' '.join([str(value) for value in point['_text']]),
                point['weight'])
            )
    elif k_attrib == 'automatic':
        lines.append(' %(nk1)s %(nk2)s %(nk3)s %(k1)s %(k2)s %(k3)s' % monkhorst_pack)

    return lines


def get_atomic_forces_card(name, **kwargs):
    """
    Convert XML data to ATOMIC_FORCES card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    try:
        external_atomic_forces = kwargs['external_atomic_forces']
    except KeyError:
        logger.debug("Missing required arguments when building ATOMIC_FORCES card!")
        return []

    # Warning if number of atoms in atomic positions differ with forces
    atomic_positions = kwargs.get('atomic_positions', {})
    atoms = atomic_positions.get('atom', [])
    if atoms and len(atoms) != len(external_atomic_forces):
        logger.error("incorrect number of atomic forces")

    # Build input card text lines
    lines = [name]
    for forces in external_atomic_forces:
        lines.append(' {0}'.format(' '.join([str(value) for value in forces])))
    return lines


def get_cell_parameters_card(name, **kwargs):
    """
    Convert XML data to CELL_PARAMETERS card

    :param name: Card name
    :param kwargs: Dictionary with converted data from XML file
    :return: List of strings
    """
    try:
        atomic_structure = kwargs['atomic_structure']
    except KeyError:
        logger.error("Missing required arguments when building ATOMIC_POSITIONS card!")
        return []

    # Add cell parameters card
    cells = atomic_structure.get('cell', {})
    if cells:
        lines = ['%s bohr' % name]
        for key in sorted(cells):
            if key not in ['a1', 'a2', 'a3']:
                continue
            lines.append(' %s' % ' '.join([str(value) for value in cells[key]]))
        return lines
    return []


#
# Phonon Card
#
def get_qpoints_card(name, **kwargs):
    """
    :param name:
    :param kwargs:
    :return:
    """
    try:
        ldisp = kwargs['ldisp']
        qplot = kwargs['qplot']
        if not qplot and ldisp:
            return []
        q_points_list = kwargs['q_points_list']['q_point']
    except KeyError as err:
        logger.error("Missing required arguments when building "
                     "parameter '%s'! %s" % (name, err))
        return []

    lines = []
    if not ldisp and not qplot:
        for q_point in q_points_list:
            lines.append(' '.join([str(coord) for coord in q_point['_text']]))

    elif qplot:
        nqs = kwargs['nqs']
        lines.append(' {0}'.format(nqs))
        for q_point in q_points_list:
            vector = ' '.join([str(coord) for coord in q_point['_text']])
            lines.append(' %s %s' % (vector, q_point['weight']))

    return lines
