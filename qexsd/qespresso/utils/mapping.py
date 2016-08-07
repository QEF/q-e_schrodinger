#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato
#
"""
Useful classes for building mapping structures.
"""

from collections import MutableMapping


class BiunivocalMap(MutableMapping):
    """
    A dictionary that implements a bijective correspondence, namely with constraints
    of uniqueness both on keys that on values.
    """
    def __init__(self, *args, **kwargs):
        self.__map = {}
        self.__inverse = {}
        self.update(*args, **kwargs)

    def __getitem__(self, key):
        if key in self.__map:
            return self.__map[key]
        if hasattr(self.__class__, '__missing__'):
            return getattr(self.__class__, '__missing__')(self, key)
        raise KeyError(key)

    def __setitem__(self, key, item):
        try:
            if self.__inverse[item] != key:
                raise ValueError("Value '{0}' is already mapped by another key!".format(item))
        except KeyError:
            if key in self.__map:
                del self.__inverse[self.__map[key]]
            self.__map[key] = item
            self.__inverse[item] = key
        else:
            del self.__inverse[self.__map[key]]
            self.__map[key] = item
            self.__inverse[item] = key

    def __delitem__(self, key):
        del self.__inverse[self.__map[key]]
        del self.__map[key]

    def __iter__(self):
        return iter(self.__map)

    def __len__(self):
        return len(self.__map)

    def __contains__(self, key):
        return key in self.__map

    def __repr__(self):
        return repr(self.__map)

    def copy(self):
        if self.__class__ is BiunivocalMap:
            return BiunivocalMap(self.__map.copy())
        import copy
        __map = self.__map
        try:
            self.__map = {}
            c = copy.copy(self)
        finally:
            self.__map = __map
        c.update(self)
        return c

    @classmethod
    def fromkeys(cls, iterable, value=None):
        d = cls()
        for key in iterable:
            d[key] = value
        return d

    def getkey(self, value, default=None):
        """
        If value is in dictionary's values, return the key correspondent
        to the value, else return None.

        :param value: Value to map
        :param default: Default to return if the value is not in the map values
        """
        try:
            return self.__inverse[value]
        except KeyError:
            return default

    def inverse(self):
        """Return a copy of the inverse dictionary."""
        return self.__inverse.copy()
