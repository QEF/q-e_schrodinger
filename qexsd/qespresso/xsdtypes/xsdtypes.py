# -*- coding: utf-8 -*-
#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato
#
"""
This module contains XSD types classes for xsdtypes package
"""

import logging
import datetime

from collections import OrderedDict
from decimal import Decimal
from .exceptions import XMLSchemaValidationError, XMLDecodeError, XMLEncodeError, XMLValueError

logger = logging.getLogger('qespresso')


def is_datetime(date_string, date_format='%Y-%m-%d'):
    """
    Check if the string represents a valid datetime according to the specified formatting.

    :param date_string: The string containing the datetime
    :param date_format: The reference formatting for datetime
    :return: True if the string is a valid datetime, False if not.
    """
    try:
        datetime.datetime.strptime(date_string, date_format)
    except ValueError:
        return False
    else:
        return True

#
# XSD built-in types definition
#
XSD_NAMESPACE_PATH = 'http://www.w3.org/2001/XMLSchema'
XSI_NAMESPACE_PATH = 'http://www.w3.org/2001/XMLSchema-instance'

# XSD declaration tags
_SIMPLE_TYPE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'simpleType')
_COMPLEX_TYPE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'complexType')
_ATTRIBUTE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'attribute')
_ELEMENT_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'element')
_RESTRICTION_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'restriction')
_EXTENSION_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'extension')
_LIST_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'list')
_SEQUENCE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'sequence')             # Ordered sequence of elements
_ALL_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'all')                       # Unordered sequence of elements
_CHOICE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'choice')                 # One element in a group
_SIMPLE_CONTENT_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'simpleContent')
_COMPLEX_CONTENT_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'complexContent')
_GROUP_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'group')
_ATTRIBUTE_GROUP_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'attributeGroup')

_ENUMERATION_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'enumeration')
_LENGTH_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'length')
_MIN_LENGTH_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'minLength')
_MAX_LENGTH_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'maxLength')
_PATTERN_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'pattern')
_WHITE_SPACE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'whiteSpace')
_MAX_INCLUSIVE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'maxInclusive')
_MAX_EXCLUSIVE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'maxExclusive')
_MIN_INCLUSIVE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'minInclusive')
_MIN_EXCLUSIVE_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'minExclusive')
_TOTAL_DIGITS_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'totalDigits')
_FRACTION_DIGITS_TAG = '{%s}%s' % (XSD_NAMESPACE_PATH, 'fractionDigits')


_XSD_BUILTIN_TYPES_MAP = {
    # Builtin type for empty elements
    'anyType': (str, lambda x: not x),

    # --- String Types ---
    'string': str,	                            # character string
    # normalized string	line breaks are normalized
    # token	whitespace is normalized
    # NMTOKEN	should not contain whitespace (attribute only)
    # NMTOKENS	List of NMTOKENs (attribute only)
    # Name	not starting with a digit
    # NCName	cannot contain colons
    # ID	unique identification in document (attribute only)
    # IDREF	reference to ID field in document (attribute only)
    # IDREFS	list of references to ID fields in document (attribute only)
    # ENTITY	reference to entity (attribute only)
    # ENTITIES	list of references to entities (attribute only)
    # language	language codes

    # --- Numerical Types ---
    'byte': (int, [lambda x: -2**7 <= x < 2**7]),        # signed 8 bit value
    'decimal': Decimal,                                  # decimal number
    'double': float,                                     # 64 bit floating point
    'float': float,	                                     # 32 bit floating point
    'int': (int, [lambda x: -2**63 <= x < 2**63]),	     # signed 64 bit value
    'integer': int,	                                     # any integer value
    'long': (int, [lambda x: -2**127 <= x < 2**127]),	 # signed 128 bit value
    'negativeInteger': (int, [lambda x: x < 0]),         # only negative value allowed [< 0]
    'positiveInteger': (int, [lambda x: x > 0]),         # only positive value allowed [> 0]
    'nonPositiveInteger': (int, [lambda x: x <= 0]),     # only zero and smaller value allowed [<= 0]
    'nonNegativeInteger': (int, [lambda x: x >= 0]),     # only zero and more value allowed [>= 0]
    'short': (int, [lambda x: -2**16 <= x < 2**16]),     # signed 32 bit value
    'unsignedByte': (int, [lambda x: 0 <= x < 2**8]),    # unsigned 8 bit value
    'unsignedInt': (int, [lambda x: 0 <= x < 2**64]),    # unsigned 64 bit value
    'unsignedLong': (int, [lambda x: 0 <= x < 2**128]),  # unsigned 128 bit value
    'unsignedShort': (int, [lambda x: 0 <= x < 2**32]),  # unsigned 32 bit value

    # ---Dates and Times---
    'date': (str, [lambda x: is_datetime(x, '')]),                  # CCYY-MM-DD
    'dateTime': (str, [lambda x: is_datetime(x, '')]),              # CCYY-MM-DDThh:mm:ss
    'gDay': (str, [lambda x: len(x) == 2 and 0 < int(x) <= 31]),    # DD
    'gMonth': (str, [lambda x: len(x) == 2 and 0 < int(x) <= 12]),  # MM
    'gMonthDay': (str, [lambda x: is_datetime(x, '')]),             # MM-DD
    'gYear': (str, [lambda x: is_datetime(x, '')]),    	            # CCYY
    'gYearMonth': (str, [lambda x: is_datetime(x, '')]),            # CCYY-MM
    'time': (str, [lambda x: is_datetime(x, '')]),                  # hh:mm:ss
    'duration': str,                                                # PnYnMnDTnHnMnS

    # Other types
    'QName': str,                       # prf:name (the prefix needs to be qualified with an in scope namespace)
    'anyURI': str, 	                    # absolute or relative uri (RFC 2396)
    'boolean': (
        bool, None,
        lambda x: True if x in ('true', '1') else False if x in ('false', '0') else None,
        lambda x: str(x).lower()
    )                                   # true/false or 1/0
    # 'base64Binary': int,              # base64 encoded binary value
    # 'hexBinary': int                  # hexadecimal encoded binary value
}
"""Definition map of XSD predefined types"""


class XSDType(object):
    """
    Base class for representing generic XSD types. Use this class
    for create XSD built-in types.

    An instance contains a Python's type transformation ana a list
    of validator functions.

    Use Python's types conversion:

        Decoding from XML: python_type(value)
        Encoding to XML: str(value)
    """
    def __init__(self, name, python_type, validators=None, to_python=None, from_python=str):
        """
        :param python_type: The correspondent Python's type
        :param validators: The optional validator for value objects
        :param to_python: The optional decode function
        :param from_python: The optional encode function
        """
        if not callable(python_type):
            raise TypeError("%s object is not callable" % python_type.__class__.__name__)
        self.name = name
        self.python_type = python_type
        self.validators = validators if validators is not None else []
        self.to_python = python_type if to_python is None else to_python
        self.from_python = from_python

    def is_list(self):
        return False

    def validate(self, value):
        """
        Validator for decoded values.
        :param value: The Python's object that has to be validated
        """
        if not isinstance(value, self.python_type):
            raise TypeError("value type is '{0}' instead of '{1}'"
                            .format(type(value), repr(self.python_type)))
        if not all([validator(value) for validator in self.validators]):
            raise XMLValueError(value, self.name)

    def decode(self, text):
        """
        Transform an XML text into a Python object.
        :param text: XML text
        """
        if not isinstance(text, str):
            raise TypeError("argument must be a string!")
        try:
            value = self.to_python(text)
        except ValueError:
            raise XMLDecodeError("cannot decode '%s' to type: %s" % (text, repr(self.python_type)))

        self.validate(value)
        return value

    def encode(self, obj):
        """
        Transform a Python object into an XML string.
        :param obj: The Python object that has to be encoded in XML
        """
        if not isinstance(obj, self.python_type):
            raise XMLEncodeError(obj, self.python_type)
        return self.from_python(obj)

    def basename(self):
        """
        Get the name of the type without prefix or namespace URI.
        """
        return self.name.rsplit(":")[-1].rsplit("}")[-1]

    def get_rank(self):
        """
        The rank of the XSDType-only instance (an XSD builtin type) is always 0.
        """
        return 0

    def get_lengths(self, only_lists=True):
        return [] if only_lists else [None]


class XSDSimpleType(XSDType):
    """
    A class for represent XSD schema simple type.
    """
    def __init__(self, base_type, elem, name=None, is_list=False, length=None, validators=None, enumeration=None):
        if not isinstance(base_type, XSDType):
            raise TypeError("argument 'base_type' must be an XSD type instance: {0}".format(base_type))
        super(XSDSimpleType, self).__init__(name, getattr(base_type, 'python_type'), validators)
        self.elem = elem
        self.base_type = base_type
        self._is_list = is_list
        self.length = length
        self.enumeration = [
            self.base_type.decode(value) for value in enumeration
        ] if enumeration else []

    def is_list(self):
        return self._is_list

    def validate(self, value):
        if isinstance(value, list):
            if self._is_list:
                for item in value:
                    self.validate(item)
            else:
                self.base_type.validate(value)
        else:
            super(XSDSimpleType, self).validate(value)
            if self.enumeration and value not in self.enumeration:
                raise XMLValueError(value, self.name)

    def decode(self, text):
        if not isinstance(text, str):
            raise TypeError("argument must be a string!")

        if self._is_list:
            matrix = [item.strip() for item in text.split('\n') if item.strip()]
            if len(matrix) == 1:
                # Only one data line --> decode to simple list
                return [
                    self.base_type.decode(item) for item in matrix[0].split()
                ]
            else:
                # More data lines --> decode to nested lists
                return [
                    [XSDType.decode(self.base_type, item) for item in matrix[row].split()]
                    for row in range(len(matrix))
                ]

        value = self.base_type.decode(text)
        self.validate(value)
        return value

    def encode(self, obj):
        if self._is_list:
            if not isinstance(obj, list):
                raise TypeError("argument must be a list!")
            return u' '.join([self.base_type.encode(item) for item in obj])
        return self.base_type.encode(obj)

    def get_rank(self):
        """
        Compute the pseudo-dimension of the XSDSimpleType,
        considering the dimension of the base type.

        :return: int
        """
        base_type_rank = self.base_type.get_rank()
        return base_type_rank + 1 if self._is_list else base_type_rank

    def get_lengths(self, only_lists=True):
        """
        Get the lengths defined for the XSD type.

        :param only_lists: Include lengths defined for other types.
        :return: a list with defined lengths
        """
        lengths = self.base_type.get_lengths()
        if self._is_list:
            lengths.append(self.length)
        elif self.base_type.is_list() and self.length is not None:
            lengths[-1] = self.length
        elif not only_lists:
            lengths.append(self.length)
        return lengths


class XSDComplexType(XSDSimpleType):
    """
    A class for represent XSD schema simple type.
    """
    def __init__(self, base_type, elem, name=None, is_list=False, validators=None,
                 enumeration=None, content_model=None, attributes=None):
        super(XSDComplexType, self).__init__(base_type, elem, name, is_list, validators, enumeration)
        self.content_model = content_model or XSDGroup()
        self.attributes = attributes or OrderedDict()


class XSDElement(object):
    """
    Support structure to associate an element and its attributes with XSD simple types.
    """
    def __init__(self, name, path, xsd_type, attrib=None):
        self.name = name
        """The name is the element tag"""

        self.path = path
        """The XPath of this element"""

        self.xsd_type = xsd_type
        """XSD type of the element"""

        self.attrib = attrib or {}
        """Attributes of element declaration (eg. minOccurs, default ...)"""

    def basename(self):
        """
        Get the name of the element without prefix or namespace URI.
        """
        return self.name.rsplit(":")[-1].rsplit("}")[-1]

    def get_type(self):
        return self.xsd_type

    def is_optional(self):
        return 'minOccurs' in self.attrib and self.attrib['minOccurs'] == "0"

    def get_default(self):
        try:
            return self.attrib['default']
        except KeyError:
            return None

    def get_min_occurs(self):
        if 'minOccurs' in self.attrib:
            return int(self.attrib['minOccurs'])
        else:
            return 1

    def get_max_occurs(self):
        if 'maxOccurs' in self.attrib:
            if self.attrib['maxOccurs'] == 'unbounded':
                return None
            else:
                return int(self.attrib['maxOccurs'])
        else:
            return 1


class XSDAttribute(object):
    """
    Support structure to associate an attribute with XSD simple types.

    """
    def __init__(self, xsd_type, attrib=None):
        self.xsd_type = xsd_type
        """XSD type of the attribute"""

        self.attrib = attrib or {}
        """Attributes of attribute declaration (eg. use, default, ...)"""

    def get_type(self):
        return self.xsd_type

    def is_optional(self):
        return 'use' in self.attrib and self.attrib['use'] == 'optional'

    def get_default(self):
        try:
            return self.attrib['default']
        except KeyError:
            return None


class XSDGroup(list):

    def __init__(self, name=None, group_type=None, attrib=None, *args, **kwargs):
        self.name = name
        self.group_type = group_type
        self.attrib = attrib or {}
        """Attributes of element declaration (eg. minOccurs, default ...)"""
        super(XSDGroup, self).__init__(*args, **kwargs)

    def get_min_occurs(self):
        if 'minOccurs' in self.attrib:
            return int(self.attrib['minOccurs'])
        else:
            return 1

    def get_max_occurs(self):
        if 'maxOccurs' in self.attrib:
            if self.attrib['maxOccurs'] == 'unbounded':
                return None
            else:
                return int(self.attrib['maxOccurs'])
        else:
            return 1


#
# Factory functions for XSD types
def xsd_builtin_types_factory(xsd_type_class=XSDType, *args, **kwargs):
    """
    Build a dictionary for XSD builtin types mapping.

    :param xsd_type_class: XSDType class for instances.
    :param args: List of tuples containing a single definition.
    :param kwargs: A dictionary with multiple built-in type declarations.
    :return: Dictionary that map a type name to a decoding type class instance
    """
    xsd_types = dict()
    if kwargs:
        for type_name, type_def in kwargs.items():
            if isinstance(type_def, tuple):
                xsd_types[type_name] = xsd_type_class(type_name, *type_def)
            else:
                xsd_types[type_name] = xsd_type_class(type_name, type_def)
    if args:
        if len(args) == 1:
            xsd_types[args[0]] = xsd_type_class(str)
        else:
            xsd_types[args[0]] = xsd_type_class(*args)

    return xsd_types

XSD_BUILTIN_TYPES = xsd_builtin_types_factory(XSDType, **_XSD_BUILTIN_TYPES_MAP)
"""Dictionary for XSD built-in types mapping. The values are XSDType instances"""

XSI_ATTRIBUTES = {
    '{%s}schemaLocation' % XSI_NAMESPACE_PATH: XSDAttribute(XSD_BUILTIN_TYPES['anyURI']),
    '{%s}noNamespaceSchemaLocation' % XSI_NAMESPACE_PATH: XSDAttribute(XSD_BUILTIN_TYPES['anyURI'])
}
"""XSI attributes for schema instances"""


def xsd_simple_type_factory(elem, **kwargs):
    """
    Factory function for XSD simple type declaration.

    :param elem: The ElementTree's Element. It must be
    a simpleType declaration element.
    :param kwargs: Various lookup tables and parameters,
    dependant by the context where the function is called.
    :return: a couple with the type_name, that maybe an empty string,
    and the XSDSimpleType instance that represents the XSD declaration.
    """
    logger.debug("xsd_simple_type_factory: elem.attrib={0}, kwargs={1}"
                 .format(elem.attrib, kwargs.keys()))
    if elem.tag != _SIMPLE_TYPE_TAG:
        raise ValueError("a 'simpleType' element is needed: {0}".format(elem))

    # Get used parameters from the named arguments
    prefix = kwargs.get('prefix', '')
    xsd_types = kwargs.get('xsd_types', {})
    xsd_type_class = kwargs.get('xsd_type_class', XSDSimpleType)

    base_type = None
    is_list = False
    length = None
    min_length = None
    max_length = None
    validators = []
    enumeration = []

    try:
        type_name = '%s%s' % (prefix, elem.attrib['name'])
        logger.debug("Parse global simpleType '{0}': {1}".format(type_name, elem))
    except KeyError:
        type_name = None
        logger.debug("Parse local simpleType: {0}".format(elem))

    logger.debug("Children: {0}".format(list(elem)))
    child = elem[0]
    if child.tag == _RESTRICTION_TAG:
        # Case 1: 'restriction' tag child
        logger.debug("Found 'restriction' tag: %s" % child.tag)
        if child.attrib and 'base' in child.attrib:
            base_type = xsd_get_type(child.attrib['base'], xsd_types)
            logger.debug("Associated to base type '{0}': {1}".format(base_type, base_type.name))

        for node in child:
            logger.debug("Parse restriction child: {0}".format(node))
            if node.tag == _SIMPLE_TYPE_TAG:
                # Case 1.1: simpleType declaration inside untyped restriction
                if base_type is not None:
                    raise XMLSchemaValidationError("unexeptected simpleType element: {0}".format(node))
                _, base_type = xsd_simple_type_factory(node, **kwargs)
            elif node.tag == _ENUMERATION_TAG:
                # Case 1.2: restriction by possible values enumeration
                enumeration.append(node.attrib['value'])
                logger.debug("Added enumeration: %s" % node.attrib['value'])
            elif node.tag == _LENGTH_TAG:
                # Case 1.3: 'length' restriction
                length = int(node.attrib['value'])
                validators.append(eval('lambda x: len(x) == %s' % length))
                logger.debug("Added a 'length == %s' restriction" % length)
            elif node.tag == _MIN_LENGTH_TAG:
                # Case 1.4: 'minLength' restriction
                min_length = int(node.attrib['value'])
                validators.append(eval('lambda x: len(x) >= %s' % length))
                logger.debug("Added a 'minLength == %s' restriction" % length)
            elif node.tag == _MAX_LENGTH_TAG:
                # Case 1.5: 'maxLength' restriction
                max_length = int(node.attrib['value'])
                validators.append(eval('lambda x: len(x) <= %s' % length))
                logger.debug("Added a 'maxLength == %s' restriction" % length)
            else:
                # TODO: other restriction types --> validators[] as the previous
                logger.debug("Skipped '%s' restriction" % node.tag)

    elif child.tag == _LIST_TAG:
        # Case 2: 'list' tag child
        is_list = True
        logger.debug("Found 'list' tag")
        if child.attrib and 'itemType' in child.attrib:
            # Case 2.1: List tag with itemType attribute
            base_type = xsd_get_type(child.attrib['itemType'], xsd_get_type)
            logger.debug("Associated to base type '{0}': {1}".format(base_type, base_type.name))
        elif child.tag == _SIMPLE_TYPE_TAG:
            # Case 2.2: simpleType declaration inside untyped list
            _, base_type = xsd_simple_type_factory(child[0], **kwargs)

    if base_type is None:
        raise XMLSchemaValidationError("Missing base type for type element: {0}".format(elem))

    if length is not None and (min_length is not None or max_length is not None):
        raise XMLSchemaValidationError(
            "It is an error for both 'length' and either of 'minLength' or 'maxLength' "
            "to be specified on the same type definition: {0}".format(elem)
        )
    elif min_length is not None or max_length is not None:
        try:
            if int(min_length) > int(max_length):
                raise XMLSchemaValidationError(
                    "'minLength' must be less or equal of 'maxLength': {0}".format(elem)
                )
            elif int(min_length) == int(max_length):
                length = min_length
            else:
                length = tuple([min_length, max_length])
        except TypeError:
            length = tuple([min_length, max_length])

    if type_name in xsd_types:
        # complexType object already exist, return only a reference to it
        return type_name, xsd_types[type_name]

    logger.debug("Create instance for simple type '{0}' based on '{1}'".format(type_name, base_type))
    return type_name, xsd_type_class(base_type, elem, type_name, is_list, length, validators, enumeration)


def xsd_attribute_type_factory(elem, **kwargs):
    """
    Factory function for XSD attribute declaration.

    :param elem: The ElementTree's Element. It must be
    a attribute declaration element.
    :param kwargs: Various lookup tables and parameters,
    dependant by the context where the function is called.
    :return: a couple with the attribute name and the
    instance of XSDType associated with it.
    """
    logger.debug("xsd_attribute_type_factory: elem.attrib={0}, kwargs={1}"
                 .format(elem.attrib, kwargs.keys()))
    if elem.tag != _ATTRIBUTE_TAG:
        raise ValueError("an 'attribute' element is needed: {0}".format(elem))

    # Get used parameters from the named arguments
    xsd_types = kwargs.get('xsd_types', {})
    xsd_attributes = kwargs.get('xsd_attributes', {})
    attribute_name = elem.attrib.get('name', '')

    if attribute_name == '':
        # No name attribute in XSD attribute declaration
        attribute_name = elem.attrib.get('ref', '')
        if attribute_name == '':
            # Missing also 'name' and 'ref' attributes
            raise XMLSchemaValidationError(
                "invalid attribute declaration in XSD schema: {0}".format(elem)
            )
        try:
            # Reference found: associate the global attribute declaration
            xsd_attribute = xsd_attributes[attribute_name]
            logger.info("Refer to global attribute '{0}'".format(elem.ref))
            return attribute_name, xsd_attribute
        except KeyError:
            raise XMLSchemaValidationError("missing global attribute: {0}".format(elem))

    try:
        # Named attribute: try to map to a XSD builtin type or global simpleType
        xsd_type = xsd_get_type(elem.attrib['type'], xsd_types)
        logger.debug("Attribute '{0}' mapped to global simple type '{1}'".
                     format(attribute_name, xsd_type.name))
        return attribute_name, XSDAttribute(xsd_type, dict(elem.attrib))
    except KeyError:
        # No 'type' attribute in declaration, parse for child local simpleType
        try:
            _, xsd_type = xsd_simple_type_factory(elem[0], **kwargs)
        except ValueError:
            raise XMLSchemaValidationError("simpleType declaration expected")
        logger.debug("Attribute '{0}' mapped to a local simple type based on '{1}'".
                     format(attribute_name, xsd_type.base_type))
        return attribute_name, XSDAttribute(xsd_type, dict(elem.attrib))
    except ValueError:
        # The attribute 'type' is in the declaration but the XSD type is not found
        raise XMLSchemaValidationError("missing type declaration: %s" % elem.attrib['type'])


def xsd_complex_type_factory(elem, **kwargs):
    """
    Factory function for XSD complex type declaration.

    :param elem: The ElementTree's Element. It must be
    a complexType declaration element.
    :param kwargs: Various lookup tables and parameters,
    dependant by the context where the function is called.
    :return: a couple with the type_name, that maybe an empty string,
    and the XSDComplexType instance that represents the XSD declaration.
    """
    logger.debug("xsd_complex_type_factory: elem.attrib={0}, kwargs={1}"
                 .format(elem.attrib, kwargs.keys()))
    if elem.tag != _COMPLEX_TYPE_TAG:
        raise ValueError("a 'complexType' element is needed: {0}".format(elem))

    # Get used parameters from the named arguments
    prefix = kwargs.get('prefix', '')
    xsd_types = kwargs.get('xsd_types', {})
    xsd_type_class = kwargs.get('xsd_type_class', XSDComplexType)

    base_type = None
    is_list = False
    attributes = OrderedDict()

    try:
        type_name = '%s%s' % (prefix, elem.attrib['name'])
        logger.debug("Parse global complexType '{0}': {1}".format(type_name, elem))
    except KeyError:
        type_name = None
        logger.debug("Parse local complexType: {0}".format(elem))

    logger.debug("Children: {0}".format(list(elem)))
    content_node = elem[0]

    try:
        xsd_type = xsd_types[type_name]
    except KeyError:
        content_model = None
    else:
        content_model = xsd_type.content_model
        if content_model:
            logger.error("XSD type content model is already defined!")

    # The caller is xds_element_factory --> add sub-elements
    if 'parent_path' in kwargs and content_node.tag in (_GROUP_TAG, _SEQUENCE_TAG, _ALL_TAG, _CHOICE_TAG):
        logger.debug("Call from xsd_element_factory: parse content_model declarations!")
        content_model = xsd_group_factory(content_node, **kwargs)

    if type_name in xsd_types:
        logger.debug("Return existing complex type '{0}'".format(type_name))
        return type_name, xsd_types[type_name]

    if content_node.tag in (_GROUP_TAG, _SEQUENCE_TAG, _ALL_TAG, _CHOICE_TAG):
        # Found a section containing group declarations
        # is_list = content_model.tag == _SEQUENCE_TAG
        base_type = xsd_get_type('string', xsd_types)
    elif content_node.tag in (_SIMPLE_CONTENT_TAG, _COMPLEX_CONTENT_TAG):
        # Found content declaration
        content_spec = content_node[0]
        try:
            base_type = xsd_get_type(content_spec.attrib['base'], xsd_types)
        except KeyError:
            # 'base' attribute missing: parse local type declaration
            _, base_type = xsd_simple_type_factory(content_spec.child[0], **kwargs)
        if content_spec.tag == _RESTRICTION_TAG:
            logger.debug("Found restriction tag")
            # TODO: add restrictions on base_type
            for child in content_spec:
                if child.tag == _ATTRIBUTE_TAG:
                    attributes.update([xsd_attribute_type_factory(child, **kwargs)])
        elif content_spec.tag == _EXTENSION_TAG:
            logger.debug("Found extension tag")
            # TODO: add extensions on base_type
            for child in content_spec:
                if child.tag == _ATTRIBUTE_TAG:
                    attributes.update([xsd_attribute_type_factory(child, **kwargs)])
    elif content_node.tag == _ATTRIBUTE_TAG:
        # Found attribute declaration
        attributes.update([xsd_attribute_type_factory(content_node, **kwargs)])

    # Add other attribute declarations
    for attribute in elem[1:]:
        attributes.update([xsd_attribute_type_factory(attribute, **kwargs)])

    if base_type is None:
        raise XMLSchemaValidationError("error in XSD schema: {0}".format(elem))

    logger.debug("Create instance for complex type '{0}' based on '{1}'".format(type_name, base_type))
    return type_name, xsd_type_class(
        base_type, elem, type_name, is_list, content_model=content_model, attributes=attributes
    )


def xsd_group_factory(elem, **kwargs):
    logger.debug("xsd_group_factory: elem.attrib={0}, kwargs={1}"
                 .format(elem.attrib, kwargs.keys()))

    if elem.tag == _GROUP_TAG:
        name = elem.attrib.get('name')
        ref = elem.attrib.get('ref')
        if not name and not ref:
            raise XMLSchemaValidationError("Missing both attributes 'name' and 'ref' in element: {}".format(elem))
        elif name and ref:
            raise XMLSchemaValidationError("Found both attributes 'name' and 'ref' in element: {}".format(elem))
        elif ref:
            xsd_groups = kwargs['xsd_groups']
            try:
                return xsd_groups[ref]
            except KeyError:
                raise XMLSchemaValidationError("Missing XSD group '{}'".format(ref))

        content_model = elem[0]
    else:
        content_model = elem
        name = None

    if content_model.tag not in (_SEQUENCE_TAG, _ALL_TAG, _CHOICE_TAG):
        raise ValueError("a sequence/all/choice element is needed: {0}".format(content_model))

    group = XSDGroup(name, content_model.tag, elem.attrib)
    for child in content_model:
        if child.tag == _ELEMENT_TAG:
            group.append(xsd_element_factory(child, **kwargs))
        elif child.tag == _ALL_TAG:
            raise XMLSchemaValidationError("'all' content type not allowed here: {}".format(elem))
        elif child.tag in (_SEQUENCE_TAG, _CHOICE_TAG):
            group.append(xsd_group_factory(child, **kwargs))
    return group


def xsd_element_factory(elem, xsd_element=None, **kwargs):
    """
    Factory function for XSD element declaration. The function
    walks through complexTypes to find declarations of descendant
    XSD elements.

    :param elem: The ElementTree's Element. It must be
    an element declaration element.
    :param xsd_element: The complex type, if it's already created.
    :param kwargs: Various lookup tables and parameters,
    dependant by the context where the function is called.
    An xsd_elements dictionary is needed as the destination
    of the XSD element declarations.
    """
    logger.debug("xsd_element_factory: elem.attrib={0}, kwargs={1}"
                 .format(elem.attrib, kwargs.keys()))
    if elem.tag != _ELEMENT_TAG:
        raise ValueError("an 'element' element is needed: {0}".format(elem))

    # Get used parameters from the named arguments
    prefix = kwargs.get('prefix', '')
    parent_path = kwargs.get('parent_path')
    xsd_types = kwargs.get('xsd_types', {})
    xsd_elements = kwargs.get('xsd_elements')
    xsd_element_class = kwargs.get('xsd_type_class', XSDElement)

    element_name = '%s%s' % (prefix, elem.attrib['name'])
    element_type = None
    element_attributes = elem.attrib

    # Get and update the parent path. If None then this is
    # the root node that has a path equal to '.'.
    if parent_path is None:
        element_path = '.'
    else:
        element_path = '/'.join((parent_path, elem.attrib['name']))
    kwargs['parent_path'] = element_path

    if element_name != prefix:
        logger.info("Parse global element '{0}': {1}".format(element_name, elem))
    else:
        logger.info("Parse local element: {0}".format(elem))
    logger.debug("Children: {0}".format(list(elem)))

    if 'type' in elem.attrib:
        element_type = xsd_get_type(elem.attrib['type'], xsd_types)
        if isinstance(element_type, XSDComplexType):
            _, element_type = xsd_complex_type_factory(
                    element_type.elem, **kwargs)
    elif 'ref' in elem.attrib:
        raise ValueError
    elif elem[0].tag == _COMPLEX_TYPE_TAG:
        _, element_type = xsd_complex_type_factory(elem, **kwargs)
    elif elem[0].tag == _SIMPLE_TYPE_TAG:
        _, element_type = xsd_simple_type_factory(elem, **kwargs)

    if element_type is None:
        raise XMLSchemaValidationError("error in XSD schema!")

    if xsd_element is None:
        logger.debug("Add element '{0}' with path '{1}'".format(element_name, element_path))
        xsd_element = xsd_element_class(element_name, element_path, element_type, element_attributes)
    else:
        logger.debug("Link path '{0}' to existing element '{1}' ".format(element_path, element_name))

    try:
        xsd_elements[element_path] = xsd_element
    except TypeError:
        pass
    return xsd_element


def add_xsd_types_to_dict(declarations, target_dict, factory_function, **kwargs):
    """
    Function to add XSD global types declarations. Unresolved
    references to other types in the list are retried until
    are completed.

    :param declarations: Sequence of XSD simpleType/complexType
    declarations.
    :param target_dict: Dictionary for type declarations.
    :param factory_function: Factory function to use.
    :param kwargs: Various arguments to pass to the factory function.
    """
    logger.debug("Type declarations to add: {0}".format(len(declarations)))
    prefix = kwargs.get('prefix', '')
    counter = 0
    while declarations:
        missing = list()
        for elem in declarations:
            try:
                name = '%s%s' % (prefix, elem.attrib['name'])
                if name in target_dict:
                    logger.debug("Skip already inserted element '%s'" % name)
                    continue
                target_dict.update([factory_function(elem, **kwargs)])
                counter += 1
            except ValueError:
                missing.append(elem)
        if len(declarations) == len(missing):
            raise XMLSchemaValidationError(
                    "missing global declarations in XML schema: {0}".format(missing)
            )
        declarations = list(missing)
        if missing:
            logger.debug("Retry for %d missing reference declarations" % len(missing))
    logger.debug("Type declarations added: %d" % counter)


def xsd_get_type(type_name, xsd_types):
    """
    Lookup for a XSD type in XSD predefined built-in types map and
    in XML Schema types map.

    :param type_name: XSD type name
    :param xsd_types: Map for XSD simple types
    :return: BaseXSDType/XSDSimpleType instance
    """
    try:
        namespace, _ = type_name.split(':', 1)
    except ValueError:
        namespace = 'xsd'
    except AttributeError:
        raise TypeError("the argument must be a string!")

    try:
        if namespace in ('xs', 'xsd'):
            return XSD_BUILTIN_TYPES[type_name]
        else:
            return xsd_types[type_name]
    except KeyError:
        raise ValueError("type name '%s' not found!" % type_name)


__all__ = ('XSD_NAMESPACE_PATH', 'XSI_NAMESPACE_PATH', 'XSI_ATTRIBUTES',
           'XSDType', 'XSDSimpleType', 'XSDComplexType', 'XSDElement',
           'xsd_simple_type_factory', 'xsd_attribute_type_factory',
           'xsd_complex_type_factory', 'xsd_element_factory',
           'XSD_BUILTIN_TYPES', 'xsd_get_type', 'add_xsd_types_to_dict')
