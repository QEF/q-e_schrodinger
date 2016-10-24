#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato, Giovanni Borghi
#
"""
Test classes for Quantum Espresso input converters.
"""
import unittest


class InputConversionTestCase(unittest.TestCase):
    longMessage = True


def make_test_function(xml_file, ref_in_file):
    def test(self):
        xml_conf = qespresso.PwDocument()
        xml_conf.read(xml_file)
        qe_input = xml_conf.get_qe_input().split('\n')
        with open(ref_in_file, 'r') as qe_input_file:
            k = 0
            are_equals = True
            for ref_line in qe_input_file:
                ref_line = ref_line.rstrip('\n').strip(' \t')
                in_line = qe_input[k].strip(' \t')
                if ref_line != in_line:
                    print("Unmatched lines: '%s' != '%s'" % (in_line, ref_line))
                    are_equals = False
                    break
                else:
                    k += 1
        self.assertTrue(are_equals, xml_file)
    return test


if __name__ == '__main__':
    import glob
    import os
    import sys

    if os.path.dirname(__file__):
        rel_path = os.path.dirname(os.path.dirname(__file__))
        pkg_folder = os.path.realpath(rel_path)
    else:
        rel_path = ".."
        pkg_folder = os.path.realpath(rel_path)

    sys.path.insert(0, pkg_folder)
    import qespresso

    test_files = glob.glob(os.path.join(rel_path, "examples/*.xml"))
    for xml_filename in test_files:
        qe_input_filename = '%s.in' % xml_filename[:-4]
        if not os.path.isfile(qe_input_filename):
            continue
        test_func = make_test_function(xml_filename, qe_input_filename)
        test_name = os.path.basename(xml_filename[:-4])
        klassname = 'Test_{0}'.format(test_name)
        globals()[klassname] = type(
            klassname, (InputConversionTestCase,),
            {'test_gen_{0}'.format(test_name): test_func}
        )
    unittest.main()
