#!/usr/bin/python
# -*- coding: utf-8 -*-
import sqlite3

if __name__ == "__main__":

    params = 'gap_i, gap_ts, ROUND(gap_cr, 2), ROUND(gap_mr, 2), ga_kf'

    for metric in ['db','turi']:
        testsfile = open('second_tests_' + metric + '.test', 'w')
        conn = sqlite3.connect('GAL.db')
        first_results = conn.cursor()
        second_results = conn.cursor()

        query = 'SELECT ga_id FROM genetico, geneticop WHERE ga_type = gap_id AND ga_kf >= 5 GROUP BY ga_type ORDER BY AVG(ga_' + metric + ') ASC LIMIT 20'
        first_results.execute(query)

        tests = '['
        count = 0

        for first_result in first_results:
            query = 'SELECT '+params+' FROM genetico, geneticop WHERE ga_type = gap_id AND ga_kf >= 5 AND ga_id = ' + str(first_result[0])
            second_results.execute(query)
            second_result = second_results.fetchone()

            tests += '(("../athena", "../img/lennamedium.tiff", Image, '+str(second_result[4])+',\n'
            tests += ' ("Genetic",\n'
            tests += '    GeneticOpt (Improvement 3,\n'
            tests += '                LG('+str(int(second_result[0]))+', '+str(second_result[0])+', 1),\n'
            tests += '                LG('+str(second_result[1])+', '+str(second_result[1])+', 1),\n'
            tests += '                LG('+str(second_result[2])+', '+str(second_result[2])+', 1),\n'
            tests += '                LG('+str(second_result[3])+', '+str(second_result[3])+', 1),\n'
            tests += '               )\n'
            tests += ')),\n' if (count != 19) else '))\n'
            count += 1
        tests += ']'

        testsfile.write(tests)
        testsfile.close()
        conn.close()
