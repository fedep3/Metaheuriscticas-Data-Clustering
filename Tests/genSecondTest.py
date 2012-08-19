#!/usr/bin/python
# -*- coding: utf-8 -*-
import sqlite3

if __name__ == "__main__":

    params = 'gap_i, ROUND(gap_ts, 2), ROUND(gap_cr, 2), ROUND(gap_mr, 2)'

    for metric in ['db','turi']:
        testsfile = open('second_tests_jet_' + metric + '.test', 'w')
        conn = sqlite3.connect('GAJ.db')
        first_results = conn.cursor()
        second_results = conn.cursor()

        query = 'SELECT A.ga_type, A.ga_ki FROM (SELECT DISTINCT ga_type FROM genetico WHERE ga_kf >= 5 GROUP BY ga_type, ga_ki ORDER BY AVG(ga_'+metric+') ASC) B LEFT JOIN genetico A WHERE A.ga_type = B.ga_type AND A.ga_kf >= 5 GROUP BY A.ga_type, A.ga_ki ORDER BY AVG(A.ga_'+metric+') ASC LIMIT 20;'
        first_results.execute(query)

        tests = '['
        count = 0

        for first_result in first_results:
            query = 'SELECT '+params+' FROM genetico, geneticop WHERE ga_type = gap_id AND ga_kf >= 5 AND ga_type = ' + str(first_result[0])
            second_results.execute(query)
            second_result = second_results.fetchone()

            tests += '(("../athena", "../img/jet.tiff", Image, '+str(first_result[1])+'),\n'
            tests += ' ("Genetic",\n'
            tests += '    GeneticOpt (Improvement 3,\n'
            tests += '                LG ('+str(int(second_result[0]))+', '+str(second_result[0])+', 1),\n'
            tests += '                LG ('+str(second_result[2])+', '+str(second_result[2])+', 0.1),\n'
            tests += '                LG ('+str(second_result[3])+', '+str(second_result[3])+', 0.1),\n'
            tests += '                LG ('+str(int(second_result[1]))+', '+str(int(second_result[1]))+', 1)\n'
            tests += '               )\n'
            tests += ')),\n' if (count != 19) else '))\n'
            count += 1

        tests += ']'

        testsfile.write(tests)
        testsfile.close()
        conn.close()
