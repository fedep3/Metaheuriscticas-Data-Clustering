#!/usr/bin/python
# -*- coding: utf-8 -*-
import sqlite3
from sys import argv

if __name__ == "__main__":

    params = 'gap_i, ROUND(gap_ts, 2), ROUND(gap_cr, 2), ROUND(gap_mr, 2)'

    for metric in ['db','turi']:
        print '\n' + str.upper(metric) + '\n'
        conn = sqlite3.connect(argv[1])
        first_results = conn.cursor()
        second_results = conn.cursor()

        query = 'SELECT A.ga_type, A.ga_ki FROM (SELECT DISTINCT ga_type FROM genetico WHERE ga_kf >= 5 GROUP BY ga_type, ga_ki ORDER BY AVG(ga_'+metric+') ASC) B LEFT JOIN genetico A WHERE A.ga_type = B.ga_type AND A.ga_kf >= 5 GROUP BY A.ga_type, A.ga_ki ORDER BY AVG(A.ga_'+metric+') ASC LIMIT 20;'
        first_results.execute(query)


        for first_result in first_results:
            query = 'SELECT '+params+' FROM genetico, geneticop WHERE ga_type = gap_id AND ga_kf >= 5 AND ga_type = ' + str(first_result[0])
            second_results.execute(query)
            second_result = second_results.fetchone()

            print 'I: ' + str(int(second_result[0])) + '\tTS: ' + str(int(second_result[1])) + '\tCR: ' + str(second_result[2]) + '\tMR: ' + str(second_result[3])

        conn.close()
