#!/usr/bin/python
# -*- coding: utf-8 -*-

import sqlite3

def genSpaces(lenght):
    spaces = ''
    for i in range(lenght):
        spaces += ' & '
    return spaces

def genParams(rowparams):
    result = ''
    for i in range(len(rowparams)-1):
        result += str(rowparams[i]) + ' & '
    result += str(rowparams[len(rowparams)-1])
    return result

def genCols(lenght):
    cols = ''
    for i in range(lenght+6):
        cols += '|c'
    cols += '|'
    return cols

def takeOut(table, parametros, nclust, mins, params, latexTable, start, end, bests):


    consulta = 'SELECT '+table[0]+'_hib_fo, '+table[0]+'_hib_db, '+table[0]+'_hib_je, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+str(nclust)+' ORDER BY '+table[0]+'_hib_db ASC LIMIT 20'
    mins.execute(consulta)
    consulta = 'SELECT '+parametros[1]+' FROM '+table[1]+', '+table[1]+'p WHERE '+table[0]+'_type = '+table[0]+'p_id AND '+table[0]+'_cluster_f >= '+str(nclust)+' ORDER BY '+table[0]+'_hib_db ASC LIMIT 20'
    params.execute(consulta)

    for i in range(start,end):

        rowmins = mins.fetchone()
        rowparams = params.fetchone()

        updateBest(bests, rowmins[1], rowmins, rowparams)
        latexTable[i] +='            '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+' & '+str(rowmins[5])+' & '+genParams(rowparams)+'\\\\\n'
        latexTable[i] +='        \\hline\n'
        latexTable[i] +='        \\hline\n'

def updateBest(bests, num, rowmins, rowparams):
    for i in range(len(bests)):
        if(bests[i][0] > num):
            bests[i][0] = num
            bests[i][1] ='            '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+' & '+str(rowmins[5])+' & '+genParams(rowparams)+'\\\\\n'
            bests[i][1] +='        \\hline\n'
            bests[i][1] +='        \\hline\n'
            break


if __name__ == "__main__":

    clusters = [5,4]
    cases = ['GAL','GAP','BEEL','BEEP']
    table = [['ga','genetico'],['bee','abeja'],]
    parametros = [['$I$ & $tt$ & $pc$ & $pm$ ','gap_i, gap_tt, gap_pc, gap_pm',4],
                  ['$I$ & $m$ & $e$ & $eb$ & $ob$ ','beep_i, beep_m, beep_e, beep_eb, beep_ob',5]]
    bests = [[[999.99,''] for i in range(10)] for j in range(4)]

    print 'BEEL11.sql'

    conn = sqlite3.connect('BEEL11.sql')
    tableout = open('tableBEEL11.tex', 'w')

    latexTable = ['' for i in range(20)]
    init = ''
    finish = ''

    mins = conn.cursor()
    params = conn.cursor()

    init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[1][2])+'}\n        \\hline\n'
    init +='            {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & '+parametros[1][0]+'\\\\\n'
    init +='        \\hline\n'
    init +='        \\hline\n'

    #COMIENZO

    takeOut(table[1], parametros[1], clusters[1], mins, params, latexTable, 0, 20, bests[3])

    finish +='        \\end{tabular}\n'
    finish +='        \\caption{Resultados de las mejores corridas de \emph{BEEL11} hibridado para {\\bf Peppers}}\n'
    finish +='        \\label{tb:tableBEEL11}\n'
    finish +='    \\end{center}\n'
    finish +='\\end{table}\n'

    tableout.write(init)

    for i in range(20):
        if i == 19:
            tableout.write(latexTable[i][0:len(latexTable[i])-15])
        else:
            tableout.write(latexTable[i])

    tableout.write(finish)
    tableout.close()
    conn.close()

    print 'BEEP11.sql'

    conn = sqlite3.connect('BEEP11.sql')
    tableout = open('tableBEEP11.tex', 'w')

    latexTable = ['' for i in range(20)]
    init = ''
    finish = ''

    mins = conn.cursor()
    params = conn.cursor()

    init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[1][2])+'}\n        \\hline\n'
    init +='            {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & '+parametros[1][0]+'\\\\\n'
    init +='        \\hline\n'
    init +='        \\hline\n'

    #COMIENZO

    takeOut(table[1], parametros[1], clusters[1], mins, params, latexTable, 0, 20, bests[3])

    finish +='        \\end{tabular}\n'
    finish +='        \\caption{Resultados de las mejores corridas de \emph{BEEP11} hibridado para {\\bf Lenna}}\n'
    finish +='        \\label{tb:tableBEEP11}\n'
    finish +='    \\end{center}\n'
    finish +='\\end{table}\n'

    tableout.write(init)

    for i in range(20):
        if i == 19:
            tableout.write(latexTable[i][0:len(latexTable[i])-15])
        else:
            tableout.write(latexTable[i])

    tableout.write(finish)
    tableout.close()
    conn.close()
                  
    for alg in range(2):

        for tanda in range(4):

            for corrida in range(5):
                print cases[alg]+str(tanda+1)+str(corrida+1)+'.sql'
                conn = sqlite3.connect(cases[alg]+str(tanda+1)+str(corrida+1)+'.sql')
                tableout = open('table'+cases[alg]+str(tanda+1)+str(corrida+1)+'.tex', 'w')

                latexTable = ['' for i in range(20)]
                init = ''
                finish = ''

                mins = conn.cursor()
                params = conn.cursor()

                init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[alg/2][2])+'}\n        \\hline\n'
                init +='            {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & '+parametros[alg/2][0]+'\\\\\n'
                init +='        \\hline\n'
                init +='        \\hline\n'

                #COMIENZO

                takeOut(table[alg/2], parametros[alg/2], clusters[alg%2], mins, params, latexTable, 0, 20, bests[alg])

                finish +='        \\end{tabular}\n'
                if alg%2 == 0:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+cases[alg]+str(tanda+1)+str(corrida+1)+'} hibridado para {\\bf Lenna}}\n'
                else:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+cases[alg]+str(tanda+1)+str(corrida+1)+'} hibridado para {\\bf Peppers}}\n'
                finish +='        \\label{tb:table'+cases[alg]+str(tanda+1)+str(corrida+1)+'}\n'
                finish +='    \\end{center}\n'
                finish +='\\end{table}\n'

                tableout.write(init)

                for i in range(20):
                    if i == 19:
                        tableout.write(latexTable[i][0:len(latexTable[i])-15])
                    else:
                        tableout.write(latexTable[i])

                tableout.write(finish)
                tableout.close()
                conn.close()

        btableout = open('btable'+cases[alg]+'.tex', 'w')
        btableout.write(init)
        for i in range(10):
            if i == 9:
                btableout.write(bests[alg][i][1][0:len(bests[alg][i][1])-15])
            else:
                btableout.write(bests[alg][i][1])
        btableout.write(finish)
