#!/usr/bin/python
# -*- coding: utf-8 -*-

import sqlite3
from decimal import Decimal

def genSpaces(lenght):
    spaces = ''
    for i in range(lenght):
        spaces += ' & '
    return spaces

def genParams(rowparam_values):
    result = ''
    for i in range(len(rowparam_values)-1):
        result += str(rowparam_values[i]) + ' & '
    result += str(rowparam_values[len(rowparam_values)-1])
    return result

def genCols(lenght):
    cols = ''
    for i in range(lenght+7):
        cols += '|c'
    cols += '|'
    return cols

def insertion_sort(lista):
    for i in range(len(lista)):
        save = lista[i]
        j = i
        while j > 0 and comp(lista[j - 1], save):
            lista[j] = lista[j - 1]
            j -= 1
            lista[j] = save

def comp(felem,selem):
    if felem[0] > selem[0]:
        return True
    else:
        return False
       
def takeOut(table, params, nclust, statistics, maxs, mins, param_values, latexTable, algcsv, start, end):
    for i in range(start,end):

        consulta = 'SELECT SUM( (tabla.'+table[0]+'_hib_db - averages.averageDB) * (tabla.'+table[0]+'_hib_db - averages.averageDB) )/ (COUNT(tabla.'+table[0]+'_hib_db) - 1), averages.averageDB,  averages.averageE, averages.averageT, averages.averageC, COUNT(tabla.'+table[0]+'_hib_db) FROM '+table[1]+' AS tabla, (SELECT AVG('+table[0]+'_hib_db) AS averageDB, ROUND(AVG('+table[0]+'_hib_eval),4) AS averageE, ROUND(AVG('+table[0]+'_hib_time),4) AS averageT, ROUND(AVG('+table[0]+'_cluster_f),4) AS averageC FROM '+table[1]+' WHERE '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_cluster_f >= '+nclust+') AS averages WHERE '+table[0]+'_type = '+str(i+1)
        statistics.execute(consulta)
        consulta = 'SELECT '+table[0]+'_hib_db, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo DESC LIMIT 1'
        maxs.execute(consulta)
        consulta = 'SELECT '+table[0]+'_hib_db, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo ASC LIMIT 1'
        mins.execute(consulta)
        consulta = 'SELECT '+params[1]+' FROM '+table[1]+', '+table[1]+'p WHERE '+table[0]+'_type = '+table[0]+'p_id AND '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_cluster_f >= '+nclust
        param_values.execute(consulta)

        rowstatistics = statistics.fetchone()
        rowmaxs = maxs.fetchone()
        rowmins = mins.fetchone()
        rowparam_values = param_values.fetchone()

        latexTable[i][0] = Decimal(str(rowstatistics[1]))
        mean = latexTable[i][0].quantize(Decimal('1.0000'))
        interval = (Decimal('1.959964') * (Decimal(str(rowstatistics[0])) / Decimal(str(rowstatistics[5]))).sqrt()).quantize(Decimal('1.0000'))

        algcsv.write(str(mean)+','+''.join(map(lambda x: str(x)+',', rowparam_values))[0:-1]+'\n')

        latexTable[i][1] +='            Promedio  & $' +str(mean)+' \\pm ' +str(interval)+ '$ & '+str(rowstatistics[2])+' & '+str(rowstatistics[3])+' & '+str(rowstatistics[4])+genSpaces(params[2])+'\\\\\n'
        latexTable[i][1] +='            \\cline{1-5}\n'
        latexTable[i][1] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+genParams(rowparam_values)+'\\\\\n'
        latexTable[i][1] +='            \\cline{1-5}\n'
        latexTable[i][1] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+genSpaces(params[2])+'\\\\\n'
    

        latexTable[i][1] +='        \\hline\n'
        latexTable[i][1] +='        \\hline\n'


if __name__ == "__main__":

    algsinfo = [['GAL','5'],['GAP','4'],['GAC','4'],['BEEL','5'],['BEEP','4'],['BEEC','4']]
    runs = [['5','7','9','11'],['4','6','7','11'],['4','5','7','11'],['5'],['5'],['4','5','11']]
    table = [['ga','genetico'],['bee','abeja'],]
    params = [['$I$ & $tt$ & $pc$ & $pm$ ','ROUND(gap_i,4), ROUND(gap_tt,4), ROUND(gap_pc,4), ROUND(gap_pm,4)',4],
                  ['$I$ & $m$ & $e$ & $eb$ & $ob$ ','ROUND(beep_i,4), ROUND(beep_m,4), ROUND(beep_e,4), ROUND(beep_eb,4), ROUND(beep_ob,4)',5]]

    tn = 0

    for algi in algsinfo:

        algcsv = open(algi[0]+'.csv', 'w')

        for run in runs[tn]:

            print algi[0]+str(run)+'.sql'
            conn = sqlite3.connect(algi[0]+str(run)+'.sql')
            tableout = open('table'+algi[0]+str(run)+'.tex', 'w')

            latexTable = [[0.0,''] for i in range(20)]
            init = ''
            initC = ''
            finish = ''
            finishC = ''

            statistics = conn.cursor()
            maxs = conn.cursor()
            mins = conn.cursor()
            param_values = conn.cursor()

            init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(params[tn/3][2])+'}\n        \\hline\n'
            init +='             & {\\bf DB} & {\\bf E} & {\\bf T} & {\\bf KE} & '+params[tn/3][0]+'\\\\\n'
            init +='        \\hline\n'
            init +='        \\hline\n'

            takeOut(table[tn/3], params[tn/3], algi[1], statistics, maxs, mins, param_values, latexTable, algcsv, 0, 15)

            finish +='        \\end{tabular}\n'
            if tn%3 == 0:
                finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Lenna}}\n'
            elif tn%3 == 1:
                finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Peppers}}\n'
            else:
                finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Cameraman}}\n'
            finish +='        \\label{tb:table'+algi[0]+str(run)+'}\n'
            finish +='    \\end{center}\n'
            finish +='\\end{table}\n'

            initC +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(params[tn/3][2])+'}\n        \\hline\n'
            initC +='             & {\\bf DB} & {\\bf E} & {\\bf T} & {\\bf KE} & '+params[tn/3][0]+'\\\\\n'
            initC +='        \\hline\n'
            initC +='        \\hline\n'

            takeOut(table[tn/3], params[tn/3], algi[1], statistics, maxs, mins, param_values, latexTable, algcsv, 15, 20)

            finishC +='        \\end{tabular}\n'
            if tn%3 == 0:
                finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Lenna}}\n'
            elif tn%3 == 1:
                finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Peppers}}\n'
            else:
                finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+str(run)+'} hibridado para {\\bf Cameraman}}\n'
            finishC +='        \\label{tb:tablec'+algi[0]+str(run)+'}\n'
            finishC +='    \\end{center}\n'
            finishC +='\\end{table}\n'

            insertion_sort(latexTable)
            tableout.write(init)

            for i in range(15):
                if i == 14:
                    tableout.write(latexTable[i][1][0:len(latexTable[i][1])-15])
                else:
                    tableout.write(latexTable[i][1])

            tableout.write(finish)

            tableout.write(initC)
            for i in range(15,20):
                if i == 19:
                    tableout.write(latexTable[i][1][0:len(latexTable[i][1])-15])
                else:
                    tableout.write(latexTable[i][1])


            tableout.write(finishC)
            tableout.close()
            conn.close()

        tn += 1
        algcsv.close()
