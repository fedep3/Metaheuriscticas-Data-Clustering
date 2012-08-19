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
       
def takeOut(table, params, nclust, turi_or_db_column, statistics, maxs, mins, param_values, latexTable, start, end):
    for i in range(start,end):

        query = 'SELECT SUM( (tabla.'+table[0]+'_db - averages.averageDB) * (tabla.'+table[0]+'_db - averages.averageDB) )/ (COUNT(tabla.'+table[0]+'_db) - 1), SUM( (tabla.'+table[0]+'_turi - averages.averageTuri) * (tabla.'+table[0]+'_turi - averages.averageTuri) )/ (COUNT(tabla.'+table[0]+'_turi) - 1), averages.averageDB, averages.averageTuri,  averages.averageE, averages.averageT, averages.averageC, COUNT(tabla.'+table[0]+'_db) FROM '+table[1]+' AS tabla, (SELECT AVG('+table[0]+'_db) AS averageDB, AVG('+table[0]+'_turi) AS averageTuri, ROUND(AVG('+table[0]+'_eval),4) AS averageE, ROUND(AVG('+table[0]+'_time),4) AS averageT, ROUND(AVG('+table[0]+'_kf),4) AS averageC FROM '+table[1]+' WHERE '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_kf >= '+nclust+') AS averages WHERE '+table[0]+'_type = '+str(i+1)
        statistics.execute(query)
        query = 'SELECT '+table[0]+'_db, '+table[0]+'_turi, '+table[0]+'_eval, '+table[0]+'_time, '+table[0]+'_kf FROM '+table[1]+' WHERE '+table[0]+'_kf >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_db ASC LIMIT 1'
        maxs.execute(query)
        query = 'SELECT '+table[0]+'_db, '+table[0]+'_turi, '+table[0]+'_eval, '+table[0]+'_time, '+table[0]+'_kf FROM '+table[1]+' WHERE '+table[0]+'_kf >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_db DESC LIMIT 1'
        mins.execute(query)
        query = 'SELECT '+params[1]+' FROM '+table[1]+', '+table[1]+'p WHERE '+table[0]+'_type = '+table[0]+'p_id AND '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_kf >= '+nclust
        param_values.execute(query)

        rowstatistics = statistics.fetchone()
        rowmaxs = maxs.fetchone()
        rowmins = mins.fetchone()
        rowparam_values = param_values.fetchone()

        latexTable[i][0] = Decimal(str(rowstatistics[turi_or_db_column]))
       
        dbmean = Decimal(str(rowstatistics[2])).quantize(Decimal('1.0000'))
        dbinterval = (Decimal('1.959964') * (Decimal(str(rowstatistics[0])) / Decimal(str(rowstatistics[7]))).sqrt()).quantize(Decimal('1.0000'))

        turimean = Decimal(str(rowstatistics[3])).quantize(Decimal('1.0000'))
        turiinterval = (Decimal('1.959964') * (Decimal(str(rowstatistics[1])) / Decimal(str(rowstatistics[7]))).sqrt()).quantize(Decimal('1.0000'))

        latexTable[i][1] +='            Promedio  & $' +str(dbmean)+' \\pm ' +str(dbinterval)+ '$ & $' +str(turimean)+' \\pm ' +str(turiinterval)+ '$ & '+str(rowstatistics[4])+' & '+str(rowstatistics[5])+' & '+str(rowstatistics[6])+genSpaces(params[2])+'\\\\\n'
        latexTable[i][1] +='            \\cline{1-6}\n'
        latexTable[i][1] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+str(rowmaxs[4])+' & '+genParams(rowparam_values)+'\\\\\n'
        latexTable[i][1] +='            \\cline{1-6}\n'
        latexTable[i][1] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+genSpaces(params[2])+'\\\\\n'
    

        latexTable[i][1] +='        \\hline\n'
        latexTable[i][1] +='        \\hline\n'

def takeOutKmeans(table, nclust, turi_or_db_column, statistics, maxs, mins, latexTable):
    query = 'SELECT SUM( (tabla.'+table+'_db - averages.averageDB) * (tabla.'+table+'_db - averages.averageDB) )/ (COUNT(tabla.'+table+'_db) - 1), SUM( (tabla.'+table+'_turi - averages.averageTuri) * (tabla.'+table+'_turi - averages.averageTuri) )/ (COUNT(tabla.'+table+'_turi) - 1), averages.averageDB, averages.averageTuri,  averages.averageE, averages.averageT, averages.averageC, COUNT(tabla.'+table+'_db) FROM '+table+' AS tabla, (SELECT AVG('+table+'_db) AS averageDB, AVG('+table+'_turi) AS averageTuri, ROUND(AVG('+table+'_eval),4) AS averageE, ROUND(AVG('+table+'_time),4) AS averageT, ROUND(AVG('+table+'_kf),4) AS averageC FROM '+table+' WHERE '+table+'_kf >= '+nclust+') AS averages'
    statistics.execute(query)
    query = 'SELECT '+table+'_db, '+table+'_turi, '+table+'_eval, '+table+'_time, '+table+'_kf FROM '+table+' WHERE '+table+'_kf >= '+nclust+' ORDER BY '+table+'_db ASC LIMIT 1'
    maxs.execute(query)
    query = 'SELECT '+table+'_db, '+table+'_turi, '+table+'_eval, '+table+'_time, '+table+'_kf FROM '+table+' WHERE '+table+'_kf >= '+nclust+' ORDER BY '+table+'_db DESC LIMIT 1'
    mins.execute(query)

    rowstatistics = statistics.fetchone()
    rowmaxs = maxs.fetchone()
    rowmins = mins.fetchone()

    latexTable[i][0] = Decimal(str(rowstatistics[turi_or_db_column]))
   
    dbmean = Decimal(str(rowstatistics[2])).quantize(Decimal('1.0000'))
    dbinterval = (Decimal('1.959964') * (Decimal(str(rowstatistics[0])) / Decimal(str(rowstatistics[7]))).sqrt()).quantize(Decimal('1.0000'))

    turimean = Decimal(str(rowstatistics[3])).quantize(Decimal('1.0000'))
    turiinterval = (Decimal('1.959964') * (Decimal(str(rowstatistics[1])) / Decimal(str(rowstatistics[7]))).sqrt()).quantize(Decimal('1.0000'))

    latexTable[i][1] +='            Promedio  & $' +str(dbmean)+' \\pm ' +str(dbinterval)+ '$ & $' +str(turimean)+' \\pm ' +str(turiinterval)+ '$ & '+str(rowstatistics[4])+' & '+str(rowstatistics[5])+' & '+str(rowstatistics[6])+'\\\\\n'
    latexTable[i][1] +='            \\cline{1-6}\n'
    latexTable[i][1] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+str(rowmaxs[4])+'\\\\\n'
    latexTable[i][1] +='            \\cline{1-6}\n'
    latexTable[i][1] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+'\\\\\n'


    latexTable[i][1] +='        \\hline\n'
    latexTable[i][1] +='        \\hline\n'


if __name__ == "__main__":

    algsinfo = [['GAM','5'], ['GAJ','5'], ['KMEANSM','5'], ['KMEANSJ','5']]
    table = [['ga','genetico'],
             ['kmeans']]
    params = [['$i$ & $ts$ & $cr$ & $mr$ ','ROUND(gap_i,4), ROUND(gap_ts,4), ROUND(gap_cr,4), ROUND(gap_mr,4)',4]]
    metrics = [['DB', 2],['TURI',3]]

    tn = 0

    for algi in algsinfo:

        for metric in metrics:

            print algi[0]+'|Metric:'+metric[0]
            conn = sqlite3.connect(algi[0]+metric[0]+'.db')
            tableout = open('table'+algi[0]+metric[0]+'.tex', 'w')

            latexTable = [[0.0,''] for i in range(20)]
            init = ''
            initC = ''
            finish = ''
            finishC = ''

            statistics = conn.cursor()
            maxs = conn.cursor()
            mins = conn.cursor()
            param_values = conn.cursor()

            if tn < 2:
                init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(params[0][2])+'}\n        \\hline\n'
                init +='             & {\\bf DB} & {\\bf Turi} & {\\bf E} & {\\bf T} & {\\bf KE} & '+params[0][0]+'\\\\\n'
                init +='        \\hline\n'
                init +='        \\hline\n'

                takeOut(table[0], params[0], algi[1], metric[1], statistics, maxs, mins, param_values, latexTable, 0, 15)

                finish +='        \\end{tabular}\n'
                if tn%3 == 0:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} hibridado para {\\bf Mandrill}}\n'
                else:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} hibridado para {\\bf Jet}}\n'

                finish +='        \\label{tb:table'+algi[0]+'}\n'
                finish +='    \\end{center}\n'
                finish +='\\end{table}\n'

                initC +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(params[0][2])+'}\n        \\hline\n'
                initC +='             & {\\bf DB} & {\\bf Turi} & {\\bf E} & {\\bf T} & {\\bf KE} & '+params[0][0]+'\\\\\n'
                initC +='        \\hline\n'
                initC +='        \\hline\n'

                takeOut(table[0], params[0], algi[1], metric[1], statistics, maxs, mins, param_values, latexTable, 15, 20)

                finishC +='        \\end{tabular}\n'
                if tn%3 == 0:
                    finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} hibridado para {\\bf Mandrill}}\n'
                else:
                    finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} hibridado para {\\bf Jet}}\n'

                finishC +='        \\label{tb:tablec'+algi[0]+'}\n'
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
            else:
                init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(0)+'}\n        \\hline\n'
                init +='             & {\\bf DB} & {\\bf Turi} & {\\bf E} & {\\bf T} & {\\bf KE} \\\\\n'
                init +='        \\hline\n'
                init +='        \\hline\n'

                takeOutKmeans(table[1][0], algi[1], metric[1], statistics, maxs, mins, latexTable)

                finish +='        \\end{tabular}\n'
                if tn%3 == 0:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} para {\\bf Mandrill}}\n'
                else:
                    finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+metric[0]+'} para {\\bf Jet}}\n'

                finish +='        \\label{tb:table'+algi[0]+'}\n'
                finish +='    \\end{center}\n'
                finish +='\\end{table}\n'

                tableout.write(init)
                tableout.write(latexTable[19][1][0:len(latexTable[i][1])-15])
                tableout.write(finish)

            tableout.close()
            conn.close()
        tn += 1
