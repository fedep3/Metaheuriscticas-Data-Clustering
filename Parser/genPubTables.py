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
    for i in range(lenght+7):
        cols += '|c'
    cols += '|'
    return cols

def insertion_sort(lista):
    for i in range(len(lista)):
        save = lista[i]
        j = i
        while j > 0 and compL(lista[j - 1], save):
            lista[j] = lista[j - 1]
            j -= 1
            lista[j] = save

def compL(felem,selem):
    if felem[0] < selem[0]:
        return True
    elif felem[0] == selem[0] and felem[1] >= selem[1]:
        return True
    else:
        return False
        
def insertion_sortE(lista):
    for i in range(len(lista)):
        save = lista[i]
        j = i
        while j > 0 and compLE(lista[j - 1], save):
            lista[j] = lista[j - 1]
            j -= 1
            lista[j] = save
        
def compLE(felem,selem):
    if felem[0] > selem[0]:
        return True
    elif felem[0] == selem[0] and felem[1] >= selem[1]:
        return True
    else:
        return False

def takeOut(table, parametros, nclust, avg, maxs, mins, params, latexTable, start, end):
    for i in range(start,end):

        consulta = 'SELECT ROUND(AVG('+table[0]+'_hib_fo),4), ROUND(AVG('+table[0]+'_hib_db),4), ROUND(AVG('+table[0]+'_hib_je),4), ROUND(AVG('+table[0]+'_hib_eval),4), ROUND(AVG('+table[0]+'_hib_time),4), ROUND(AVG('+table[0]+'_cluster_f),4) FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)
        avg.execute(consulta)
        consulta = 'SELECT '+table[0]+'_hib_fo, '+table[0]+'_hib_db, '+table[0]+'_hib_je, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo DESC LIMIT 1'
        maxs.execute(consulta)
        consulta = 'SELECT '+table[0]+'_hib_fo, '+table[0]+'_hib_db, '+table[0]+'_hib_je, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo ASC LIMIT 1'
        mins.execute(consulta)


        consulta = 'SELECT '+parametros[1]+' FROM '+table[1]+', '+table[1]+'p WHERE '+table[0]+'_type = '+table[0]+'p_id AND '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_cluster_f >= '+nclust
        params.execute(consulta)

        rowavg = avg.fetchone()
        rowmaxs = maxs.fetchone()
        rowmins = mins.fetchone()
        rowparams = params.fetchone()

        latexTable[i][0] = rowmaxs[0]
        latexTable[i][1] = rowmaxs[2]

        latexTable[i][2] +='            Promedio  & '+str(rowavg[0])+' & '+str(rowavg[1])+' & '+str(rowavg[2])+' & '+str(rowavg[3])+' & '+str(rowavg[4])+' & '+str(rowavg[5])+genSpaces(parametros[2])+'\\\\\n'
        latexTable[i][2] +='            \\cline{1-7}\n'
        latexTable[i][2] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+str(rowmaxs[4])+' & '+str(rowmaxs[5])+' & '+genParams(rowparams)+'\\\\\n'
        latexTable[i][2] +='            \\cline{1-7}\n'
        latexTable[i][2] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+' & '+str(rowmins[5])+genSpaces(parametros[2])+'\\\\\n'
    

        latexTable[i][2] +='        \\hline\n'
        latexTable[i][2] +='        \\hline\n'


if __name__ == "__main__":

    algsinfo = [['GAL','5'],['GAP','4'],['GAC','4'],['BEEL','5'],['BEEP','4'],['BEEC','4']]
    tandas = [['5','7','9','11'],['4','6','7','11'],['4','5','7','11'],['5'],['5'],['4','5','11']]
    table = [['ga','genetico'],['bee','abeja'],]
    parametros = [['$I$ & $tt$ & $pc$ & $pm$ ','gap_i, gap_tt, gap_pc, gap_pm',4],
                  ['$I$ & $m$ & $e$ & $eb$ & $ob$ ','beep_i, beep_m, beep_e, beep_eb, beep_ob',5]]

    tn = 0

    for algi in algsinfo:

        for tanda in tandas[tn]:

            print algi[0]+str(tanda)+'.sql'
            conn = sqlite3.connect(algi[0]+str(tanda)+'.sql')
            tableout = open('table'+algi[0]+str(tanda)+'.tex', 'w')

            latexTable = [[0.0,0.0,''] for i in range(20)]
            init = ''
            initC = ''
            finish = ''
            finishC = ''

            avg = conn.cursor()
            maxs = conn.cursor()
            mins = conn.cursor()
            params = conn.cursor()

            init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[tn/3][2])+'}\n        \\hline\n'
            init +='             & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & '+parametros[tn/3][0]+'\\\\\n'
            init +='        \\hline\n'
            init +='        \\hline\n'

            takeOut(table[tn/3], parametros[tn/3], algi[1], avg, maxs, mins, params, latexTable, 0, 15)

            finish +='        \\end{tabular}\n'
            if tn == 0:
                finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+str(tanda)+'} hibridado para {\\bf Lenna}}\n'
            else:
                finish +='        \\caption{Resultados de las mejores corridas de \emph{'+algi[0]+str(tanda)+'} hibridado para {\\bf Peppers}}\n'
            finish +='        \\label{tb:table'+algi[0]+str(tanda)+'}\n'
            finish +='    \\end{center}\n'
            finish +='\\end{table}\n'

            initC +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[tn/3][2])+'}\n        \\hline\n'
            initC +='             & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & '+parametros[tn/3][0]+'\\\\\n'
            initC +='        \\hline\n'
            initC +='        \\hline\n'

            takeOut(table[tn/3], parametros[tn/3], algi[1], avg, maxs, mins, params, latexTable, 15, 20)

            finishC +='        \\end{tabular}\n'
            if tn == 0:
                finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+str(tanda)+'} hibridado para {\\bf Lenna}}\n'
            else:
                finishC +='        \\caption{Continuaci\\\'on resultados de las mejores corridas de \emph{'+algi[0]+str(tanda)+'} hibridado para {\\bf Peppers}}\n'
            finishC +='        \\label{tb:tablec'+algi[0]+str(tanda)+'}\n'
            finishC +='    \\end{center}\n'
            finishC +='\\end{table}\n'

            insertion_sort(latexTable)

            tableout.write(init)

            for i in range(15):
                if i == 14:
                    tableout.write(latexTable[i][2][0:len(latexTable[i][2])-15])
                else:
                    tableout.write(latexTable[i][2])

            tableout.write(finish)

            tableout.write(initC)
            for i in range(15,20):
                if i == 19:
                    tableout.write(latexTable[i][2][0:len(latexTable[i][2])-15])
                else:
                    tableout.write(latexTable[i][2])


            tableout.write(finishC)
            tableout.close()
            conn.close()

        tn += 1
