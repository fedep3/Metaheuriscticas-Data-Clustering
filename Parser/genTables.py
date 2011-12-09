#!/usr/bin/python
 # -*- coding: utf-8 -*-

import sqlite3

def genSpaces(lenght):
    spaces = ''
    for i in range(lenght):
        spaces += ' & '
    return spaces

def genParams(rowparams):
    result = ' & '
    for i in range(len(rowparams)-1):
        result += str(rowparams[i]) + ' & '
    result += str(rowparams[len(rowparams)-1])
    return result

def genCols(lenght, typea):
    cols = ''
    if typea == 'hib':
        init = 8
    else:
        init = 6
    
    for i in range(lenght+init):
        cols += '|c'
    cols += '|'
    return cols

def addO(O, sparams):
    params = sparams.split(',')
    result = ''
    for i in range(len(params)-1):
        if O == 'AVG':
            result += 'ROUND(AVG('+params[i]+'),4),'
        else:
            result += O+'('+params[i]+'),'
    if O == 'AVG':
        result += 'ROUND(AVG('+params[len(params)-1]+'),4)'
    else:
        result += O+'('+params[len(params)-1]+')'
    return result

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

def takeOut(table, parametros, nclust, avg, maxs, mins, params, latexTable, start, end, optclusters, typea):
    for i in range(start,end):

        if typea == 'hib':
            consulta = 'SELECT ROUND(AVG('+table[0]+'_hib_fo),4), ROUND(AVG('+table[0]+'_hib_db),4), ROUND(AVG('+table[0]+'_hib_je),4), ROUND(AVG('+table[0]+'_hib_eval),4), ROUND(AVG('+table[0]+'_hib_time),4), ROUND(AVG('+table[0]+'_cluster_f),4) FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)
            avg.execute(consulta)
            consulta = 'SELECT '+table[0]+'_hib_fo, '+table[0]+'_hib_db, '+table[0]+'_hib_je, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo DESC LIMIT 1'
            maxs.execute(consulta)
            consulta = 'SELECT '+table[0]+'_hib_fo, '+table[0]+'_hib_db, '+table[0]+'_hib_je, '+table[0]+'_hib_eval, '+table[0]+'_hib_time, '+table[0]+'_cluster_f FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_hib_fo ASC LIMIT 1'
            mins.execute(consulta)
        else:
            consulta = 'SELECT ROUND(AVG('+table[0]+'_alg_fo),4), ROUND(AVG('+table[0]+'_alg_db),4), ROUND(AVG('+table[0]+'_alg_je),4), ROUND(AVG('+table[0]+'_alg_eval),4), ROUND(AVG('+table[0]+'_alg_time),4) FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)
            avg.execute(consulta)
            consulta = 'SELECT '+table[0]+'_alg_fo, '+table[0]+'_alg_db, '+table[0]+'_alg_je, '+table[0]+'_alg_eval, '+table[0]+'_alg_time FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_alg_fo DESC LIMIT 1'
            maxs.execute(consulta)
            consulta = 'SELECT '+table[0]+'_alg_fo, '+table[0]+'_alg_db, '+table[0]+'_alg_je, '+table[0]+'_alg_eval, '+table[0]+'_alg_time FROM '+table[1]+' WHERE '+table[0]+'_cluster_f >= '+nclust+' AND '+table[0]+'_type = '+str(i+1)+' ORDER BY '+table[0]+'_alg_fo ASC LIMIT 1'
            mins.execute(consulta)


        consulta = 'SELECT '+parametros[1]+' FROM '+table[1]+', '+table[1]+'p WHERE '+table[0]+'_type = '+table[0]+'p_id AND '+table[0]+'_type = '+str(i+1)+' AND '+table[0]+'_cluster_f >= '+nclust
        params.execute(consulta)

        rowavg = avg.fetchone()
        rowmaxs = maxs.fetchone()
        rowmins = mins.fetchone()
        rowparams = params.fetchone()

        latexTable[i][0] = rowmaxs[0]
        latexTable[i][1] = rowmaxs[2]

        if typea == 'hib':
            latexTable[i][2] +='            Promedio  & '+str(rowavg[0])+' & '+str(rowavg[1])+' & '+str(rowavg[2])+' & '+str(rowavg[3])+' & '+str(rowavg[4])+' & '+str(rowavg[5])+' & '+optclusters+genSpaces(parametros[2])+'\\\\\n'
            latexTable[i][2] +='            \\cline{1-8}\n'
            latexTable[i][2] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+str(rowmaxs[4])+' & '+str(rowmaxs[5])+' & '+optclusters+genParams(rowparams)+'\\\\\n'
            latexTable[i][2] +='            \\cline{1-8}\n'
            latexTable[i][2] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+' & '+str(rowmins[5])+' & '+optclusters+genSpaces(parametros[2])+'\\\\\n'
        else:
            latexTable[i][2] +='            Promedio  & '+str(rowavg[0])+' & '+str(rowavg[1])+' & '+str(rowavg[2])+' & '+str(rowavg[3])+' & '+str(rowavg[4])+genSpaces(parametros[2])+'\\\\\n'
            latexTable[i][2] +='            \\cline{1-6}\n'
            latexTable[i][2] +='            Mejor & '+str(rowmaxs[0])+' & '+str(rowmaxs[1])+'  & '+str(rowmaxs[2])+' & '+str(rowmaxs[3])+' & '+str(rowmaxs[4])+genParams(rowparams)+'\\\\\n'
            latexTable[i][2] +='            \\cline{1-6}\n'
            latexTable[i][2] +='            Peor & '+str(rowmins[0])+' & '+str(rowmins[1])+'  & '+str(rowmins[2])+' & '+str(rowmins[3])+' & '+str(rowmins[4])+genSpaces(parametros[2])+'\\\\\n'
        

        latexTable[i][2] +='        \\hline\n'
        latexTable[i][2] +='        \\hline\n'

if __name__ == "__main__":

    db = ['img','csv']
    clust = ['5','3']
    table = [['ant','hormiga'],['bee','abeja'],['de','de'],['ga','genetico'],['pso','pso'],['sde','sde'],['wpso','wpso']]
    parametros = [['$I$','antp_i',1],
                  ['$I$ & $m$ & $e$ & $eb$ & $ob$ ','beep_i, beep_m, beep_e, beep_eb, beep_ob',5],
                  ['$I$ & $w_1$ & $w_2$ & $w_3$ ','dep_i, dep_w1, dep_w2, dep_w3',4],
                  ['$I$ & $tt$ & $pc$ & $pm$ ','gap_i, gap_tt, gap_pc, gap_pm',4],
                  ['$I$ & W & $c_1$ & $c_2$ & $vmx$ ','psop_i, psop_w, psop_c1, psop_c2, psop_vmx',5],
                  ['$I$ & $w_1$ & $w_2$ & $w_3$ & $\\gamma$ & $Cr$ ','sdep_i, sdep_w1, sdep_w2, sdep_w3, sdep_f, sdep_pc',6],
                  ['$I$ & $w_1$ & $w_2$ & $w_3$ & $W$ & $c_1$ & $c_2$ & $vmx$ ','wpsop_i, wpsop_w1, wpsop_w2, wpsop_w3, wpsop_w, wpsop_c1, wpsop_c2, wpsop_vmx',8]]
                  
    optimo = ['$[5-10]$','3']
    typesa = ['hib','alg']
    
    for actualtype in range(2):

        for format in range(2):

            conn = sqlite3.connect('final_'+db[format]+'.sql')

            for alg in range(7):

                #RESULTADOS

                tableout = open('table'+table[alg][0].upper()+typesa[actualtype].upper()+db[format]+'.tex', 'w')
                pmpout = open('pmp'+table[alg][0].upper()+typesa[actualtype].upper()+db[format]+'.tex', 'w')

                if alg == 0:
                    size = 8
                else:
                    size = 20

                latexTable = [[0.0,0.0,''] for i in range(size)]
                init = ''
                initC = ''
                finish = ''
                finishC = ''

                avg = conn.cursor()
                maxs = conn.cursor()
                mins = conn.cursor()
                params = conn.cursor()

                init +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[alg][2], typesa[actualtype])+'}\n        \\hline\n'
                if typesa[actualtype] == 'hib':
                    init +='            & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & {\\bf KO} & '+parametros[alg][0]+'\\\\\n'
                else:
                    init +='            & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & '+parametros[alg][0]+'\\\\\n'                
                init +='        \\hline\n'
                init +='        \\hline\n'

                if alg == 0:
                    count = 8
                else:
                    count = 15

                #COMIENZO

                takeOut(table[alg], parametros[alg], clust[format], avg, maxs, mins, params, latexTable, 0, count, optimo[format], typesa[actualtype])

                finish +='        \\end{tabular}\n'
                if format == 0:
                    if typesa[actualtype] == 'hib':
                        finish +='        \\caption{Resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} hibridado para {\\bf Lenna}}\n'
                    else:
                        finish +='        \\caption{Resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} no hibridado para {\\bf Lenna}}\n'
                else:
                    if typesa[actualtype] == 'hib':
                        finish +='        \\caption{Resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} hibridado para {\\bf Iris}}\n'
                    else:
                        finish +='        \\caption{Resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} no hibridado para {\\bf Iris}}\n'
                finish +='        \\label{tb:table'+table[alg][0]+db[format]+'}\n'
                finish +='    \\end{center}\n'
                finish +='\\end{table}\n'

                #CONTINUACION

                if count == 15:
                    initC +='\n\n'
                    initC +='\\begin{table}[h!]\n    \\footnotesize\n    \\begin{center}\n        \\begin{tabular}{'+genCols(parametros[alg][2], typesa[actualtype])+'}\n        \\hline\n'
                    if typesa[actualtype] == 'hib':
                        initC +='            & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf E} & {\\bf T} & {\\bf KE} & {\\bf KO} & '+parametros[alg][0]+'\\\\\n'
                    else:
                        initC +='            & {\\bf FO} & {\\bf DB} & $J_e$ & {\\bf EA} & {\\bf T} & '+parametros[alg][0]+'\\\\\n'                
                    initC +='        \\hline\n'
                    initC +='        \\hline\n'

                    takeOut(table[alg], parametros[alg], clust[format], avg, maxs, mins, params, latexTable, 15, 20, optimo[format], typesa[actualtype])

                    finishC +='        \\end{tabular}\n'
                    if format == 0:
                        if typesa[actualtype] == 'hib':
                            finishC +='        \\caption{Continuacion resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} hibridado para {\\bf Lenna}}\n'
                        else:
                            finishC +='        \\caption{Continuacion resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} no hibridado para {\\bf Lenna}}\n'
                    else:
                        if typesa[actualtype] == 'hib':
                            finishC +='        \\caption{Continuacion resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} hibridado para {\\bf Iris}}\n'
                        else:
                            finishC +='        \\caption{Continuacion resultados de las mejores corridas de \emph{'+table[alg][0].upper()+'} no hibridado para {\\bf Iris}}\n'
                    finishC +='        \\label{tb:table'+table[alg][0]+db[format]+'}\n'
                    finishC +='    \\end{center}\n'
                    finishC +='\\end{table}\n'

                if typesa[actualtype] == 'alg' and (alg == 2 or alg > 3):
                    insertion_sortE(latexTable)
                else:
                    insertion_sort(latexTable)

                tableout.write(init)
                pmpout.write(init)

                for i in range(count):
                    if i == count - 1:
                        tableout.write(latexTable[i][2][0:len(latexTable[i][2])-15])
                    else:
                        tableout.write(latexTable[i][2])
                    if i == 0:
                        pmpout.write(latexTable[i][2][0:len(latexTable[i][2])-15])


                tableout.write(finish)
                pmpout.write(finish)

                if count == 15:
                    tableout.write(initC)
                    for i in range(15,20):
                        if i == 19:
                            tableout.write(latexTable[i][2][0:len(latexTable[i][2])-15])
                        else:
                            tableout.write(latexTable[i][2])
                    tableout.write(finishC)

                tableout.close()
                pmpout.close()

            conn.close()
