# %% Importation des packages

import pandas as pd
import numpy as np
import copy
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
import seaborn as sns
import os
from sklearn.linear_model import Lasso
from sklearn.preprocessing import StandardScaler
import seaborn as sns

cwd = os.getcwd()

# %% Variable yty_var
conso_tri = pd.read_csv('Ressources/Consommation des ménages trimestrielle.csv',sep=';')
conso_tri = conso_tri.set_index('Période').replace({',':'.'},regex=True).astype('float64')

yty_var = pd.DataFrame()

for column in conso_tri.columns:
    var = (conso_tri[column].diff(4)/conso_tri[column].shift(4)).iloc[4:].reset_index().drop('Période', axis = 1)
    yty_var[column] = var[var.columns[0]]

yty_var=yty_var.set_index(conso_tri.index.drop('1990T1').drop('1990T2').drop('1990T3').drop('1990T4')).T
# %% ACP

X_yty_var = StandardScaler().fit_transform(yty_var.T)

pca = PCA()
X_pca = pd.DataFrame(pca.fit_transform(X_yty_var))

principal_components = pd.DataFrame(pca.components_)
principal_components = principal_components.rename({i : 'PC'+str(i+1) for i in range(len(principal_components.columns))},axis=1)
principal_components = principal_components.rename(dict(zip(range(len(yty_var.T.columns)),yty_var.T.columns)))

# %% ANOVA

weights = pd.DataFrame()

for date in conso_tri.T.columns:
    weights[date] = conso_tri.T[date]*(1/conso_tri.T[date][17])

weights = weights.set_index(conso_tri.T.index).iloc[:-1,:].T

meanWeights = pd.DataFrame(weights.mean(), columns=['Poids moyen']).T

variance = pd.DataFrame(yty_var.iloc[:-1,:].T.var(), columns=["Variance de variations"]).T

share_of_variance = pd.DataFrame()

totalVariance = 0

for i in range(0,17,1):
    totalVariance += variance.T['Variance de variations'][i]*meanWeights.T['Poids moyen'][i]

for column in variance.columns: 
    share_of_variance[column] = pd.Series([variance[column][0]*meanWeights[column][0]*100/totalVariance])

share_of_variance.index = ['Part de la variance en %']
sov_sorted = share_of_variance.T.sort_values(by='Part de la variance en %')[::-1]

# %% Réimportation de toutes les variables

conso = pd.read_csv('Ressources/Consommation des ménages trimestrielle.csv',sep=';')
conso = conso.set_index('Période').replace({',':'.'},regex=True).astype('float64')

""" Import des variables explicatives : goods, services, retail_trade """

goods = pd.read_csv('Ressources/Goods index.csv',sep=';').drop(range(6))
goods = goods.rename({'Titre :':'Date'},axis='columns').set_index('Date').replace({',':'.'},regex=True).astype('float64')

services = pd.read_csv('Ressources/Services index.csv',sep=';').drop(range(4))
services = services.rename({'Titre :':'Date'},axis='columns').set_index('Date').replace({',':'.'},regex=True).astype('float64')

retail_trade = pd.read_csv('Ressources/Retail trade index.csv',sep=';').drop(range(4)).reset_index().drop('index',axis=1)
retail_trade = retail_trade.rename({'Titre :':'Date'},axis='columns').set_index('Date').replace({',':'.'},regex=True).astype('float64')

services_90 = services.loc['Déc 2019':'Jan 1990'].reset_index()
goods_90 = goods.loc[:'01/01/1990'].reset_index()
goods_90['Date'] = retail_trade.reset_index()['Date'] 

""" Constitution d'une dataframe X explicative, et constitution d'une yty-var-indice """

X = retail_trade.merge(services_90,on='Date').merge(goods_90,on='Date').set_index('Date')
X_trimestriel = pd.DataFrame([X.iloc[3*k:3*(k+1)].mean() for k in range(int(len(X)/3))],columns=X.columns,index=conso.index[::-1]).iloc[::-1]

var = []
for i in range(4,len(conso),1):
    var.append(float(conso['TOTAL '].iloc[i]/conso['TOTAL '].iloc[i-4] - 1))
yty_var_tot = pd.DataFrame({'Variations de la consommation':var, 'Date':list(conso.index)[4:]})
yty_var_tot = yty_var_tot.set_index('Date').loc['2011T2':]

variablesOfficielles = X_trimestriel.columns

variablesOfficiellesBrut = [variable for variable in variablesOfficielles if "brut" in variable.lower() ]
variablesOfficiellesCvs = [variable for variable in variablesOfficielles if "cvs" in variable.lower() ]
variablesOfficiellesTendance = [variable for variable in variablesOfficielles if "tendance" in variable.lower() ]

total = len(variablesOfficiellesBrut) + len(variablesOfficiellesCvs) + len(variablesOfficiellesTendance)

# %% Préselection de variables explicatives

def create_subcategories(datasetVariables) : 

    groups = dict()
    groups["Non catégorisés"] = []

    for i in range(len(datasetVariables)) : 
    
        completeVariable = datasetVariables[i]

        separator = ","

        if not completeVariable.find(",") == -1 :
            separator = ","
        elif not completeVariable.find("-") == -1 :
            separator = "-"
        else :
            separator = ""
    
        if not separator == "" :
            cuts = completeVariable.split(separator)
            category = (separator.join(cuts[:len(cuts)-1]),cuts[0])[len(cuts)<3]
            shortVariable = cuts[len(cuts)-1]
                
            if not category in groups : 
                groups[category] = list()
    
            groups[category].append(shortVariable)

        else : 
            groups["Non catégorisés"].append(completeVariable)

    indexCategories = pd.DataFrame(groups.keys(), columns=["categories"])

    indexCategories["length"] = range(len(indexCategories["categories"]))

    for i in range(len(indexCategories["categories"])):
        indexCategories["length"][i] = len(groups[indexCategories["categories"][i]])
    
    return indexCategories, groups

indexCategories, sortedOutInputs = create_subcategories(variablesOfficielles)

agroAlimentaire = [4,10,11,12,51,109]
cokefactionRaffinage = [74,91]
bienEquipement = [18,52,62,80,81]
commerce = [1,2,3,6,13,14,15]
informationCommunication = [16,36,37]
materielTransport = [34,45,86]

def shortlisted_variables_information(subCategories,indexCategories,names=False):

    variablesNumber = 0
    listNames = []

    for element in subCategories:
        variablesNumber += indexCategories.length[element]
        ("",listNames.append(indexCategories.categories[element]))[names]

    return (variablesNumber, (variablesNumber,listNames))[names]

nombreVariablesPreselectionnees = shortlisted_variables_information(agroAlimentaire,indexCategories)
nombreVariablesPreselectionnees += shortlisted_variables_information(cokefactionRaffinage,indexCategories)
nombreVariablesPreselectionnees += shortlisted_variables_information(commerce,indexCategories)
nombreVariablesPreselectionnees += shortlisted_variables_information(bienEquipement,indexCategories)
nombreVariablesPreselectionnees += shortlisted_variables_information(informationCommunication,indexCategories)
nombreVariablesPreselectionnees += shortlisted_variables_information(materielTransport,indexCategories)

def inputs_reconstruction(subCategoryName, variables):

    inputs = []

    for variable in variables:
        inputs.append(",".join([subCategoryName,variable]))

    return inputs

def retrieve_data_frame_inputs(subCategories, indexCategories, sortedOutInputs, brut=True ,):

    subCategoriesVariables = []

    for i in range(len(subCategories)):
        category = indexCategories.categories[subCategories[i]]
        subCategoriesVariables += inputs_reconstruction(category,sortedOutInputs[category])

    subCategoriesVariables = [variable for variable in subCategoriesVariables if not ("cvs","brut")[not brut] in variable.lower()]
        
    return subCategoriesVariables

def analysis_of_variables_tracking_of_consumption_fonction(consumptionFonctionName,selectedSubCategories,indexCategories, sortedOutInputs, brut=True):

    subClassVariations = yty_var.T[consumptionFonctionName].to_frame("variations"+consumptionFonctionName).reset_index().drop(['Période'], axis=1)
    subCategoriesVariables = X_trimestriel[retrieve_data_frame_inputs(selectedSubCategories,indexCategories, sortedOutInputs, brut)]

    #calculs des variations de chacune des variables officielles 
    for column in subCategoriesVariables.columns:
        subClassVariations[column] = (subCategoriesVariables[column].diff(4)/subCategoriesVariables[column].shift(4)).iloc[4:].reset_index().drop('Période', axis = 1)
        

    variance = pd.DataFrame(subClassVariations.var()).T
    variance.index=['Variance de variation']
    
    variablesLesPlusCorrélés = pd.DataFrame(subClassVariations.corr()["variations"+consumptionFonctionName].abs().nlargest(20)).T
    variablesLesPlusCorrélés.index = ["Corrélation avec la fonction de consommation"]

    return variance.T, variablesLesPlusCorrélés.T


variablesSelectionBrut = []

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Produits agro-alimentaires",agroAlimentaire,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Cokéfaction et raffinage",cokefactionRaffinage,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction(" Biens d'équipement",bienEquipement,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Commerce",commerce,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Matériels de transport",materielTransport,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Information-communication",informationCommunication,indexCategories, sortedOutInputs)
variablesSelectionBrut.extend(variables.index[1:10].tolist())

variablesSelectionCvs = []

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Produits agro-alimentaires",agroAlimentaire,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Cokéfaction et raffinage",cokefactionRaffinage,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction(" Biens d'équipement",bienEquipement,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Commerce",commerce,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Matériels de transport",materielTransport,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variance, variables = analysis_of_variables_tracking_of_consumption_fonction("Information-communication",informationCommunication,indexCategories, sortedOutInputs, False)
variablesSelectionCvs.extend(variables.index[1:10].tolist())

variablesSelectionBrut.extend(sortedOutInputs["Non catégorisés"])
variablesSelectionCvs.extend(sortedOutInputs["Non catégorisés"])

variablesSelectionBrut=variablesSelectionBrut[:-3:]

X_trimestriel=X_trimestriel['2010T2':]
var_indice, var_indice_index = [], []
for i in range(4,len(X_trimestriel),1):
    var_indice.append(X_trimestriel.iloc[i]/X_trimestriel.iloc[i-4] - 1)
    var_indice_index.append(X_trimestriel.index[i])
yty_var_indice = pd.DataFrame(var_indice, index=var_indice_index, columns=X_trimestriel.columns)
yty_var_indice = (yty_var_indice - yty_var_indice.mean())/yty_var_indice.std()
yty_var_indice = yty_var_indice.replace([np.inf, -np.inf], np.nan).fillna(0)
X_var=yty_var_indice['2011T2':][::-1]
X_var.index.name='Date'

corr_cvs=abs(yty_var_tot.reset_index().merge(X_var[variablesSelectionCvs].reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False))
selectionCVS=corr_cvs.sort_values(ascending=False).index[0:25]

selectionCVS=selectionCVS.drop("Fabrication d'équipements électriques, taux moyen d'utilisation des capacités de production (CVS)").drop('Commerce de détail, indice en volume (CJO CVS)').drop('Petit commerce, indice en volume (CJO CVS)').drop('Alimentation générale hors boucherie, Hypermarchés, indice en valeur (CJO CVS)').drop("Fabrication d'équipements électriques, évolution des livraisons par rapport au mois précédent (CVS)").drop("Indicateur mensuel du climat des affaires dans l'industrie manufacturière (cvs)")
selectionCVS=selectionCVS.drop('Grande distribution, indice en valeur (CJO CVS)').drop('Supermarchés, indice en volume (CJO CVS)').drop('Petit commerce, indice en valeur (CJO CVS)').drop('Grande distribution, indice en volume (CJO CVS)').drop('Alimentation générale, Petit commerce traditionnel, indice en valeur (CJO CVS)').drop("Equipements électriques et électroniques, autres machines, taux moyen d'utilisation des capacités de production (CVS)")[:-2:]

corr_brut=abs(yty_var_tot.reset_index().merge(X_var[variablesSelectionBrut].reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False))
selectionBrut=corr_brut.sort_values(ascending=False).index[0:25]

corrMatrix = abs(X_var[selectionBrut].corr())
index=corrMatrix.sort_index(axis=0,ascending=True).reset_index()['index']
selectionBrut=selectionBrut.drop(index[19:21]).drop(index[14:16]).drop(index[4:6]).drop(index[10:14]).drop(index[7]).drop(index[22:24])[:-2:]

# %% Format datetime des index
import datetime as dt

lesdates = []
dernierJour = {
    1:31,
    2:28,
    3:31,
    4:30,
    5:31,
    6:30,
    7:31,
    8:31,
    9:30,
    10:31,
    11:30,
    12:31
}
for i in range(len(yty_var_tot)*3):
    annee = 1991+((i+4)//12)
    mois = (i+4)%12
    if mois == 0:
        mois += 12
        annee -= 1
    jour = dernierJour[mois]
    if (annee - 1988)%4 == 0 and mois == 2:
        jour = 29
    lesdates.append(dt.date(annee,mois,jour))
lesdates = pd.Index(lesdates)