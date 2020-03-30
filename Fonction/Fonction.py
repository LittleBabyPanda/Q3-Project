#!/usr/bin/env python
# coding: utf-8

# # Création d'une fonction toute faite

# In[1]:


get_ipython().run_line_magic('matplotlib', 'inline')
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.linear_model import Lasso
from sklearn.preprocessing import StandardScaler


# In[2]:


conso = pd.read_csv('Ressources/Consommation des ménages trimestrielle.csv',sep=';')
conso=conso.set_index('Période').replace({',':'.'},regex=True).astype('float64')


# In[3]:


var = []
for i in range(4,len(conso),1):
    var.append(float(conso['TOTAL '].iloc[i]/conso['TOTAL '].iloc[i-4] - 1))
yty_var = pd.DataFrame({'Variations de la consommation':var, 'Date':list(conso.index)[4:]})
yty_var = yty_var.set_index('Date').loc['2010T2':]


# In[6]:


def graphiques(df):
    #trimestrialisation du df
    index=[]
    annee=2019
    for i in range(int(len(df)/12)+1):
        for j in range(4,0,-1):
            periode=str(annee)+'T'+str(j)
            index.append(periode)
        annee-=1
    del(index[-1])
    tri=pd.DataFrame([df.iloc[3*k:3*(k+1)].mean() for k in range(int(len(df)/3))],columns=df.columns,index=index)
    tri=tri.fillna(method='ffill')[::-1]

    
    #Normalisation du set de variables explicatives
    tri_norm=(tri-tri.mean())/tri.std()
    tri_norm=tri_norm.fillna(0)
    
    #Préparation des outils nécessaires à la régression
    var_indice, var_indice_index = [], []
    for i in range(4,len(tri),1):
        var_indice.append(tri.iloc[i]/tri.iloc[i-4] - 1)
        var_indice_index.append(tri.index[i])
    yty_var_indice = pd.DataFrame(var_indice, index=var_indice_index, columns=tri.columns)
    yty_var_indice = (yty_var_indice - yty_var_indice.mean())/yty_var_indice.std()
    yty_var_indice= yty_var_indice.replace([np.inf, -np.inf], np.nan).fillna(0)
    
    
    #Régression Lasso en niveaux 
    coefs = pd.DataFrame(index=tri_norm.columns)
    alphas = np.linspace(0,0.01)
    nbr_coef_non_nuls = []

    for alpha in alphas:
        clf = Lasso(alpha=alpha)
        clf.fit(tri_norm, yty_var)
        coefs[str(alpha)] = clf.coef_
        nbr_coef_non_nuls.append(np.count_nonzero(clf.coef_))

    fig_lvl, (ax,ax_nbr_coefs) = plt.subplots(2,1,figsize=(15,8))

    coefs.T.plot(ax=ax,legend=False,title="Coefficients de la régression Lasso (en niveaux) en fonction du paramètre de pénalisation")

    ax_nbr_coefs.set_title("Nombre de coefficients non-nuls en fonction de la pénalisation (en niveaux)")
    ax_nbr_coefs.set_ylim(0,200)
    ax_nbr_coefs.hlines(34,0,0.01,label='34 coefficients',color='darkred')
    ax_nbr_coefs.plot(alphas,nbr_coef_non_nuls,label='Nombre de coefficients non-nuls',color='darkblue')
    plt.legend()    
    
    plt.show() 
    
    #Détermination des différents modèles 
    coefs2 = pd.DataFrame(index=tri_norm.columns)
    alphas = [0.00010204,0.00112245,0.0022449,0.00326531]

    for alpha in alphas:
        clf = Lasso(alpha=alpha)
        clf.fit(tri_norm, yty_var)
        coefs2[str(alpha)] = clf.coef_

    variables_modele_tres_complexe = list(coefs2.loc[coefs2['0.00010204']!=0].index)
    variables_modele_complexe = list(coefs2.loc[coefs2['0.00112245']!=0].index)
    variables_modele_simple = list(coefs2.loc[coefs2['0.0022449']!=0].index)
    variables_modele_tres_simple = list(coefs2.loc[coefs2['0.00326531']!=0].index)
    
    
    
    #Régression Lasso en variations
    coefs_var = pd.DataFrame(index=yty_var_indice.columns)
    alphas_var = np.linspace(0,0.005)
    nbr_coef_non_nuls_var = []

    for alpha in alphas_var:
        clf_var = Lasso(alpha=alpha)
        clf_var.fit(yty_var_indice, yty_var['2011T2':])
        coefs_var[str(alpha)] = clf_var.coef_
        nbr_coef_non_nuls_var.append(np.count_nonzero(clf_var.coef_))

    fig2, (ax2,ax3) = plt.subplots(2,1,figsize=(15,8))

    coefs_var.T.plot(ax=ax2,legend=False,title="Coefficients de la régression Lasso (en variations) en fonction du paramètre de pénalisation",figsize=(20,15))

    ax3.set_title("Nombre de coefficients non-nuls en fonction de la pénalisation (en variations)")
    ax3.set_ylim(0,200)
    ax3.hlines(32,0,0.01,label='32 coefficients',color='darkred')
    ax3.plot(alphas_var,nbr_coef_non_nuls_var,label='Nombre de coefficients non-nuls',color='darkblue')
    plt.legend()

    plt.show()
    
    #Détermination des différents modèles
    coefs_var2 = pd.DataFrame(index=yty_var_indice.columns)
    alphas = [0.00010204,0.00112245,0.0022449,0.00326531]

    for alpha in alphas:
        clf_var = Lasso(alpha=alpha)
        clf_var.fit(yty_var_indice, yty_var['2011T2':])
        coefs_var2[str(alpha)] = clf_var.coef_

    variables_modele_tres_complexe_var = list(coefs_var2.loc[coefs_var2['0.00010204']!=0].index)
    variables_modele_complexe_var = list(coefs_var2.loc[coefs_var2['0.00112245']!=0].index)
    variables_modele_simple_var = list(coefs_var2.loc[coefs_var2['0.0022449']!=0].index)
    variables_modele_tres_simple_var = list(coefs_var2.loc[coefs_var2['0.00326531']!=0].index)
    
    
    
    #Concaténation des catégories retenues
    tres_complexe = list(pd.DataFrame(variables_modele_tres_complexe_var + variables_modele_tres_complexe).drop_duplicates().iloc[:,0])
    complexe = list(pd.DataFrame(variables_modele_complexe_var + variables_modele_complexe).drop_duplicates().iloc[:,0])
    simple = list(pd.DataFrame(variables_modele_simple_var + variables_modele_simple).drop_duplicates().iloc[:,0])
    tres_simple = list(pd.DataFrame(variables_modele_tres_simple + variables_modele_tres_simple).drop_duplicates().iloc[:,0])

    total = list(pd.DataFrame(tres_complexe + complexe + simple + tres_simple).drop_duplicates().iloc[:,0])


    X_trim_new = tri[total][::-1]
    X_trim_new.index.name='Date'
    X_var_new = yty_var_indice[total][::-1]
    X_var_new.index.name='Date'

    #On les met dans l'ordre correspondant
    yty_var_lvl = yty_var.loc[::-1]
    yty_var_var = yty_var['2011T2':].loc[::-1]
    
    
    #Plot des variables en niveaux
    sns.set()

    figure, axes = plt.subplots(figsize=(25,25))

    axes.set_title("Coefficients de corrélation des 71 (étude en niveaux) variables")

    sns.barplot(yty_var_lvl.reset_index().merge(X_trim_new.reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False),yty_var_lvl.reset_index().merge(X_trim_new.reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False).index,palette='BuGn_r',ax=axes)

    plt.show()
    
    
    
    
    #Plot des variables en variations
    sns.set()

    figure, axes2 = plt.subplots(figsize=(25,25))

    axes2.set_title("Coefficients de corrélation des 71 variables")

    sns.barplot(yty_var_lvl.reset_index().merge(X_trim_new.reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False),yty_var_lvl.reset_index().merge(X_trim_new.reset_index(),on='Date').corr().iloc[1:,0].sort_values(ascending=False).index,palette='BuGn_r',ax=axes2)

    plt.show()
    
    
    
    #Graphiques d'études de la corrélation:
    sns.set()
    figure3, axes3 = plt.subplots(71,1,figsize=(18,300),sharex=True)
    axes3twin = [None] * len(axes3)

    for i in range(len(axes3)):
        axes3[i].plot(yty_var_var[::-1],color="darkblue",label="Variation de la consommation")
        axes3twin[i] = axes3[i].twinx()
        axes3twin[i].plot(X_var_new[::-1].iloc[:,i],color="darkred",label="Variation de l'indice")
        axes3[i].set_title(X_var_new[::-1].columns[i])

    plt.show()
    
    return 

