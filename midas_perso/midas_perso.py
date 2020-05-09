import numpy as np
import pandas as pd


def phi(J,m,theta):

    ''' Renvoie un pd.Series contenant les valeurs de phi(i,theta) pour i allant de 1 à J, 
    avec phi la fonction polynomiale exponentielle d'Almon. m est le nombre d'occurrences de la variable haute fréquence 
    par occurrence de la variable basse fréquence
    
    On attend que J soit un entier stricement positif, et theta un vecteur à 2 éléments qui sont des floats. '''
    convenient_series = pd.Series([np.exp(theta[0]*step + (theta[1]*(step**2))) for step in np.arange(1,m+1)])
    phi = []
    for step in range(J):
        phi.append(convenient_series.iloc[step]/convenient_series.sum())
    phi = pd.Series(phi)

    return phi

def deriv1_phi(J,m,theta):

    ''' Calcul de la première dérivée partielle de la fonction phi(j,theta), pour j variant de 1 à J '''

    deriv1_phi = []
    for step in range(J):
        deriv1_phi.append(np.exp(theta[0]*step + (theta[1]*(step**2)))*(pd.Series([(step-step2) * np.exp(theta[0]*step2 + theta[1]*(step2**2)) for step2 in np.arange(1,m+1)])).sum() / (((pd.Series([np.exp(theta[0]*step2 + theta[1]*(step2**2)) for step2 in np.arange(1,m+1)]).sum())**2)))

    deriv1_phi = pd.Series(deriv1_phi)

    return deriv1_phi

def deriv2_phi(J,m,theta):

    ''' Calcul de la deuxième dérivée partielle de la fonction phi(j,theta), pour j variant de 1 à J '''

    deriv2_phi = []
    for step in range(J):
        deriv2_phi.append(np.exp(theta[0]*step + (theta[1]*(step**2)))*(pd.Series([(step-step2) * (step+step2) * np.exp(theta[0]*step2 + theta[1]*(step2**2)) for step2 in np.arange(1,m+1)])).sum() / (((pd.Series([np.exp(theta[0]*step2 + theta[1]*(step2**2)) for step2 in np.arange(1,m+1)]).sum())**2)))

    deriv2_phi = pd.Series(deriv2_phi)

    return deriv2_phi

def jacobian_midas(x,y,p_x,p_y,m,J,params):
    ''' We assume x is a dataframe of explanatory variables, y is a series of target variable (univariate) '''
    T = len(y)
    k = len(x.columns)

    theta1 = params.iloc[-2]
    theta2 = params.iloc[-1]
    theta = [theta1,theta2]

    jacobian = pd.DataFrame()

    for t in np.arange(max(p_x,p_y)+1,T): # On construit la matrice ligne par ligne

        ligne_t = [1] # On ajoute la dérivée par rapport à la constante alpha_0 , 1


        for i in np.arange(1,p_y+1):

            ligne_t.append(y.iloc[t-i]) # On ajoute les dérivées par rapport aux alphas_i


        for i in np.arange(0,k):
            a = x.iloc[(t-1)*m + 1 : (t-1)*m + J + 1, i]
            b = phi(J,m,theta)
            a.index = b.index
            ligne_t.append(b @ a) # On ajoute les dérivées par rapport aux gamma_LEAD
        

        for i in np.arange(0,k):

            for c in np.arange(1,p_x+1):
                a = x.iloc[(t-c)*m-m:(t-c)*m,i]
                b = phi(m, m, theta)
                a.index = b.index
                ligne_t.append(b @ a) # On ajoute les dérivées par rapport aux gamma_LAG


        deriv1 = 0 # Cette variable va contenir la dérivée par rapport à theta1

        for i in np.arange(0,k):
            a = x.iloc[(t-1)*m + 1 : (t-1)*m + J + 1, i]
            b = deriv1_phi(J,m,theta)
            a.index = b.index
            deriv1 -= params.iloc[p_y+i+1] * (b @ a)
        
        for i in np.arange(0,k):

            for c in np.arange(1,p_x+1):
                a = x.iloc[(t-c)*m - m : (t-c)*m, i]
                b = deriv1_phi(m,m,theta)
                a.index = b.index
                deriv1 -= params.iloc[p_y+k+c] * (b @ a)
        
        ligne_t.append(deriv1)

        deriv2 = 0

        for i in np.arange(0,k):
            a = x.iloc[(t-1)*m + 1 : (t-1)*m + J + 1, i]
            b = deriv2_phi(J,m,theta)
            a.index = b.index
            deriv2 += params.iloc[p_y+i+1] * (b @ a)
        
        for i in np.arange(0,k):

            for c in np.arange(1,p_x+1):
                a = x.iloc[(t-c)*m - m : (t-c)*m, i]
                b = deriv2_phi(m,m,theta)
                a.index = b.index
                deriv2 += params.iloc[p_y+k+c] * (b @ a)

        ligne_t.append(deriv2)
    
        jacobian[t] = pd.Series(ligne_t)

    jacobian = jacobian.T

    return jacobian

def nlsquares(x,y,p_x,p_y,m,J,seuil=1,gardefouseuil=1000,init=None):

    ''' Renvoie l'estimateur par moindres carrés non-linéaires par l'algorithme de Gauss-Newton '''
    k = len(x.columns)
    if init is None:
        params = [1/(1+p_y+ (k*(p_x+1))) for i in range(1+p_y+ (k*(p_x+1)))]
        params.append(-1.0)
        params.append(0.0)
        params = pd.Series(params)
    elif len(params) != 3+p_y+ (k*(p_x+1)):
        params = [1/(1+p_y+ (k*(p_x+1))) for i in range(1+p_y+ (k*(p_x+1)))]
        params.append(-1.0)
        params.append(0.0)
        params = pd.Series(params)
    else:
        params = pd.Series(init)

    theta1 = params.iloc[-2]
    theta2 = params.iloc[-1]
    theta = [theta1,theta2]

    params2 = [] # On constitue le vecteurs des betas, calculés à partir des paramètres

    for i in range(p_y+1):

        params2.append(params.iloc[i])

    for i in range(k):
        params2 += list(params.iloc[p_y+i+1] * phi(J,m,theta))

    for i in range(k):
        for c in np.arange(1,p_x+1):
            params2 += list(params.iloc[p_y+k+1+c] * phi(m,m,theta))

    params2 = pd.Series(params2)

    inputs = pd.DataFrame()

    for t in np.arange(max(p_x,p_y)+1, len(y)): # On constitue les vecteurs des inputs, pour pouvoir faire le produit scalaire avec le vecteur des betas
        
        inputline = [1]
        
        for i in range(p_y):
        
            inputline.append(y.iloc[t-1-i])
        
        for i in range(k):

            for j in np.arange(1,J+1):

                inputline.append(x.iloc[(t-1)*m+j,i])
            
        for i in range(k):

            for c in range(p_x):

                for j in range(m):

                    inputline.append(x.iloc[(t-1-c)*m - j-1,i])
        
        inputs[t] = pd.Series(inputline)
    
    inputs = inputs.T

    # On calcule les résidus de Y - Xb
    # On commence par poser les données Y

    b = y.iloc[max(p_x,p_y)+1:]
    b.index = range(len(y)-max(p_x,p_y)-1)
    a = inputs @ params2
    a.index = b.index
    residuals = b - a

    # On calcule la norme des résidus, et on va faire une descente de Gauss-Markov

    norme = np.sqrt((residuals**2).sum()) # Norme 2
    gardefoucompteur = 0

    while norme > seuil:
        gardefoucompteur += 1
        if gardefoucompteur > gardefouseuil:
            break
        
        jacob = jacobian_midas(x, y, p_x, p_y, m, J, params)

        firstfactor = np.linalg.inv(jacob.T @ jacob)

        a = residuals
        b = jacob

        a.index = b.index
        
        secondfactor = (b.T) @ (a.T)
        

        params += pd.Series(firstfactor @ secondfactor)

        params2 = [] # On constitue le vecteurs des betas, calculés à partir des paramètres

        for i in range(p_y+1):

            params2.append(params.iloc[i])

        theta1 = params.iloc[-2]
        theta2 = params.iloc[-1]
        theta = [theta1,theta2]

        for i in range(k):
            params2 += list(params.iloc[p_y+i+1] * phi(J,m,theta))

        for i in range(k):
            for c in np.arange(1,p_x+1):
                params2 += list(params.iloc[p_y+k+1+c] * phi(m,m,theta))

        

        params2 = pd.Series(params2)

        b = y.iloc[max(p_x,p_y)+1:]
        b.index = range(len(y)-max(p_x,p_y)-1)
        a = inputs @ params2
        a.index = b.index
        residuals = b - a

        norme = np.sqrt((residuals**2).sum())

    return params, params2
