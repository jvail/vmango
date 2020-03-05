import pandas as pd
import numpy as np

from openalea.vmango.simulation.fruitmodel.fruit_dry_mass_growth import fruit_dry_mass_growth as CROISSANCE_MS
from openalea.vmango.simulation.fruitmodel.fruit_fresh_mass_growth import fruit_fresh_mass_growth as CROISSANCE_MF

def fruit_growth(
    Jour_Flo,
    Tab_journalier,
    Tab_horaire,
#--------------------- Arguments Croissance MF ------------------------------------
    MF_Init,                        # cf fichier allom?trie fruit dont L < 105 mm,
#--------------------- Arguments Croissance MS ---------------------------------
    envirlum,                       # Evolution de l'environnement lumineux dans la journ?e
    MS_Init_Division_Cellulaire,    # Poids du fruit ? la fin de la division cellulaire en gramme de MS
    MS_Debut_Sim,
    LF,                             # Rapport feuille / fruit [10, 150]
    verbose=False
):


    Tab_journalier = Tab_journalier[Tab_journalier['DATE'] >= Jour_Flo].copy()
     # construction colonne DAB
    Tab_journalier.loc[:,'DAB'] = list(range(len(Tab_journalier.index)))

    # calcul des degr?s jours ? partir DAB = 0
    Tbase = 16
    m = [max(0, tm - Tbase) for tm in Tab_journalier['TM'].tolist()]
    calcddj = np.cumsum(m)
    Tab_journalier.insert(len(Tab_journalier.columns), 'ddj', calcddj)
    # Tab_journalier.to_csv("./r_vs_py_validation/Tab_journalier_py.csv", na_rep='NA', index=False)


    # fin de la multiplication cellulaire dans le fruit (temp?rature base 16 ?C)
    ddjy = 352.72
    # valeur la plus proche et sup?rieure ? ddjy dans Tab_journalier
    ddjini = np.amin(calcddj[calcddj>ddjy])

    DDJ_Init =  ddjini
    Tab_journalier_fruit = Tab_journalier[Tab_journalier['ddj'] >= DDJ_Init]
    # DAB r??lle du fruit ? laquelle d?bute la simulation
    DAB_Init = Tab_journalier_fruit[Tab_journalier_fruit['ddj'] == DDJ_Init]

    Tab_horaire_fruit = Tab_journalier_fruit.merge(Tab_horaire, on='DATE')
    DATE = np.unique(Tab_horaire_fruit['DATE'].to_numpy())
    DAB_sim = np.unique(Tab_horaire_fruit['DAB'].to_numpy())

    #---Initialisation des valeurs des param?tres
    Reserve_Feuille = 0.8 * 0.074 * LF * 0.4051
    Reserve_Rameau  = 0.1 * 0.4387 * (41.83 + 77.41) / 2.0
    Eaupepu = 0.4086 * (MF_Init - MS_Debut_Sim)**0.7428 + 0.5874 * (MF_Init - MS_Debut_Sim)**1.0584

    Croissance = pd.DataFrame({
        'Date': [DATE[0]],
        'Masse_Fruit': [MF_Init],
        'MS_Fruit': [MS_Debut_Sim],
        'Eaupepu': [Eaupepu],
        'Reserve_Feuille': [Reserve_Feuille],
        'Reserve_Rameau': [Reserve_Rameau],
        'Potentiel_Hydrique': [np.nan],
        'P_Turgescence': [np.nan],
        'P_Osmotique': [np.nan],
        'Xyleme': [np.nan],
        'Phloeme': [np.nan],
        'Transpiration': [np.nan],
        'Saccharose': [np.nan],
        'sucres_solubles': [np.nan],
        'acides_organiques': [np.nan]
    })

    MS = pd.DataFrame({
        'MS_Fruit': [MS_Debut_Sim],
        'Reserve_Feuille': [Reserve_Feuille],
        'Reserve_Rameau': [Reserve_Rameau]
    })

    for i in range(len(DATE)):

        date = DATE[i]
        Table_Croissance = Tab_horaire_fruit[Tab_horaire_fruit['DATE'] == date].sort_values('HEURE', ascending=True, inplace=False)
        # Fonction croissance en Mati?re s?che
        MS_Fruit_Precedent = MS['MS_Fruit']

        MS = CROISSANCE_MS (
            Rayonnement = Table_Croissance['Rayonnement'],   # en J.cm-2 cumul? sur l'heure
            Temperature_Air =   Table_Croissance['TM'],      # Temp?rature journali?re de l'air
            Temperature_Fruit = Table_Croissance['TM'],      # Temp?rature journali du fruit.
            envirlum =          envirlum,                               # Evolution de l'environnement lumineux dans la journ?e
            Poids_Fruit_Init =   MS_Init_Division_Cellulaire,           # Poids du fruit ? la fin de la division cellulaire en gramme de MS
            MS_Fruit_Precedent = MS['MS_Fruit'][0],                        # en gramme de MS
            Reserve_Rameau =     MS['Reserve_Rameau'][0],                  # en gramme de carbone
            Reserve_Feuille =    MS['Reserve_Feuille'][0],                 # en gramme de carbone
            LF = LF                                                     # Rapport feuille / fruit [10, 150]
        )

        # Fonction de croissance en Mati?re fraiche
        MF = CROISSANCE_MF(
            Date =              np.unique(Table_Croissance['DATE']),
            Temperature_Air =   Table_Croissance['Temperature_Air'],     # dynamique horaire
            rayo =              Table_Croissance['Rayonnement'],         # dynamique horaire
            humirela =          Table_Croissance['HR'],                  # dynamique horaire
            ddj =               Table_Croissance['ddj'],
            Temp_air_moy =      np.mean(Table_Croissance['TM']),            # donn?e journali?re moyenne
            MSfruit =           [MS_Fruit_Precedent[0], MS['MS_Fruit'][0]],
            MF_Init =           Croissance['Masse_Fruit'].iloc[-1],
            Eaupepu_Init =      Croissance['Eaupepu'].iloc[-1],
            Poids_Fruit_Init =  MS_Init_Division_Cellulaire,                        # Poids du fruit ? la fin de la division cellulaire en gramme de MS
            H = 0.002027 ,                                                          # Pression Seuil Y ~ H * Volume Fruit, pour la croissance, Parametre ? Estimer.
            Phiini = 0.414,                                                         #  Taux d'accroissement cellulaire (MPa / jour, article 2007)
            DDini = 2000,                                                           # DDJ ? partir duquel extensibilit? cellulaire (Phiini d?croit).
            Tau = 0.966,                                                            # Tau de d?croissance de l'extensibilit? cellulaire.
            aLf = 0.3732                                                            # Parametres pour calculer la conductivit? hydraulique (param papier 2007 * 24)
        )

        Croissance.loc[i, 'Potentiel_Hydrique':'acides_organiques'] = MF[0].iloc[0].tolist()
        Croissance.loc[i + 1, 'Date':'Eaupepu'] = MF[1].iloc[0].tolist()

        #### MODIF MAY17
        Croissance.loc[i + 1, 'Reserve_Feuille'] = MS.loc[:,'Reserve_Feuille'].tolist()
        Croissance.loc[i + 1, 'Reserve_Rameau'] = MS.loc[:,'Reserve_Rameau'].tolist()

        if Croissance['Saccharose'][i] >= 0.04:
            # pd.DataFrame([[local[0], type(local[1]).__name__] for local in locals().items()], columns=['name', 'type']).to_csv(f'{__name__ }_vars.csv', index=False, sep=';')
            if verbose == True:
                print('Le fruit est mur')
            # sort de la boucle et arr?te les simulations
            break

    return (DAB_sim, Croissance)
