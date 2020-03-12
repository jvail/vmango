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
    verbose=False,
    idsimu=np.nan
):

    Tab_journalier = Tab_journalier.drop(Tab_journalier[Tab_journalier['DATE'] < Jour_Flo].index)
    Tab_horaire = Tab_horaire.drop(Tab_horaire[Tab_horaire['DATE'] < Jour_Flo].index)
    # Tab_journalier = Tab_journalier[Tab_journalier['DATE'] >= Jour_Flo]
     # construction colonne DAB
    Tab_journalier.reset_index(inplace=True, drop=True)
    Tab_journalier.loc[:,'DAB'] = Tab_journalier.index.copy()

    # calcul des degr?s jours ? partir DAB = 0
    Tbase = 16
    Tab_journalier.loc[:,'ddj'] = np.cumsum([max(0, tm - Tbase) for tm in Tab_journalier['TM'].values])
    # m = [max(0, tm - Tbase) for tm in Tab_journalier['TM'].tolist()]
    # calcddj = np.cumsum(m)
    # Tab_journalier.insert(len(Tab_journalier.columns), 'ddj', calcddj)
    # Tab_journalier.to_csv("./r_vs_py_validation/Tab_journalier_py.csv", na_rep='NA', index=False)


    # fin de la multiplication cellulaire dans le fruit (temp?rature base 16 ?C)
    ddjy = 352.72
    Tab_journalier_fruit = Tab_journalier.drop(Tab_journalier[Tab_journalier['ddj'] < ddjy].index)
    # valeur la plus proche et sup?rieure ? ddjy dans Tab_journalier
    # ddjini = np.amin(calcddj[calcddj>ddjy])

    # DDJ_Init =  ddjini
    # Tab_journalier_fruit = Tab_journalier[Tab_journalier['ddj'] >= DDJ_Init]
    # DAB r??lle du fruit ? laquelle d?bute la simulation
    # DAB_Init unuses it seems
    # DAB_Init = Tab_journalier_fruit[Tab_journalier_fruit['ddj'] == DDJ_Init]

    Tab_horaire_fruit = Tab_journalier_fruit.merge(Tab_horaire, on='DATE')

    DAB = Tab_horaire_fruit['DAB'].unique()
    Rayonnement = Tab_horaire_fruit['Rayonnement'].values
    TM = Tab_horaire_fruit['TM'].values
    Temperature_Air = Tab_horaire_fruit['Temperature_Air'].values
    HR = Tab_horaire_fruit['HR'].values
    ddj = Tab_horaire_fruit['ddj'].values

    #---Initialisation des valeurs des param?tres
    Reserve_Feuille = 0.8 * 0.074 * LF * 0.4051
    Reserve_Rameau  = 0.1 * 0.4387 * (41.83 + 77.41) / 2.0
    Eaupepu_Init = 0.4086 * (MF_Init - MS_Debut_Sim)**0.7428 + 0.5874 * (MF_Init - MS_Debut_Sim)**1.0584

    Croissance = pd.DataFrame({
        'Date': [Tab_horaire_fruit['DATE'].iat[0]],
        'Masse_Fruit': [MF_Init],
        'MS_Fruit': [MS_Debut_Sim],
        'Eaupepu': [Eaupepu_Init],
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

    MS = (MS_Debut_Sim, (Reserve_Feuille, Reserve_Rameau))

    for i, date in enumerate(Tab_horaire_fruit['DATE'].unique()):

    # for i in range(int(len(Tab_horaire_fruit.index) / 24)):
    # for i, daily in Tab_horaire_fruit.iloc[::24].iterrows():
        # nb_day = int(i/24)
        start = i * 24
        stop = start + 24
        # Table_Croissance = Tab_horaire_fruit.iloc[start:stop]
        # date = DATE[i]
        # date = Tab_horaire_fruit['DATE'].iat[i*24]
        # Table_Croissance = Tab_horaire_fruit[Tab_horaire_fruit['DATE'] == date].sort_values('HEURE', ascending=True, inplace=False)
        # Table_Croissance = Tab_horaire_fruit[Tab_horaire_fruit['DATE'] == date]
        # Fonction croissance en Mati?re s?che
        MS_Fruit_Precedent = MS[0]
        Rayonnement_ = Rayonnement[start:stop]
        TM_ = TM[start:stop]
        Temperature_Air_ = Temperature_Air[start:stop]
        HR_ = HR[start:stop]
        ddj_ = ddj[start:stop]

        MS = CROISSANCE_MS (
            Rayonnement = Rayonnement_,   # en J.cm-2 cumul? sur l'heure
            Temperature_Air =   TM_,      # Temp?rature journali?re de l'air
            Temperature_Fruit = TM_,      # Temp?rature journali du fruit.
            envirlum =          envirlum,                               # Evolution de l'environnement lumineux dans la journ?e
            Poids_Fruit_Init =   MS_Init_Division_Cellulaire,           # Poids du fruit ? la fin de la division cellulaire en gramme de MS
            MS_Fruit_Precedent = MS[0],                        # en gramme de MS
            Reserve_Rameau =     MS[1][1],                  # en gramme de carbone
            Reserve_Feuille =    MS[1][0],                 # en gramme de carbone
            LF = LF,                                                     # Rapport feuille / fruit [10, 150]
            idsimu=idsimu
        )

        # Fonction de croissance en Mati?re fraiche
        MF = CROISSANCE_MF(
            Date =              date, #np.unique(Table_Croissance['DATE']),
            Temperature_Air =   Temperature_Air_,     # dynamique horaire
            rayo =              Rayonnement_,         # dynamique horaire
            humirela =          HR_,                  # dynamique horaire
            ddj =               ddj_,
            Temp_air_moy =      np.mean(TM_),            # donn?e journali?re moyenne
            MSfruit =           (MS_Fruit_Precedent, MS[0]),
            MF_Init =           MF_Init, #Croissance['Masse_Fruit'].iloc[-1],
            Eaupepu_Init =      Eaupepu_Init, #Croissance['Eaupepu'].iloc[-1],
            Poids_Fruit_Init =  MS_Init_Division_Cellulaire,                        # Poids du fruit ? la fin de la division cellulaire en gramme de MS
            H = 0.002027 ,                                                          # Pression Seuil Y ~ H * Volume Fruit, pour la croissance, Parametre ? Estimer.
            Phiini = 0.414,                                                         #  Taux d'accroissement cellulaire (MPa / jour, article 2007)
            DDini = 2000,                                                           # DDJ ? partir duquel extensibilit? cellulaire (Phiini d?croit).
            Tau = 0.966,                                                            # Tau de d?croissance de l'extensibilit? cellulaire.
            aLf = 0.3732                                                            # Parametres pour calculer la conductivit? hydraulique (param papier 2007 * 24)
        )

        Croissance.loc[i, 6:15] = MF[0]
        Croissance.loc[i + 1, 0:6] = MF[1] + MS[1]

        MF_Init = MF[1][1]
        Eaupepu_Init = MF[1][3]
        Saccharose = MF[0][6]
        #### MODIF MAY17
        # Croissance.loc[i + 1, 'Reserve_Feuille'] = MS[1]
        # Croissance.loc[i + 1, 'Reserve_Rameau'] = MS[2]


        if Saccharose >= 0.04:
            # pd.DataFrame([[local[0], type(local[1]).__name__] for local in locals().items()], columns=['name', 'type']).to_csv(f'{__name__ }_vars.csv', index=False, sep=';')
            if verbose == True:
                print('Le fruit est mur')
            # sort de la boucle et arr?te les simulations
            break

    return (DAB, Croissance)
