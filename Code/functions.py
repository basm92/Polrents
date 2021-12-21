import pandas as pd
from tqdm import tqdm

def get_margin(dataframe):
    
    out = pd.DataFrame()
    
    for i in tqdm(range(len(dataframe))):
        
        try:
        
            distr = dataframe.iloc[i]['District']
            date = dataframe.iloc[i]['Verkiezingdatum']
        
            if dataframe.iloc[i]['gewonnen'] == 0:
            
                votes_marginal_winner = dataframe[(dataframe['District'] == distr) & 
                  (dataframe['Verkiezingdatum'] == date) &
                  (dataframe['marginal_winner'] == 1)]['Aantal stemmen'].values[0]
            
                margin = (dataframe.iloc[i]['Aantal stemmen'] - votes_marginal_winner)/dataframe.iloc[i]['totaal aantal stemmen']
        
            if dataframe.iloc[i]['gewonnen'] == 1:
        
                votes_marginal_loser = dataframe[(dataframe['District'] == distr) & 
                  (dataframe['Verkiezingdatum'] == date) &
                  (dataframe['marginal_loser'] == 1)]['Aantal stemmen'].values[0]
            
                margin = (dataframe.iloc[i]['Aantal stemmen'] - votes_marginal_loser)/dataframe.iloc[i]['totaal aantal stemmen']
    
        except:
            
            margin = None
            
        interim = dataframe.iloc[i:i+1]
        interim = interim.assign(margin = margin)
            
        out = out.append(interim)
        
    return out
    
def get_match(df_ep):
    
    matches = pd.DataFrame()
    
    #loop through each unique candidates
    for i in tqdm(df_ep['naam'].unique()):
        
        try: 
            
            interim = pd.DataFrame()

            distr_name = df_ep[df_ep['naam'] == i].iloc[0]['districtsnaam']
            verk_dat = df_ep[df_ep['naam'] == i].iloc[0]['verkiezingdatum']

            closest_verk_dat = all_candidates[(all_candidates['District'] == distr_name) & (all_candidates['Verkiezingdatum'] <= verk_dat)]['Verkiezingdatum'].max()
            candidate_matches = all_candidates[(all_candidates['District'] == distr_name) & (all_candidates['Verkiezingdatum'] == closest_verk_dat)]["Naam"].tolist()

            found_match = process.extractOne(i, candidate_matches)[0]

                # find name in all_candidates on basis of winning election
            interim = interim.assign(name_in_elected_people = [i],
                                     name_in_all_elections = [found_match])

            matches = matches.append(interim)

        except:
            next
            
    return(matches)
    

def get_elec_stats(dataframe):
    
    out = pd.DataFrame()
    
    for i in tqdm(range(len(dataframe))):
        
        naam = dataframe.iloc[i]['Naam']
        datum = dataframe.iloc[i]['Verkiezingdatum']
        district = dataframe.iloc[i]['District']
                
        if (datum, district) in consequential_elections:
            consequential_election = 1
        else: 
            consequential_election = 0 
        
        # proberen
        hoeveelste_keer_prob = dataframe[
            (dataframe['Naam'] == naam) & 
            (dataframe['Verkiezingdatum'] <= datum)].shape[0]
        hoeveelste_keer_prob_alg = dataframe[
            (dataframe['Naam'] == naam) & 
            (dataframe['Verkiezingdatum'] <= datum) & 
            (dataframe['Type'] != 'herstemming')].shape[0]
          
        hoeveel_keer_prob_tot = dataframe[dataframe['Naam'] == naam].shape[0]
    
        last = dataframe[dataframe['Naam'] == naam].shape[0] - 1
        
        alle_verkiezingen_voor_deze_persoon = dataframe[(dataframe['Naam'] == naam)].sort_values('Verkiezingdatum')
        # write here the new functions hoelang tussen 1 en laatst and hoelang tussen 1 and 2
        hoelang_tussen_1_en_laatst = alle_verkiezingen_voor_deze_persoon.iloc[last]['Verkiezingdatum'] - alle_verkiezingen_voor_deze_persoon.iloc[0]['Verkiezingdatum']
        
        try:
            hoelang_tussen_1_en_2 = (dataframe[(dataframe['Naam'] == naam) & 
                                               (dataframe['Type'] != 'herstemming')].
                                     iloc[1]['Verkiezingdatum'] - 
                                     dataframe[(dataframe['Naam'] == naam) 
                                               & (dataframe['Type'] != 'herstemming')].
                                     iloc[0]['Verkiezingdatum'])
            
        except:
            hoelang_tussen_1_en_2 = None
        
        
        # gewonnen
        
        if dataframe.iloc[i]['gewonnen'] == 1:
        
            hoeveelste_keer_gewonnen = dataframe[(dataframe['Naam'] == naam) & 
                                             (dataframe['gewonnen'] == 1) &
                                             (dataframe['Verkiezingdatum'] <= datum)].shape[0]
            hoeveelste_keer_gewonnen_alg = dataframe[(dataframe['Naam'] == naam) & 
                                                 (dataframe['gewonnen'] == 1) & 
                                                 (dataframe['Verkiezingdatum'] <= datum) & 
                                                 (dataframe['Type'] != 'herstemming')].shape[0]
        else:
            
            hoeveelste_keer_gewonnen = None
            hoeveelste_keer_gewonnen_alg = None
            
        # hoevaak gewonnen door elected_people
        hoeveelste_keer_gewonnen_tweedeproxy = elected_people[
            (elected_people['naam'] == dataframe.iloc[i]['name_in_elected_people']) &
            (elected_people['verkiezingdatum'] == datum)].shape[0]
        
        hoevaak_gewonnen_alltime = elected_people[elected_people['naam'] == dataframe.iloc[i]['name_in_elected_people']].shape[0]
            
        hoevaak_gewonnen_toekomst = elected_people[
            (elected_people['naam'] == dataframe.iloc[i]['name_in_elected_people']) &
            (elected_people['verkiezingdatum'] > datum)].shape[0]
        hoevaak_gewonnen_verleden = elected_people[
            (elected_people['naam'] == dataframe.iloc[i]['name_in_elected_people']) &
            (elected_people['verkiezingdatum'] < datum)].shape[0]
            
        # volgende verkiezingen (ook zoeken in elected_people)
        elections_participated_in_future = dataframe[(dataframe['Naam'] == naam) & 
                                                           (dataframe['Verkiezingdatum'] > datum) &
                                                           ((dataframe['Type'] == 'algemeen') | 
                                                            (dataframe['Type'] == 'periodiek'))]['Verkiezingdatum'].tolist()
        verk_2_6_gewonnen = [None, None, None, None, None]
        
        for j, val in enumerate(elections_participated_in_future):

                if j > 4:
                    continue

                cur_gewonnen = elected_people[
                    (elected_people['naam'] == dataframe.iloc[i]['name_in_elected_people']) 
                    & (elected_people['verkiezingdatum'] == val)].shape[0]
                verk_2_6_gewonnen[j] = cur_gewonnen               
        
        # put everything together in dataframe
        interim = dataframe.iloc[i:i+1]
        
        interim = interim.assign(consequential_election = consequential_election)
        interim = interim.assign(hoelang_tussen_1_en_2 = hoelang_tussen_1_en_2)
        interim = interim.assign(hoelang_tussen_1_en_laatst = hoelang_tussen_1_en_laatst)
        
        interim = interim.assign(hoeveelste_keer_prob = hoeveelste_keer_prob)
        interim = interim.assign(hoeveelste_keer_prob_alg = hoeveelste_keer_prob_alg)
        
        interim = interim.assign(hoeveelste_keer_gewonnen = hoeveelste_keer_gewonnen)
        interim = interim.assign(hoeveelste_keer_gewonnen_alg = hoeveelste_keer_gewonnen_alg)
        interim = interim.assign(hoeveelste_keer_gewonnen_tweedeproxy = hoeveelste_keer_gewonnen_tweedeproxy)
        interim = interim.assign(hoevaak_gewonnen_alltime = hoevaak_gewonnen_alltime)
        interim = interim.assign(hoevaak_gewonnen_verleden = hoevaak_gewonnen_verleden)
        interim = interim.assign(hoevaak_gewonnen_toekomst = hoevaak_gewonnen_toekomst)
        interim = interim.assign(verk_2_6_gewonnen = [verk_2_6_gewonnen])
        
                                                         
        out = out.append(interim)
        
    return out
        
