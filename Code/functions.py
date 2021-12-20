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
