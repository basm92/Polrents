import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# Read the data sources
all_margins = pd.read_csv("./Data/elections/allmargins.csv").iloc[:,2:]
#all_elected = pd.read_csv("./Data/elections/allelected.csv")
#all_elected['voornaam'] = all_elected['voornaam'].str.strip()
#all_elected['Naam'] = all_elected['voornaam'] + ' ' + all_elected['achternaam']

all_elected = all_margins[all_margins['margin']> 0 ].Naam.unique()

# Set parameter margin:
closemargin = 0.2

## Close elections in general

### How many close elections? 
all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District', 'Verkiezingdatum']).size().reset_index().iloc[:,0:2].count()

### Per type
all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District', 'Verkiezingdatum','Type']).size().reset_index().iloc[:,0:3]

### Where are the close elections, in which districts?
all_margins[abs(all_margins['margin']) <  closemargin].groupby(['District']).Verkiezingdatum.nunique().reset_index()

### In what years are the close elections? 
all_margins['election_year'] = all_margins['Verkiezingdatum'].str.split("/").apply(lambda x: x[2])
years_withpols = all_margins[abs(all_margins['margin']) < closemargin].groupby(['election_year']).District.nunique()

### In what district-years?
all_margins[abs(all_margins['margin']) < closemargin].groupby(['District','election_year']).Verkiezingdatum.nunique().reset_index()


## Close elections with non-politicians

#### Filter dataframe to negative margins (losers)
margins_without_politicians = all_margins[(abs(all_margins['margin']) < closemargin) & (all_margins['margin'] < 0)]
list_elected = all_elected

#### Check whether each of the names in list_elected matches a given name in margins_without_politicians, and filter
losers = margins_without_politicians[margins_without_politicians.apply(lambda row: all(a not in row['Naam'] for a in list_elected), axis = 1)]

### How many close electıons wıth only losers?
losers.groupby(['District', 'Verkiezingdatum']).size().reset_index().iloc[:,0:2].count()

### Per Type?
losers.groupby(['District', 'Verkiezingdatum','Type']).size().reset_index().iloc[:,0:3]

### Where are the close elections, in which districts?
losers.groupby(['District']).Verkiezingdatum.nunique().reset_index()

### In which years do close elections take place?
years_nopols = losers.groupby(['election_year']).District.nunique()


### Create a figure: Close elections - two district heatmaps

### One cumulative timemap 

#### Data for first subplot
years_nopols = years_nopols.reset_index()
years_nopols['District'] = years_nopols.District.cumsum()

years_withpols = years_withpols.reset_index()
years_withpols['District'] = years_withpols.District.cumsum()

data = pd.merge(years_nopols, years_withpols, how = "outer", left_on="election_year", right_on="election_year", sort = True)
data = data.rename(columns={'election_year':'Year', 'District_x':'Without Politicians', 'District_y':'With Politicians'})
data['Year'] = data['Year'].apply(lambda x: pd.Timestamp(x))
data.fillna(method='backfill', inplace=True)

#### First subplot
plt.plot(data['Year'], data['Without Politicians'], color = 'blue', label = 'Without Future Politicians')
plt.plot(data['Year'], data['With Politicians'], color = 'orange', label = 'With Future Politicians')
plt.legend()
plt.title("Cumulative Close Elections (20% Margin) With and Without Future Politicians")
plt.show()

#### Data for second subplot: new candidates per year
all_margins['election_year'] = all_margins['election_year'].apply(lambda x: pd.to_numeric(x))
pd.DataFrame({'election_year':range(1848, 1917), 'new_candidates':None})

all_margins[all_margins['election_year'] == '1848']



## Two district heatmaps in one figure


