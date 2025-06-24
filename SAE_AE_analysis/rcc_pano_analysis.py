
""" This script processes the RCC data from the CantreatCOVID trial, focusing on various aspects such as randomization, baseline characteristics, daily diaries, and hospital visits. 
It replaces empty strings with None, filters data based on specific conditions, and merges datasets to analyze patient outcomes."""


import pandas as pd
import pyspark.pandas as ps
from pyspark.sql import SparkSession
from pyspark.sql.functions import col, coalesce, when, to_date, datediff


def replace_empty_with_none(df):
    """
    Replace empty strings in the DataFrame with None.
    """
    for col_name in df.columns:
        if df[col_name].dtype == 'object':  # String columns
            df[col_name] = df[col_name].replace(['', 'NA', 'N/A', 'na', 'NaN'], None)
    return df


spark = SparkSession.builder \
    .appName("ctc RCC Analysis") \
    .config("spark.sql.ansi.enabled", "false") \
    .getOrCreate()
ps.set_option("compute.fail_on_ansi_mode", False)



## Load the RCC data from the CSV file
spark_df_rcc = spark.read \
    .format("csv") \
    .option("header", "true") \
    .option("sep", ",") \
    .option("quote", "\"") \
    .option("escape", "\"") \
    .option("multiLine", "true") \
    .option("ignoreLeadingWhiteSpace", "true") \
    .option("ignoreTrailingWhiteSpace", "true") \
    .option("mode", "PERMISSIVE") \
    .option("inferSchema", "true") \
    .load("/workspaces/CTC_covid/paxlovid_oxford/panoramic/CTC_data_mapped_for_PANORAMIC_2024-12-04.csv")


# Check first row to see if there's an empty column
first_row = spark_df_rcc.limit(1).collect()[0]
print("First column name:", spark_df_rcc.columns[0])
print("First column value:", first_row[0])


# If first column is empty/unnamed, drop it
if spark_df_rcc.columns[0] == "" or spark_df_rcc.columns[0] == "_c0":
    spark_df_rcc = spark_df_rcc.drop("_c0")
    
# convert to pandas-on-Spark
df_rcc = ps.DataFrame(spark_df_rcc)

df_rcc.dtypes
df_rcc.head()

# Get all unique event names
events = df_rcc['redcap_event_name'].unique().to_list()
events

# check participant numbers
df_rcc['participant_id'].unique().shape ## 716

## Filter for the 'Randomization' event
rcc_random = df_rcc[df_rcc['redcap_event_name']=='Randomization'] 
rcc_random = replace_empty_with_none(rcc_random)
## drop all the all na columns
rcc_random = rcc_random.dropna(axis=1, how='all')
rcc_random.shape ## (716, 5)
# rcc_random_pd = rcc_random.to_pandas()


rcc_random.head()
rcc_random['rand_group'].value_counts() 
"""rand_group
Paxlovid      358
Usual Care    358"""

rcc_random.head(5)


################ filter out baseline characteristic of patients ##################
###################################################
####################################################

rcc_baseline = df_rcc[df_rcc['redcap_event_name']=='Baseline']
rcc_baseline = replace_empty_with_none(rcc_baseline)
rcc_baseline = rcc_baseline.dropna(axis=1, how='all')

rcc_baseline.shape
rcc_baseline.head()

############### filter out usual care group consuming Paxlovid ###############
###################################################
####################################################

rcc_dd_dc = df_rcc[['DD_MedNewCovid_Paxlovid', 'DC_InHospital', 'redcap_event_name', 'participant_id']]
rcc_dd_dc.head()

rcc_dd_dc = rcc_dd_dc.merge(rcc_random[['participant_id', 'rand_group']], on='participant_id', how='left')

rcc_dd_dc['DD_MedNewCovid_Paxlovid'] = rcc_dd_dc['DD_MedNewCovid_Paxlovid'].replace(['', 'NA', 'N/A', 'na', 'NaN'], None)

rcc_dd_dc['DD_MedNewCovid_Paxlovid'].value_counts()
rcc_dd_dc_yes = rcc_dd_dc[rcc_dd_dc['DD_MedNewCovid_Paxlovid']== 'Yes']

rcc_dd_dc_yes.head(len(rcc_dd_dc_yes))
# 11/358 ~= 3.1% of patients of usual care group received Paxlovid

rcc_dd_dc_yes_usual_care = rcc_dd_dc_yes[rcc_dd_dc_yes['rand_group'] == 'Usual Care']
rcc_dd_dc_yes_usual_care.shape #(11, 4)

rcc_dd_dc_yes_usual_care_id_set = set(rcc_dd_dc_yes_usual_care.participant_id.to_list())
rcc_dd_dc_yes_usual_care

# rcc_dd_dc_yes_usual_care = rcc_dd_dc_yes_usual_care[['participant_id', 'redcap_event_name','DD_MedNewCovid_Paxlovid', 'DC_InHospital', 'rand_group']]
# rcc_dd_dc_yes_usual_care.to_pandas().to_csv('/workspaces/CTC_covid/paxlovid_oxford/usual_care_taken_paxlovid_250616.csv', index=False)


########## day 28 primary outcome ################
#####################################################
#####################################################

rcc_day28_inhospital = df_rcc[df_rcc['redcap_event_name'].isin(['Day 28','Day 1','Day 21','Day 4 (Paxlovid only)'])]

rcc_day28_inhospital.shape
rcc_day28_inhospital = replace_empty_with_none(rcc_day28_inhospital)
rcc_day28_inhospital = rcc_day28_inhospital.dropna(axis=1, how='all')

rcc_day28_inhospital['redcap_event_name'].unique().tolist()

rcc_day28_inhospital.columns.tolist()

rcc_day28_inhospital['DC_InHospitalOvernight'] = rcc_day28_inhospital['DC_InHospital']



############# study completion ######################
###################################################
##########################################################

rcc_completion = df_rcc[df_rcc['redcap_event_name']=='Study Completion/Termination']
rcc_completion = replace_empty_with_none(rcc_completion)
rcc_completion.shape
rcc_completion = rcc_completion.dropna(axis=1, how='all')
rcc_completion.head()

rcc_completion_on = rcc_completion[rcc_completion['redcap_study']=='Ontario 1']

rcc_completion_on['end_study_reason'].value_counts()
end_reason_dict = {
    1: 'Adverse Event',
    2: 'Death',
    4: 'Lost to Follow Up',
    5: 'Study Terminated by Sponsor',
    6: 'Participant\'s decision to withdraw',
    99: 'Other'
}



######### 'Concomitant Medications' ################
#####################################################
######################################################


rcc_concomitant = df_rcc[df_rcc['redcap_event_name']=='Concomitant Medications']
rcc_concomitant.shape
rcc_concomitant.dtypes

rcc_concomitant = replace_empty_with_none(rcc_concomitant)
rcc_concomitant = rcc_concomitant.dropna(axis=1, how='all')
rcc_concomitant.head()