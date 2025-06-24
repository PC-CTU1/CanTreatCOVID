### This script processes MedDRA coded safety events from an Excel file,
# merges them with randomization data, and performs analysis on adverse events (AEs) and
# serious adverse events (SAEs) by randomization group. It outputs the results to a new Excel file.

import pandas as pd

meddra_ae = pd.read_excel('/workspaces/CTC_covid/paxlovid_oxford/SAEs/CanTreatCOVID_MedDRA_coded_safety_events_19Nov2024.xlsx', sheet_name='CTC - AEs', skiprows=2)

meddra_sae = pd.read_excel('/workspaces/CTC_covid/paxlovid_oxford/SAEs/CanTreatCOVID_MedDRA_coded_safety_events_19Nov2024.xlsx', sheet_name='CTC - SAEs', skiprows=2)


meddra_ae.shape
meddra_ae.dropna(axis=1, how='all', inplace=True)

# Check if 'participant_id' exists in the dataframe
if 'participant_id' in meddra_ae.columns:
    # Fill NA values in participant_id with the nearest forward value
    meddra_ae['participant_id'] = meddra_ae['participant_id'].ffill()
else:
    print("Warning: 'participant_id' column not found in the dataframe")

# Also apply the same to the SAE dataframe if needed
if 'participant_id' in meddra_sae.columns:
    meddra_sae['participant_id'] = meddra_sae['participant_id'].ffill()
else:
    print("Warning: 'participant_id' column not found in the SAE dataframe")
    
    
meddra_ae['participant_id'].isna().sum(), meddra_sae['participant_id'].isna().sum() ## check no missing participant_id values

meddra_ae = meddra_ae.merge(rcc_random_pd[['participant_id', 'rand_group']], on='participant_id', how='left')

meddra_sae = meddra_sae.merge(rcc_random_pd[['participant_id', 'rand_group']], on='participant_id', how='left')



########################################################################
# Analysis for Adverse Events (AE)
# Count total number of AEs per randomization group
ae_counts = meddra_ae.groupby('rand_group').size().reset_index(name='num_adverse_events')

# Count number of participants with at least one AE per randomization group
participants_with_ae = meddra_ae.groupby('rand_group')['participant_id'].nunique().reset_index(name='num_participants_with_ae')



# Analysis for Serious Adverse Events (SAE)
# Count total number of SAEs per randomization group
sae_counts = meddra_sae.groupby('rand_group').size().reset_index(name='num_serious_adverse_events')

# Count number of participants with at least one SAE per randomization group
participants_with_sae = meddra_sae.groupby('rand_group')['participant_id'].nunique().reset_index(name='num_participants_with_sae')


# Combine all results
results = ae_counts.merge(participants_with_ae, on='rand_group')
results = results.merge(sae_counts, on='rand_group', how='outer')
results = results.merge(participants_with_sae, on='rand_group', how='outer')

# Fill any NaN values with 0 and convert to integers
results.fillna(0, inplace=True)
count_columns = ['num_adverse_events', 'num_participants_with_ae', 
                 'num_serious_adverse_events', 'num_participants_with_sae']
results[count_columns] = results[count_columns].astype(int)

# Display the results
print("Summary of Adverse Events by Randomization Group:")
print(results)


# Save the processed dataframes to a new Excel file
output_excel_path = '/workspaces/CTC_covid/paxlovid_oxford/SAEs/CanTreatCOVID_MedDRA_coded_safety_events_19Nov2024.xlsx'

with pd.ExcelWriter(output_excel_path, engine='openpyxl') as writer:
    meddra_ae.to_excel(writer, sheet_name='Processed_AEs', index=False)
    meddra_sae.to_excel(writer, sheet_name='Processed_SAEs', index=False)
    results.to_excel(writer, sheet_name='Summary_Results', index=False)

print(f"Processed data saved to {output_excel_path}")