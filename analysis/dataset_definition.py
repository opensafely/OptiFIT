from ehrql import create_dataset, codelist_from_csv, years
from ehrql.tables.core import (
    clinical_events,
    medications,
    ons_deaths,
    patients,
    practice_registrations,
)
from ehrql.tables.tpp import (
    addresses,
    apcs,
    apcs_cost,
    appointments,
    clinical_events,
    clinical_events_ranges,
    covid_therapeutics,
    decision_support_values,
    ec,
    ec_cost,
    emergency_care_attendances,
    ethnicity_from_sus,
    household_memberships_2020,
    medications,
    occupation_on_covid_vaccine_record,
    ons_deaths,
    opa,
    opa_cost,
    opa_diag,
    opa_proc,
    open_prompt,
    parents,
    patients,
    practice_registrations,
    sgss_covid_all_tests,
    ukrr,
    vaccinations,
    wl_clockstops,
    wl_openpathways,
)

# create dummy data
# opensafely exec ehrql:v1 generate-dataset analysis/dataset_definition.py

# Define codelists
# note should probably restrict to just this FIT code 1049361000000101 (see nhs refsets code list)
# as seems to be the used SNOMED code for FIT test in TPP

fit_codes = codelist_from_csv(
    "codelists/user-joewest-fit-and-fob-snomed.csv",
    column="code",
)
single_fit_code = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-faecimm_cod.csv",
    column="code",
)

single_fit_code = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-faecimm_cod.csv",
    column="code",
)

# Define the study population
# Note: patients must have a practice registration that spans the enter_date and 2026-01-01,
# and be aged 18 or older at the time of their first FIT test
enter_date = "2019-01-01"

# Note: the end date is set to 2026-01-01 to allow follow up of 1 year until end 2026 from 1st January 2025 (the end of the fit inclusion
#  period) this may need to change if HES data only available until sometime in 2025
# this sets the population to only include patients who have a practice registration that spans the enter_date and 2026-01-01
# Note: this will need testing if it is the right registration condition to use, but it seems to be the right one to use for now
has_registration = practice_registrations.spanning(enter_date, "2026-01-01").exists_for_patient()

# Define the future clinical events for each patient where the event date is on or after the enter_date and before 2026-01-01
# Note: to allow follow up of 1 year until end 2026. This is probably not a needed step.
future_events = clinical_events.where(clinical_events.date.is_on_or_between(enter_date, "2026-01-01"))

# Define the FIT test events for each patient where there is a numeric value recorded (i.e. the test was completed) up to end 2024
fit_events = clinical_events.where(
        clinical_events.snomedct_code.is_in(single_fit_code)
).where(
        # Note: filter out NULL numeric values before sorting
        clinical_events.numeric_value.is_not_null()
).where(
        clinical_events.date.is_on_or_between(enter_date, "2025-01-01")
)
# Do we accept the max FIT and cap it?
# Need to check what's the max

# Define index FIT
# Select the earliest FIT date;
# if multiple records occur on that date, select the highest value
index_fit = (
    fit_events
    .sort_by(clinical_events_ranges.date, -clinical_events_ranges.numeric_value)
    .first_for_patient()
)

index_fit_date = index_fit.date
index_fit_value = index_fit.numeric_value
fit_comparator = index_fit.comparator

# Define eligibility criteria

## Is alive at index FIT
is_alive = patients.is_alive_on(index_fit_date)

## Require registration to cover the index FIT date and the following year
lookback_start_date = index_fit_date - years(1)
followup_end_date = index_fit_date + years(1)

has_full_registration = (
    practice_registrations
    .spanning(lookback_start_date, followup_end_date)
    .exists_for_patient()
)

# Death occurs during follow-up
died_during_followup = (
    (ons_deaths.date.is_after(index_fit_date) & ons_deaths.date.is_on_or_before(followup_end_date))
    | (patients.date_of_death.is_after(index_fit_date) & patients.date_of_death.is_on_or_before(index_fit_date))
)

# Combined
has_registration = (has_full_registration | died_during_followup)

# issue: one registration may not cover the duration of start-end period - just like CPRD, accept?
# issue: do we need FU from 1 year before or just index FIT
# Taking maximum first FIT as there are multiple FIT values on the same date for some patients, 
# and so have taken the maximum value as the first FIT value for those patients (could take minimum or mean but imagine there may be zeros)

## Age 18 or older at index FIT
## Define the age of patients at the time of their first FIT test
age_at_index_fit = patients.age_on(index_fit_date)
aged_18_or_older = (
    age_at_index_fit.is_not_null()
    & age_at_index_fit >= 18
    & age_at_index_fit <110)

## Has valid index FIT
has_index_fit = index_fit_date.is_not_null()

## Has HES/ONS linkage (need validation)
has_hes_record = apcs.exists_for_patient()
has_ons_death_record = ons_deaths.exists_for_patient() #only Feb 2019 onwards

## Combine eligibility criteria
eligible_cohort = (
    has_index_fit
    & is_alive
    & aged_18_or_older
    & has_registration
    & (has_hes_record | has_ons_death_record)
)

# variables
index_address = addresses.for_patient_on(index_fit_date)
index_gp = practice_registrations.for_patient_on(index_fit_date)

# Define the valid FIT test events for each patient where there is a numeric value recorded (i.e. the test was completed) and the result is positive (i.e. the numeric value is greater than or equal to 10)
fit_values = fit_events.numeric_value

valid_fit_events_positive = fit_events.where(earliest_max_fit_event.numeric_value >= 10)

# Define the age of patients at the time of their first FIT test

aged_18_or_older = (first_fit_date - patients.date_of_birth).years >= 18

# Define the calendar year of first fit test

year_of_first_fit = first_fit_date.year

# Create the dataset and define the population

dataset = create_dataset()
dataset.define_population(eligible_cohort)
dataset.configure_dummy_data(population_size=10000)

dataset.sex = patients.sex
dataset.age_at_index_fit = age_at_index_fit
dataset.index_fit_date = index_fit_date
dataset.index_fit_year = index_fit_date.year
dataset.index_fit_value = index_fit_value
dataset.fit_comparator = fit_comparator
dataset.date_of_death = patients.date_of_death
dataset.ons_deaths = ons_deaths.date
dataset.imd_quintile = index_address.imd_quintile
dataset.imd_rounded = index_address.imd_rounded
dataset.msoa_code = index_address.msoa_code
dataset.rural_urban_classification = (
    index_address.rural_urban_classification
)
dataset.region = index_gp.practice_nuts1_region_name
dataset.ethnicity = ethnicity_from_sus.code
dataset.define_population(has_registration & aged_18_or_older & fit_events.exists_for_patient())

# add columns to the dataset

dataset.sex = patients.sex
dataset.age_at_first_fit = (first_fit_date - patients.date_of_birth).years
dataset.year_of_first_fit = year_of_first_fit
dataset.first_fit_date = first_fit_date
dataset.value_of_first_max_fit_observed = earliest_max_fit_event.numeric_value
dataset.first_fit_code = earliest_max_fit_event.snomedct_code
dataset.valid_fit_events_positive = valid_fit_events_positive.exists_for_patient()


## some code that doesn't work yet (due to multiple FIT values being available on the same date and me not being able to figure out
#  how to get just one!)
# dataset.first_fit_value = fit_events.where(clinical_events.date == first_fit_date).numeric_value 

dataset.configure_dummy_data(population_size=10000)
