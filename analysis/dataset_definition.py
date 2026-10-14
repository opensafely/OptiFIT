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

# Define the study population
# Note: patients must have a practice registration that spans the enter_date and 2026-01-01,
# and be aged 18 or older at the time of their first FIT test
study_start_date = "2019-01-01"
study_end_date = "2024-12-31"

fit_event = (
    clinical_events_ranges
    .where(clinical_events_ranges.snomedct_code.is_in(fit_codes))
    .where(clinical_events_ranges.numeric_value.is_not_null())
    .where(clinical_events_ranges.numeric_value >= 0)
    .where(clinical_events_ranges.date.is_on_or_after(study_start_date))
    .where(clinical_events_ranges.date.is_on_or_before(study_end_date))
)

# Define index FIT
# Select the earliest FIT date;
# if multiple records occur on that date, select the highest value
index_fit = (
    fit_event
    .sort_by(clinical_events_ranges.date, -clinical_events_ranges.numeric_value)
    .first_for_patient()
)

index_fit_date = index_fit.date
index_fit_value = index_fit.numeric_value
fit_comparator = index_fit.comparator

# Define eligibility criteria
## is alive at index FIT
is_alive = patients.is_alive_on(index_fit_date)

## is registered at FIT
has_registration = (
    practice_registrations.for_patient_on(index_fit_date)
    .exists_for_patient()
)

## Age 18 or older at index FIT
## Define the age of patients at the time of their first FIT test
age_at_index_fit = patients.age_on(index_fit_date)
age_eligible = (
    age_at_index_fit.is_not_null()
    & (age_at_index_fit >= 18)
    & (age_at_index_fit < 110)
)

## Has valid index FIT
has_index_fit = index_fit_date.is_not_null()

## Combine eligibility criteria
eligible_cohort = (
    has_index_fit
    & is_alive
    & age_eligible
    & has_registration
)

# variables
index_address = addresses.for_patient_on(index_fit_date)
index_gp = practice_registrations.for_patient_on(index_fit_date)


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


# opensafely exec ehrql:v1 generate-dataset analysis/dataset_definition.py --output output/dataset.csv.gz