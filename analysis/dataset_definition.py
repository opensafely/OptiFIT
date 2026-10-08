from ehrql import create_dataset, codelist_from_csv
from ehrql.tables.tpp import (
    clinical_events,
    medications,
    patients,
    practice_registrations,
)

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

# Define the first FIT test date for each patient
first_fit_date = (
    fit_events
    .sort_by(clinical_events.date)
    .first_for_patient()
    .date
)

# Taking maximum first FIT as there are multiple FIT values on the same date for some patients, 
# and so have taken the maximum value as the first FIT value for those patients (could take minimum or mean but imagine there may be zeros)

earliest_max_fit_event = fit_events.sort_by(
        # Note the leading minus sign to sort numeric_value in reverse order
        -clinical_events.numeric_value, clinical_events.date
).first_for_patient()

# Define the valid FIT test events for each patient where there is a numeric value recorded (i.e. the test was completed) and the result is positive (i.e. the numeric value is greater than or equal to 10)
fit_values = fit_events.numeric_value

valid_fit_events_positive = fit_events.where(earliest_max_fit_event.numeric_value >= 10)

# Define the age of patients at the time of their first FIT test

aged_18_or_older = (first_fit_date - patients.date_of_birth).years >= 18

# Define the calendar year of first fit test

year_of_first_fit = first_fit_date.year

# Create the dataset and define the population

dataset = create_dataset()
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
