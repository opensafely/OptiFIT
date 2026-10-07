from ehrql import create_dataset, codelist_from_csv
from ehrql.tables.tpp import (
    clinical_events,
    medications,
    patients,
    practice_registrations,
)

# Define codelists
# note should probably restrict to just this FIT code 1049361000000100 as seems to be the used SNOMED code for FIT test in TPP,
# but leaving in the other codes for now as they are in the codelist
fit_codes = codelist_from_csv(
    "codelists/user-joewest-fit-and-fob-snomed.csv",
    column="code",
)

# Define the study population
# Note: patients must have a practice registration that spans the enter_date and 2026-01-01,
# and be aged 18 or older at the time of their first FIT test
enter_date = "2019-01-01"

# Note: the end date is set to 2026-01-01 to allow follow up of 1 year until end 2026 from 1st January 2025 (the end of the fit inclusion
#  period)
has_registration = practice_registrations.spanning(enter_date, "2026-01-01").exists_for_patient()

# Define the future clinical events for each patient where the event date is on or after the enter_date and before 2025-01-01
# Note: the end date is set to 2025-01-01 to avoid including fit tests that occur after the study period (which ends on 2024-12-31)
# Note: to allow follow up of 1 year until end 2026
future_events = clinical_events.where(clinical_events.date.is_on_or_between(enter_date, "2025-01-01"))

# Define the FIT test events for each patient where there is a numeric value recorded (i.e. the test was completed)
fit_events = clinical_events.where(
        clinical_events.snomedct_code.is_in(fit_codes)
).where(
        # Note: filter out NULL numeric values before sorting
        clinical_events.numeric_value.is_not_null()
).where(
        clinical_events.date.is_on_or_after(enter_date)
)

# Define the first FIT test date for each patient
first_fit_date = (
    fit_events
    .sort_by(clinical_events.date)
    .first_for_patient()
    .date
)

# Define the valid FIT test events for each patient where there is a numeric value recorded (i.e. the test was completed) and the result is positive (i.e. the numeric value is greater than or equal to 10)
fit_values = fit_events.numeric_value
valid_fit_events_positive = fit_events.where(clinical_events.numeric_value >= 10)

earliest_max_fit_event = fit_events.sort_by(
        # Note the leading minus sign to sort numeric_value in reverse order
        -clinical_events.numeric_value, clinical_events.date
).first_for_patient()

# Define the age of patients at the time of their first FIT test
aged_18_or_older = (first_fit_date - patients.date_of_birth).years >= 18

# Create the dataset and define the population
dataset = create_dataset()
dataset.define_population(has_registration & aged_18_or_older)

dataset.sex = patients.sex
dataset.age_at_first_fit = (first_fit_date - patients.date_of_birth).years
dataset.first_fit_date = first_fit_date
dataset.value_of_first_max_fit_observed = earliest_max_fit_event.numeric_value

## some code that doesn't work yet (due to multiple FIT values being available on the same date and me not being able to figure out
#  how to get just one!)
# dataset.first_fit_value = fit_events.where(clinical_events.date == first_fit_date).numeric_value 
# dataset.earliest_max_fit_event = earliest_max_fit_event
# dataset.first_fit_code = future_events.where(clinical_events.date == first_fit_date).snomedct_code
