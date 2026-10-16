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

# opensafely exec ehrql:v1 generate-dataset analysis/dataset_definition_with_loop.py

fit_codes = codelist_from_csv(
    "codelists/user-joewest-fit-and-fob-snomed.csv",
    column="code",
)
study_start_date = "2019-01-01"
study_end_date = "2024-12-31"

def with_repeated_fits(n):
    variables = {}

    for i in range(1, n + 1):
        events = (
            clinical_events_ranges
            .where(
                clinical_events_ranges.snomedct_code.is_in(fit_codes)
            )
            .where(clinical_events_ranges.date.is_not_null())
            .where(clinical_events_ranges.date.is_on_or_after(study_start_date))
            .where(clinical_events_ranges.date.is_on_or_before(study_end_date))
        )

        if i > 1:
            events = events.where(
                clinical_events_ranges.date.is_after(
                    variables[f"fit_{i-1}_date"]
                )
            )

        event = (
            events
            .sort_by(
                clinical_events_ranges.date,
                -clinical_events_ranges.numeric_value
            )
            .first_for_patient()
        )

        variables[f"fit_{i}_date"] = event.date
        variables[f"fit_{i}_value"] = event.numeric_value
        variables[f"fit_{i}_comparator"] = event.comparator

    return variables

# Define index_date as the first FIT date
repeated_fits = with_repeated_fits(10)
index_fit_date = repeated_fits["fit_1_date"]

# Define eligibility criteria
## is alive at index FIT
is_alive = patients.is_alive_on(index_fit_date)

## is registered at FIT
has_registration = (
    practice_registrations.for_patient_on(index_fit_date)
    .exists_for_patient()
)

# variables
index_address = addresses.for_patient_on(index_fit_date)
index_gp = practice_registrations.for_patient_on(index_fit_date)

## Combine eligibility criteria
eligible_cohort = (
    is_alive
    & has_registration
)

dataset = create_dataset()
dataset.define_population(eligible_cohort)
dataset.configure_dummy_data(population_size=10)

dataset.sex = patients.sex
dataset.dob = patients.date_of_birth
dataset.index_fit_date = index_fit_date
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
repeated_fits = with_repeated_fits(10)
for name, variable in repeated_fits.items():
    setattr(dataset, name, variable)