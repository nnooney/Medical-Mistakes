# Random Effects
  # Patients
    # Demographics 
    # Race 
    # Age
    # Ethnicity
    # Gender
  # Hospital ID (oshpid)
    # Mistake Rate
    # Number of Beds
    # Profit Margin
    # Type of Facility (Acute Care, Psychiatric Care, Chemical Rehab, Physical Rehab, Skilled Nursing)
    # Type of Hospital (Teaching, Non-Profit, Profit)
  # County/Zip

load("~/shared/Data/Object Model Data")
colnames(model.data)

################################################
# BASIC MODEL (No Effects)
################################################
basic.model <- glm(data=model.data, obj.mistake ~ charleson + exp.specialists + 
                     sex1 + race1 + eth1 + age1 + 
                     typ_care1 + adm_typ1 + los + pay_cat1 + pay_type1, family="binomial")