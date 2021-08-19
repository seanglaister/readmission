# Obtain a new list of readmitted and control patients
library(tidyverse)
library(dplyr)

#### Step 1: Load Patients, isolate Readmitted Patients. Choose the first occurence of the readmissions = "second_ICU_Stay" ####
patients <- read.csv("Readmission/Data/patient.csv") #loading this in takes some time. 
patients <- patients %>% select(patientunitstayid, patienthealthsystemstayid, unitvisitnumber,unittype, unitstaytype, unitadmitsource, hospitaladmitoffset, unitdischargeoffset)

#hospitaladmitoffset that's positive makes no sense. Convert it to negative first and hope it's right. If it's wrong we can toss it out later. 
patients$hospitaladmitoffset[which(patients$hospitaladmitoffset > 0)] <- patients$hospitaladmitoffset[which(patients$hospitaladmitoffset > 0)] * -1

patients <- patients[which(patients$unitstaytype %in% c("admit", "readmit")), ]

readmitted_patients <- patients[which(patients$unitstaytype == 'readmit'), ]

# ORDER readmitted_patients first by patienthealthsystemstayid, then by -(hospitaladmitoffset)
# readmitted_patients <- readmitted_patients[order( readmitted_patients$patienthealthsystemstayid, -(readmitted_patients$hospitaladmitoffset)), ]
second_ICU_Stay <- readmitted_patients %>% group_by(patienthealthsystemstayid) %>% slice(which.min(unitvisitnumber)) #HAN: min to find first readmission among the many possible readmissions.

# # EXTRACT the  first readmitted ICU stay of each patienthealthsystemstayid, = "second_ICU_Stay"
# second_ICU_Stay <- readmitted_patients[!duplicated(readmitted_patients$patienthealthsystemstayid), ]

#### Step 2: Find the corresponding first_ICU_Stay for each second_ICU_Stay, = "first_ICU_Stay" ####
# Filter patients who have the same healthsystemid's as the first_ICU_Stay patinets
readmitted_patients_all_Stays <- patients[which(patients$patienthealthsystemstayid %in% second_ICU_Stay$patienthealthsystemstayid), ]


#so i saw that some in the first ICU Stay category also were labeled as readmitted. So just made a loop to make it easier to understand what's going on. First, we want select a visit number that is less than the second ICU stay. And we want to ensure that the first ICU stay we consider is an admit. And within the admit, we are interested in the one closest to the second ICU stay if more than 1 exists. 
first_ICU_Stay <- rbind()
for (i in 1:nrow(second_ICU_Stay)) {
  print(i)
  hid <- second_ICU_Stay$patienthealthsystemstayid[i]
  visitnum <- second_ICU_Stay$unitvisitnumber[i]
  
  temp <- readmitted_patients_all_Stays[which(readmitted_patients_all_Stays$patienthealthsystemstayid == hid &
                                                readmitted_patients_all_Stays$unitvisitnumber < visitnum),]
  
  temp <- temp[which(temp$unitstaytype == "admit"), ]
  
  temp <- temp[which.max(temp$unitvisitnumber), ]
  
  first_ICU_Stay <- rbind(first_ICU_Stay, temp)
}


#### Step 3: Find the readmission_time_interval between the first and second stays, add this as a column, "first_ICU_Stays$readmissiontimeinterval" ####
# -(second_ICU_Stay$hospitaladmitoffset) - -(first_ICU_Stay$hospitaladmitoffset)

#allocate all columns for first ICU stay and second ICU stay
patients <- read.csv("Readmission/Data/patient.csv")
first_ICU_Stay <- patients[which(patients$patientunitstayid %in% first_ICU_Stay$patientunitstayid), ]
second_ICU_Stay <- patients[which(patients$patientunitstayid %in% second_ICU_Stay$patientunitstayid), ]

# EDIT:HAN 4/15/2020 -> to get the readmission time interval, we would need the time from 1st icu discharge to the 2nd icu admission. 
# so we would need to first get the time from 1st ICU admission to 2nd ICU admission which you did, then add the 1st ICU duration using 
# unit discharge offset. 
# first_ICU_Stay$readmissiontimeinterval <- -(second_ICU_Stay$hospitaladmitoffset) - -(first_ICU_Stay$hospitaladmitoffset) # original

second_ICU_Stay <- second_ICU_Stay[which(second_ICU_Stay$patienthealthsystemstayid %in% first_ICU_Stay$patienthealthsystemstayid), ]
second_ICU_Stay_offset <- second_ICU_Stay %>% dplyr::select(patienthealthsystemstayid, hospitaladmitoffset)
colnames(second_ICU_Stay_offset) <- c("patienthealthsystemstayid", "hospitaladmitoffset_2ndICU")

first_ICU_Stay <- merge(first_ICU_Stay, second_ICU_Stay_offset, by = "patienthealthsystemstayid")

first_ICU_Stay$readmissiontimeinterval <- abs(first_ICU_Stay$hospitaladmitoffset_2ndICU) - abs(first_ICU_Stay$hospitaladmitoffset) - abs(first_ICU_Stay$unitdischargeoffset) #edited

#HAN: so turns out some of the offsets are incorrect in the database itself. Need to remove cases that are < 0 and also = 0 since we'll remove
# those later anyways. 
first_ICU_Stay <- first_ICU_Stay[-which(first_ICU_Stay$readmissiontimeinterval <= 0), ]
second_ICU_Stay <- second_ICU_Stay[which(second_ICU_Stay$patienthealthsystemstayid %in% first_ICU_Stay$patienthealthsystemstayid), ]



#### Step 4: Segement only readmission_time_intervals between 1 and 72 hours. Segment only those who were not discharged to the OR ####
first_ICU_Stay <- first_ICU_Stay[which((first_ICU_Stay$readmissiontimeinterval >= 60) & (first_ICU_Stay$readmissiontimeinterval <= 72*60)), ]

patients <- read.csv("Readmission/Data/patient.csv") #loading this in takes some time. 

first_ICU_Stay <- patients[which(patients$patientunitstayid %in% first_ICU_Stay$patientunitstayid), ]
first_ICU_Stay <- first_ICU_Stay[which((first_ICU_Stay$unitdischargelocation != "Death") & (first_ICU_Stay$unitdischargelocation != "Operating Room")), ]
second_ICU_Stay <- second_ICU_Stay[which(second_ICU_Stay$patienthealthsystemstayid %in% first_ICU_Stay$patienthealthsystemstayid), ]
second_ICU_Stay <- patients[which(patients$patientunitstayid %in% second_ICU_Stay$patientunitstayid), ]




#### Step 5: Obtain Control Patients (nonreadmitted ICU Stays), = "control_patients" ####

# Load all patients' ICUids and HospitalIDs into a matrix, "hospitalid"
patients <- read.csv("Readmission/Data/patient.csv") 
hospitalid <- patients %>% select(patientunitstayid, patienthealthsystemstayid)

# Create "hID_table" keeps track of the frequency of HospitalID's (so the number of ICUid's per HospitalID) 
hID_table <- as.data.frame(table(hospitalid$patienthealthsystemstayid)) 

# Threshold isolates just the HospitalID's with one ICUid, into "hID_table_only_Non_Readmitted"
hID_table_only_Non_Readmitted <- hID_table[hID_table$Freq <= 1 , ]

# Isolating just the hospital ids for those with singular ICUid's into "valid_Non_Readmitted_hIDS"
valid_Non_Readmitted_hIDS <- as.numeric(as.character(hID_table_only_Non_Readmitted$Var1))

# Load all the columns of patients information for the patients who have singular ICUid, into "control_patients" 
control_patients <- patients[which(patients$patienthealthsystemstayid %in% valid_Non_Readmitted_hIDS), ]

# Now, isolate only the admitted patient ICU Stays into "admitted_patients". (no reamdits/stepdowns/transfers)
control_patients <- control_patients[which(control_patients$unitstaytype == 'admit'), ]

# Now, isolate only the living control patients
control_patients <- control_patients[which(control_patients$unitdischargestatus == 'Alive'), ]


#### Step 6: Put new readmitted and control patients into a new pid list ####
pids <- data.frame(first_ICU_Stay$patientunitstayid)
colnames(pids) <- "patientunitstayid"
pids$class <- "readmit"
control_pids <- data.frame(control_patients$patientunitstayid)
colnames(control_pids) <- "patientunitstayid"
control_pids$class <- "control"
pids <- rbind(pids, control_pids)

#### Step 7: Write list to .csv ####
write.csv(pids, "samplepids_11_19_20.csv")

#### Step 8: Add start and end offsets to patient data ####
## pids <- read.csv("samplepids_9_26_20.csv")
patients <- patients[which(patients$patientunitstayid %in% pids$patientunitstaydid), ]
names(pids)[names(pids)=='patientunitstaydid'] <- "patientunitstayid"
discharge <- patients[c("patientunitstayid", "unitdischargeoffset")]
pids <- merge(pids, discharge, by="patientunitstayid")

names(pids)[names(pids)=='unitdischargeoffset.x'] <- "start_offset"
names(pids)[names(pids)=='unitdischargeoffset.y'] <- "end_offset"

index = 1
for (index in 1:nrow(pids)) {
  if(as.numeric(pids$start_offset[index]) < 0) {
    pids$start_offset[index] = 0
  }
  index = index + 1
  print(index)
}

write.csv(pids, "samplepids_10_14_20.csv")





