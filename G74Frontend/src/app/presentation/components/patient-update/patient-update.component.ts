import { Component, OnInit } from '@angular/core';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { Patient } from '../../../domain/models/patient.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

@Component({
  selector: 'app-patient-update',
  templateUrl: './patient-update.component.html',
  styleUrls: ['./patient-update.component.css'],
})
export class PatientUpdateComponent implements OnInit {
  patients: Patient[] = [];
  dateInputs: { [key: string]: string } = {}; // Track date inputs by patient
  message: string = '';

  constructor(private patientViewModel: PatientViewModel) {}

  ngOnInit(): void {
    this.fetchPatients();
  }

  fetchPatients(): void {
    this.patientViewModel
      .listPatients(null)
      .pipe(
        catchError((error) => {
          console.error('Error fetching patients:', error);
          this.message = 'Failed to fetch patients.';
          return of([]);
        })
      )
      .subscribe((patients) => {
        this.patients = patients;
  
        // Pre-fill date inputs for each patient and fetch medicalRecordNumber
        this.patients.forEach((patient) => {
          this.dateInputs[patient.contactInformation.emailAddress] = this.formatDate(patient.dateOfBirth);
  
          // Fetch medical record number for each patient
          this.patientViewModel
            .getMedicalRecordNumber(patient.contactInformation.emailAddress)
            .pipe(
              catchError((error) => {
                console.error(`Error fetching medical record number for ${patient.contactInformation.emailAddress}:`, error);
                return of(null);
              })
            )
            .subscribe((medicalRecordNumber) => {
              patient.medicalRecordNumber = medicalRecordNumber || 'N/A'; // Assign fetched value or 'N/A'
            });
        });
      });
  }

  formatDate(dateOfBirth: { yearOfBirth: number; monthOfBirth: number; dayOfBirth: number }): string {
    const { yearOfBirth, monthOfBirth, dayOfBirth } = dateOfBirth;
    return `${yearOfBirth}-${monthOfBirth.toString().padStart(2, '0')}-${dayOfBirth.toString().padStart(2, '0')}`;
  }

  updatePatient(patient: Patient): void {
    const email = patient.contactInformation.emailAddress;

    // Get the medical record number before updating
    this.patientViewModel
      .getMedicalRecordNumber(email)
      .pipe(
        catchError((error) => {
          console.error(`Error fetching medical record number for ${email}:`, error);
          this.message = `Failed to fetch medical record number for ${email}.`;
          return of(null);
        })
      )
      .subscribe((medicalRecordNumber) => {
        if (medicalRecordNumber) {
          const dateInput = this.dateInputs[patient.contactInformation.emailAddress];
          if (dateInput) {
            const [year, month, day] = dateInput.split('-').map(Number);
            patient.dateOfBirth = { yearOfBirth: year, monthOfBirth: month, dayOfBirth: day };
          }

          // Proceed to update the patient
          this.patientViewModel
            .updatePatientProfile(patient, medicalRecordNumber)
            .pipe(
              catchError((error) => {
                console.error(`Error updating patient with email ${email}:`, error);
                this.message = `Failed to update patient ${email}.`;
                return of(null);
              })
            )
            .subscribe((response) => {
              if (response) {
                this.message = `Patient ${email} updated successfully!`;
              }
            });
        }
      });
  }
}
