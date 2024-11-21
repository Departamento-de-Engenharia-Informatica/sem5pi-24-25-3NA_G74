import { Component, OnInit } from '@angular/core';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { Patient } from '../../../domain/models/patient.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

@Component({
  selector: 'app-patient-list',
  templateUrl: './patient-list.component.html',
  styleUrls: ['./patient-list.component.css'],
})
export class PatientListComponent implements OnInit {
  patients: Patient[] = [];
  filters: Partial<Patient> = {
    contactInformation: {
      phoneNumber: '',
      emailAddress: '',
    },
  };
  phoneNumberFilter: string = '';
  emailAddressFilter: string = '';
  dateOfBirthInput: string = '';
  message: string = '';
  isLoading: boolean = false;

  constructor(private patientViewModel: PatientViewModel) {}

  ngOnInit(): void {
    this.fetchPatients(); // Fetch all patients initially
  }

  fetchPatients(): void {
    this.isLoading = true;

    const filters: Partial<Patient> = { ...this.filters };

    // Assign phoneNumber and emailAddress from intermediate properties
    filters.contactInformation = {
      phoneNumber: this.phoneNumberFilter,
      emailAddress: this.emailAddressFilter,
    };

    if (this.dateOfBirthInput) {
      const [year, month, day] = this.dateOfBirthInput.split('-').map(Number);
      filters.dateOfBirth = { yearOfBirth: year, monthOfBirth: month, dayOfBirth: day };
    }

    this.patientViewModel
      .listPatients(Object.keys(filters).length ? filters : null)
      .pipe(
        catchError((error) => {
          console.error('Error fetching patients:', error);
          this.message = 'Failed to fetch patients. Please try again.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((patients) => {
        this.patients = patients;

        // Fetch medical record numbers for each patient
        this.patients.forEach((patient) => {
          if (patient.contactInformation?.emailAddress) {
            this.fetchMedicalRecordNumber(patient);
          }
        });

        this.message = patients.length ? '' : 'No patients found.';
        this.isLoading = false;
      });
  }

  fetchMedicalRecordNumber(patient: Patient): void {
    this.patientViewModel
      .getMedicalRecordNumber(patient.contactInformation.emailAddress!)
      .pipe(
        catchError((error) => {
          console.error(
            `Error fetching medical record number for email ${patient.contactInformation?.emailAddress}:`,
            error
          );
          patient.medicalRecordNumber = 'Error'; // Set an error message in case of failure
          return of(null);
        })
      )
      .subscribe((medicalRecordNumber) => {
        if (medicalRecordNumber) {
          patient.medicalRecordNumber = medicalRecordNumber;
        }
      });
  }

  clearFilters(): void {
    this.filters = {
      contactInformation: {
        phoneNumber: '',
        emailAddress: '',
      },
    };
    this.phoneNumberFilter = '';
    this.emailAddressFilter = '';
    this.dateOfBirthInput = '';
    this.fetchPatients();
  }
}
