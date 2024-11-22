import { Component } from '@angular/core';
import { Patient } from '../../../domain/models/patient.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';

@Component({
  selector: 'app-patient-create',
  templateUrl: './patient-create.component.html',
  styleUrls: ['./patient-create.component.css']
})
export class PatientCreateComponent {
  patient: Patient = {
    name: '',
    gender: '',
    dateOfBirth: {
      yearOfBirth: 0,
      monthOfBirth: 0,
      dayOfBirth: 0
    },
    contactInformation: {
      phoneNumber: '',
      emailAddress: ''
    },
    emergencyContact: {
      name: '',
      phoneNumber: ''
    }
  };

  dateOfBirthInput: string = '';  // Holds the date input in YYYY-MM-DD format from the form
  message: string = '';

  constructor(private patientViewModel: PatientViewModel) { }

  onSubmit(): void {
    // Parse the date input (YYYY-MM-DD) to extract year, month, and day
    const [year, month, day] = this.dateOfBirthInput.split('-').map(Number);

    // Update `patient.dateOfBirth` with the parsed date
    this.patient.dateOfBirth = {
      yearOfBirth: year,
      monthOfBirth: month,
      dayOfBirth: day
    };

    // Call the service to create the patient profile
    this.patientViewModel.createPatientProfile(this.patient).pipe(
      catchError(error => {
        console.error('Error creating patient profile:', error);  // Log the error details
        this.message = `Failed to create patient profile. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);  // Return a null observable to complete the stream
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Patient profile created successfully!';
        this.resetForm();
      }
    });
  }

  resetForm(): void {
    this.patient = {
      name: '',
      gender: '',
      dateOfBirth: {
        yearOfBirth: 0,
        monthOfBirth: 0,
        dayOfBirth: 0
      },
      contactInformation: {
        phoneNumber: '',
        emailAddress: ''
      },
      emergencyContact: {
        name: '',
        phoneNumber: ''
      }
    };
    this.dateOfBirthInput = '';  // Reset the date input field
  }
}
