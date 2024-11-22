import { Component } from '@angular/core';
import { catchError } from 'rxjs';
import { of } from 'rxjs';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';

@Component({
  selector: 'app-patient-delete',
  templateUrl: './patient-delete.component.html',
  styleUrl: './patient-delete.component.css'
})
export class PatientDeleteComponent {

  medicalRecordNumber: string = '';
  message: string = '';


  constructor(private patientViewModel: PatientViewModel) { }

  onSubmit(): void {
    if (!this.medicalRecordNumber) {
      this.message = 'Please enter a valid medical record number';
      return;
    }

    this.patientViewModel.markPatientProfileAsDeleted(this.medicalRecordNumber).pipe(
      catchError(error => {
        console.error('Error deleting patient profile: ', error);
        this.message = `Failed to delete patient profile. ${error?.error?.message || 'Please try again.'}`
        return of(null)
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Patient profile deleted successfully';
        this.resetForm();
      }
    });
  }
  resetForm(): void {
    this.medicalRecordNumber = '';
  }

}