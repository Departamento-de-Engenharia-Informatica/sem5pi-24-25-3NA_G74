import { Component } from '@angular/core';
import { PatientService } from '../../../application/services/patient.service';
import { catchError } from 'rxjs';
import { of } from 'rxjs';

@Component({
  selector: 'app-patient-delete',
  templateUrl: './patient-delete.component.html',
  styleUrl: './patient-delete.component.css'
})
export class PatientDeleteComponent {

  medicalRecordNumber: string = '';
  message: string = '';


  constructor(private patientService: PatientService) { }

  onSubmit(): void {
    if (!this.medicalRecordNumber) {
      this.message = 'Please enter a valid medical record number';
      return;
    }

    this.patientService.markPatientProfileAsDeleted(this.medicalRecordNumber).pipe(
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