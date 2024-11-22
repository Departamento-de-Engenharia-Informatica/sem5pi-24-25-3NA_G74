import { Component, EventEmitter, Input, Output } from '@angular/core';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { catchError } from 'rxjs';
import { of } from 'rxjs';

@Component({
  selector: 'app-patient-delete',
  templateUrl: './patient-delete.component.html',
  styleUrls: ['./patient-delete.component.css'],
})
export class PatientDeleteComponent {
  @Input() patient!: { contactInformation: { emailAddress: string } }; // Receive patient info
  @Output() close = new EventEmitter<boolean>();

  message: string = '';
  isLoading: boolean = false;

  constructor(private patientViewModel: PatientViewModel) {}

  deletePatient(): void {
    this.isLoading = true;
    this.message = '';

    // Fetch the medical record number using the patient's email
    const email = this.patient.contactInformation.emailAddress;

    this.patientViewModel
      .getMedicalRecordNumber(email)
      .pipe(
        catchError((error) => {
          console.error(`Error fetching medical record number for ${email}:`, error);
          this.message = `Failed to fetch medical record number for ${email}.`;
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe((medicalRecordNumber) => {
        if (medicalRecordNumber) {
          this.patientViewModel
            .markPatientProfileAsDeleted(medicalRecordNumber)
            .pipe(
              catchError((error) => {
                console.error('Error deleting patient profile: ', error);
                this.message = `Failed to delete patient profile. ${error?.error?.message || 'Please try again.'}`;
                this.isLoading = false;
                return of(null);
              })
            )
            .subscribe((response) => {
              this.isLoading = false;
              if (response) {
                this.message = `Patient profile deleted successfully.`;
                setTimeout(() => this.close.emit(true), 2000); // Close popup and refresh list
              }
            });
        }
      });
  }

  cancelDelete(): void {
    this.close.emit(false); // Close popup without refreshing
  }
}
