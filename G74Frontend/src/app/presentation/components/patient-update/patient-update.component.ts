import { Component, EventEmitter, Input, OnInit, Output, HostListener } from '@angular/core';
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
  @Input() patient!: Patient; // Selected patient passed from parent
  @Output() close = new EventEmitter<boolean>(); // Emit event to close the popup

  dateInput: string = ''; // Track date input
  message: string = '';
  isLoading: boolean = false;

  constructor(private patientViewModel: PatientViewModel) {}

  ngOnInit(): void {
    // Initialize the date input for the selected patient
    this.dateInput = this.formatDate(this.patient.dateOfBirth);
  }

  formatDate(dateOfBirth: { yearOfBirth: number; monthOfBirth: number; dayOfBirth: number }): string {
    const { yearOfBirth, monthOfBirth, dayOfBirth } = dateOfBirth;
    return `${yearOfBirth}-${monthOfBirth.toString().padStart(2, '0')}-${dayOfBirth.toString().padStart(2, '0')}`;
  }

  updatePatient(): void {
    this.isLoading = true;
    this.message = '';

    const email = this.patient.contactInformation.emailAddress;

    // Fetch the medical record number using the patient's email
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
          const [year, month, day] = this.dateInput.split('-').map(Number);
          this.patient.dateOfBirth = { yearOfBirth: year, monthOfBirth: month, dayOfBirth: day };

          this.patientViewModel
            .updatePatientProfile(this.patient, medicalRecordNumber)
            .pipe(
              catchError((error) => {
                console.error(`Error updating patient with email ${email}:`, error);
                this.message = `Failed to update patient ${email}.`;
                this.isLoading = false;
                return of(null);
              })
            )
            .subscribe((response) => {
              this.isLoading = false;
              if (response) {
                this.message = `Patient ${email} updated successfully!`;
                setTimeout(() => this.close.emit(true), 2000); // Automatically close on success
              }
            });
        }
      });
  }

  cancelUpdate(): void {
    this.close.emit(false); // Close the popup without refreshing
  }

  @HostListener('document:click', ['$event.target'])
  onOutsideClick(target: HTMLElement): void {
    if (target.classList.contains('update-popup')) {
      this.cancelUpdate(); // Close popup on clicking outside
    }
  }
}
