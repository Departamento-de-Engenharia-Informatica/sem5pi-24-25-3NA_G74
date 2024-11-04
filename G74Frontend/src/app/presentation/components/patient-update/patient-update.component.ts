import { Component } from '@angular/core';
import { PatientService } from '../../../application/services/patient.service';
import { Patient } from '../../../domain/models/patient.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

@Component({
  selector: 'app-patient-update',
  templateUrl: './patient-update.component.html',
  styleUrls: ['./patient-update.component.css']
})
export class PatientUpdateComponent {
  medicalRecordNumber: string = '';
  patient: Partial<Patient> = {};
  dateOfBirthInput: string = '';
  phoneNumber: string = '';  // Separate properties for two-way binding
  emailAddress: string = ''; // Separate properties for two-way binding
  message: string = '';

  constructor(private patientService: PatientService) { }

  onSubmit(): void {
    if (!this.medicalRecordNumber) {
      this.message = 'Please enter a valid medical record number.';
      return;
    }

    if (this.dateOfBirthInput) {
      const [year, month, day] = this.dateOfBirthInput.split('-').map(Number);
      this.patient.dateOfBirth = {
        yearOfBirth: year,
        monthOfBirth: month,
        dayOfBirth: day
      };
    }

    const contactInfo: any = {};
    if (this.phoneNumber) {
      contactInfo.phoneNumber = this.phoneNumber;
    }
    if (this.emailAddress) {
      contactInfo.emailAddress = this.emailAddress;
    }

    if (Object.keys(contactInfo).length > 0) {
      this.patient.contactInformation = contactInfo;
    } else {
      delete this.patient.contactInformation;
    }

    this.patientService.updatePatientProfile(this.patient,this.medicalRecordNumber,).pipe(
      catchError(error => {
        console.error('Error updating patient profile:', error);
        this.message = `Failed to update patient profile. ${error?.error?.message || 'Please try again.'}`;
        return of(null);
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Patient profile updated successfully!';
        this.resetForm();
      }
    });
  }

  resetForm(): void {
    this.medicalRecordNumber = '';
    this.patient = {};
    this.dateOfBirthInput = '';
    this.phoneNumber = '';
    this.emailAddress = '';
  }
}
