import { Injectable } from '@angular/core';
import { of, Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';

@Injectable({
  providedIn: 'root'
})
export class MockPatientViewModel {
  // Mock data
  private mockPatients: Patient[] = [
    {
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
      contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
      emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
    }
  ];

  createPatientProfile(patient: Patient): Observable<Patient> {
    this.mockPatients.push(patient);
    return of(patient);
  }

  updatePatientProfile(patient: Partial<Patient>, medicalRecordNumber: string): Observable<Patient> {
    const index = this.mockPatients.findIndex((p) => p.contactInformation.emailAddress === patient.contactInformation?.emailAddress);
    if (index > -1) {
      this.mockPatients[index] = { ...this.mockPatients[index], ...patient };
      return of(this.mockPatients[index]);
    }
    throw new Error('Patient not found');
  }

  markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any> {
    this.mockPatients = this.mockPatients.filter((p) => p.contactInformation.emailAddress !== medicalRecordNumber);
    return of({ message: 'Patient deleted successfully' });
  }

  listPatients(filter: Partial<Patient> | null): Observable<Patient[]> {
    // Simple filtering logic
    return of(this.mockPatients);
  }

  getMedicalRecordNumber(email: string): Observable<string> {
    const patient = this.mockPatients.find((p) => p.contactInformation.emailAddress === email);
    return of(patient ? 'mock-medical-record-number' : '');
  }

  public getMockPatients() {

    return this.mockPatients;

  }

}
