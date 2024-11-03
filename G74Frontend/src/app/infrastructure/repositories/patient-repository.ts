import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { IPatientRepository } from '../../domain/interfaces/ipatient-repository';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class PatientRepository implements IPatientRepository {
  private apiUrl = `${environment.apiUrl}/patient/`;

  constructor(private http: HttpClient) {}

  createPatientProfile(patient: Patient): Observable<Patient> {
    console.log('Sending patient data to backend:', patient); // Log data sent to backend
    return this.http.post<Patient>(this.apiUrl, patient).pipe(
      tap(response => console.log('Received response from backend:', response)), // Log successful response
      catchError(error => {
        console.error('Error response from backend:', error); // Log error response
        throw error; // Re-throw error to be handled by calling service
      })
    );
  }

  updatePatientProfile(patient: Patient, medicalRecordNumber: string): Observable<Patient> {
    const url = `${this.apiUrl}${medicalRecordNumber}`;
    return this.http.patch<Patient>(url, patient);
  }
  
  
  markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any> {
    const url = `${this.apiUrl}${medicalRecordNumber}`;
    return this.http.delete<any>(url);
  }
  

}
