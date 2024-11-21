import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';
import { HttpParams } from '@angular/common/http';


@Injectable({
  providedIn: 'root'
})

export class PatientService{

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
    
    updatePatientProfile(patient: Partial<Patient>, medicalRecordNumber: string): Observable<Patient> {
        const url = `${this.apiUrl}${medicalRecordNumber}`;
        return this.http.patch<Patient>(url, patient);
    }
    
    
    markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any> {
        const url = `${this.apiUrl}${medicalRecordNumber}`;
        return this.http.delete<any>(url);
    }

    listPatients(patient: Partial<Patient> | null): Observable<Patient[]> {

        const url = `${this.apiUrl}find`; // Append the "find" path to the base API URL
    
        // If `patient` is null or no properties are provided, send the request without query params
        const params = patient ? this.buildQueryParams(patient) : new HttpParams();
    
        return this.http.get<Patient[]>(url, { params });
    }
    
    /**
     * Helper method to construct query parameters
     */
    private buildQueryParams(patient: Partial<Patient>): HttpParams {
        let params = new HttpParams();
        // Handle top-level properties
        for (const key in patient) {
            const value = patient[key as keyof Patient];
            if (value !== undefined && value !== null && typeof value !== 'object') {
                params = params.set(key, value.toString());
            }
        }

        // Handle nested ContactInformation
        if (patient.contactInformation) {
            if (patient.contactInformation.phoneNumber) {
                params = params.set('ContactInformation.PhoneNumber', patient.contactInformation.phoneNumber);
            }
            if (patient.contactInformation.emailAddress) {
                params = params.set('ContactInformation.EmailAddress', patient.contactInformation.emailAddress);
            }
        }

        // Handle nested DateOfBirth
        if (patient.dateOfBirth) {
            if (patient.dateOfBirth.yearOfBirth) {
                params = params.set('DateOfBirth.YearOfBirth', patient.dateOfBirth.yearOfBirth.toString());
            }
            if (patient.dateOfBirth.monthOfBirth) {
                params = params.set('DateOfBirth.MonthOfBirth', patient.dateOfBirth.monthOfBirth.toString());
            }
            if (patient.dateOfBirth.dayOfBirth) {
                params = params.set('DateOfBirth.DayOfBirth', patient.dateOfBirth.dayOfBirth.toString());
            }
        }

        return params;
    }

    getMedicalRecordNumber(patientEmail : string): Observable<string> {

        const url = `${this.apiUrl}getMedicalRecordNumber`;

        const params = new HttpParams().set('email', patientEmail);
        return this.http.get<string>(url, { params });
    }

}