import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { Staff } from '../../domain/models/staff.model';
import { environment } from '../../../environments/environment';
import { HttpClient } from '@angular/common/http';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
    providedIn: 'root'
})
export class StaffService {
    http = inject(HttpClient);

    private apiUrl = `${environment.apiUrl}/staff/`;

    createStaffProfile(staff: Staff): Observable<Staff> {
        console.log('Sending staff data to backend:', staff); // Log data sent to backend
        return this.http.post<Staff>(this.apiUrl, staff).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    updateStaffProfile(licenceNumber: string, staff: Staff): Observable<Staff> {
        console.log('Sending staff data to backend:', staff); // Log data sent to backend
        return this.http.put<Staff>(`${this.apiUrl}licence/${licenceNumber}`, staff).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    getAll(): Observable<Staff[]> {
        console.log('Fetching all staff'); // Log operation
        return this.http.get<Staff[]>(this.apiUrl).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    getByLicenceNumber(licenceNumber: string): Observable<Staff> {
        console.log('Fetching staff member'); // Log operation
        return this.http.get<Staff>(`${this.apiUrl}licence/${licenceNumber}`).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    deactivateStaff(licenceNumber: string): Observable<any> {
        console.log('Deactivating staff member'); // Log operation
        return this.http.patch<Staff>(`${this.apiUrl}licence/${licenceNumber}/deactivate`, null).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }
}
