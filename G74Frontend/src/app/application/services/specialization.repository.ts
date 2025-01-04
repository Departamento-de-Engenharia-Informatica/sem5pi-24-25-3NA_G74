import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { Specialization } from '../../domain/models/specialization.model';
import { environment } from '../../../environments/environment';
import { HttpClient } from '@angular/common/http';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
    providedIn: 'root'
})
export class SpecializationService {
    http = inject(HttpClient);

    private apiUrl = `${environment.apiUrl}/specialization/`;

    createSpecialization(specialization: Specialization): Observable<Specialization> {
        console.log('Sending specialization data to backend:', specialization); // Log data sent to backend
        return this.http.post<Specialization>(this.apiUrl, specialization).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    getAll(): Observable<Specialization[]> {
        console.log('Fetching all specializations'); // Log operation
        return this.http.get<Specialization[]>(this.apiUrl).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }

    getByCode(code: string): Observable<Specialization> {
        console.log('Fetching specialization'); // Log operation
        return this.http.get<Specialization>(`${this.apiUrl}code/${code}`).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );
    }
}
