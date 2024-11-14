import { Injectable, inject } from '@angular/core';
import { IStaffRepository } from '../../domain/interfaces/istaff-repository';
import { Observable } from 'rxjs';
import { Staff } from '../../domain/models/staff.model';
import { environment } from '../../../environments/environment';
import { HttpClient } from '@angular/common/http';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
    providedIn: 'root'
})
export class StaffRepository implements IStaffRepository {
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
}
