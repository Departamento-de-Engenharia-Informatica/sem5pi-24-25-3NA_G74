import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';
import {User} from '../../domain/models/user.model';
import {IUserRepository} from '../../domain/interfaces/iuser-repository';
import {Patient} from '../../domain/models/patient.model';
import {response} from 'express';

@Injectable({
  providedIn: 'root'
})
export class UserRepository implements IUserRepository {
  private apiUrl = `${environment.apiUrl}/User/`;

  constructor(private http: HttpClient) {}

  registerUser(user: User): Observable<User> {
    console.log('Sending user data to backend:', user);
    return this.http.post<User>(this.apiUrl, user).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }

  updateUser(user: Partial<User>): Observable<User> {
    return this.http.patch<User>(this.apiUrl, user).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }


  markUserAsDeleted(): Observable<any> {
    return this.http.delete<any>(this.apiUrl).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }

}
