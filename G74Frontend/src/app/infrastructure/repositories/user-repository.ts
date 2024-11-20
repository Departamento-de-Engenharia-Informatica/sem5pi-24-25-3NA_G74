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
  private apiUrl = `${environment.apiUrl}/User`;

  constructor(private http: HttpClient) {}

  registerUser(user: User): Observable<User> {
    console.log('Sending user data to backend:', user);
    const myURl = `${this.apiUrl}/register`;
    return this.http.post<User>(myURl, user).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }

  updateUser(email: string, user: Partial<User>): Observable<User> {
    const myURL = `${this.apiUrl}/update/${email}`;
    return this.http.patch<User>(myURL, user).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }


  markUserAsDeleted(email: string): Observable<any> {
    const myURL = `${this.apiUrl}/delete/${email}`;
    return this.http.delete<User>(myURL).pipe(
      tap(response => console.log('Received response from backend:', response)),
      catchError(error => {
        console.error('Error response from backend:', error);
        throw error;
      })
    );
  }

}
