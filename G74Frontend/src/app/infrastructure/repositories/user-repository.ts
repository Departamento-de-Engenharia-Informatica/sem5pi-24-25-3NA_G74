import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';
import {User} from '../../domain/models/user.model';
import {IUserRepository} from '../../domain/interfaces/iuser-repository';

@Injectable({
  providedIn: 'root'
})
export class UserRepository implements IUserRepository {
  private apiUrl = `${environment.apiUrl}/user/`;

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

}
