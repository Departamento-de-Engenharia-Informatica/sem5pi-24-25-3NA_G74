import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { environment } from '../../../environments/environment';
import { LoginInfo } from '../models/login.model';
import { LoginResponse } from '../models/login-response.model'; // Import the response model

@Injectable({
    providedIn: 'root',
})
export class AuthService {
    private apiUrl = `${environment.apiUrl}/auth`;

    public currentUserSubject = new BehaviorSubject<any>(null);

    constructor(private http: HttpClient, private router: Router) {
        const token = localStorage.getItem('token');
        if (token) {
            this.currentUserSubject.next(this.decodeToken(token));
        }
    }

    login(loginInfo: LoginInfo): Observable<LoginResponse> {
        return this.http.post<LoginResponse>(`${this.apiUrl}/login`, loginInfo).pipe(
            map((response) => {
                localStorage.setItem('token', response.token);
                this.currentUserSubject.next(this.decodeToken(response.token));
                return response; // Return the structured LoginResponse
            })
        );
    }

    logout() {
        localStorage.removeItem('token');
        this.currentUserSubject.next(null);
        this.router.navigate(['/login']);
    }

    get currentUser() {
        return this.currentUserSubject.asObservable();
    }

    private decodeToken(token: string) {
        const payload = atob(token.split('.')[1]);
        const decoded = JSON.parse(payload);
        //console.log('Decoded Token:', decoded); // Debug log
        const user = {
            username: decoded['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name'],
            email: decoded['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress'],
            role: decoded['http://schemas.microsoft.com/ws/2008/06/identity/claims/role'],
        };
        return user;
    }

    get isAdmin(): boolean {
        const user = this.currentUserSubject.value;
        return user?.role === 'Admin';
    }
}
