import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { LoginInfo } from '../../domain/models/login.model';
import { LoginResponse } from '../../domain/models/login-response.model';
import { AuthService } from '../../domain/services/auth.service';

@Injectable({
    providedIn: 'root'
})

export class LoginViewModel {

    constructor(private authService: AuthService) { }

    login(login: LoginInfo): Observable<LoginResponse> {
        return this.authService.login(login);
    }
    
    googleLogin(token: string): Observable<LoginResponse> {
        return this.authService.googleLogin(token);
    }

    logout(): void {
        this.authService.logout(); // This removes the token and resets the user session
    }
    

}

