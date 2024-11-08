import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';

@Injectable({ providedIn: 'root' })
export class AuthService {

  constructor(private http: HttpClient, private router: Router) {}

  isAuthenticated(): boolean {
    return document.cookie.includes('.AspNetCore.Cookies');  // Substitua pelo nome do seu cookie
  }

  logout() {
    this.http.post('https://localhost:5001/api/auth/logout', {}).subscribe(() => {
      localStorage.removeItem('role');
      document.cookie = '.AspNetCore.Cookies=; Max-Age=0';
      this.router.navigate(['/login']);
    });
  }
}
