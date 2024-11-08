import { Component, ViewEncapsulation  } from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {Router} from '@angular/router';

@Component({
  selector: 'app-main-menu',
  templateUrl: './main-menu.component.html',
  styleUrl: './main-menu.component.css',
  encapsulation: ViewEncapsulation.None
})
export class MainMenuComponent {

  constructor(private router: Router, private http: HttpClient) {}

  redirectToRegister() {
    this.router.navigate(['/register-user']);
  }

  handleCredentialResponse(response: any) {
    const token = response.credential;

    const headers = new HttpHeaders({'Content-Type': 'application/json'});
    this.http.post('https://localhost:5001/api/auth/google-login', {token}, {headers})
      .subscribe((res: any) => {
        if (res.success) {
          localStorage.setItem('role', res.role);
          this.redirectUser(res.role);
        }
      });
  }

  redirectUser(role: string) {
    switch (role) {
      case 'admin':
        this.router.navigate(['/admin']);
        break;
      case 'patient':
        this.router.navigate(['/patient']);
        break;
      case 'staff':
        this.router.navigate(['/staff']);
        break;
      case 'doctor':
        this.router.navigate(['/doctor']);
        break;
      default:
        this.router.navigate(['/main']);
    }
  }

}
