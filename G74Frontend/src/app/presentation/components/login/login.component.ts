import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { LoginInfo } from '../../../domain/models/login.model';
import { LoginViewModel } from '../../../application/viewmodels/login-viewmodel';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css'],
})
export class LoginComponent {
  loginData: LoginInfo = { email: '' };
  errorMessage: string | null = null;
  isLoading = false;

  constructor(private vm: LoginViewModel, private router: Router) { }

  login() {
    this.errorMessage = null;
    this.isLoading = true;

    this.vm.login(this.loginData).subscribe(
      (response) => {
        this.isLoading = false;

        // Navigate based on the user's role
        if (response.user.role === 'Admin') {
          this.router.navigate(['/admin']);
        } else if (response.user.role === 'Patient') {
          this.router.navigate(['/patient']);
        } else if (response.user.role === 'Doctor') {
          this.router.navigate(['/doctor']);
        } else {
          this.router.navigate(['/']); // Fallback
        }
      },
      (error) => {
        this.isLoading = false;
        this.errorMessage = error.error?.message || 'Login failed. Try again.';
      }
    );
  }
}
