import { Component, ViewEncapsulation } from '@angular/core';
import { Router } from '@angular/router';
import { LoginViewModel } from '../../../application/viewmodels/login-viewmodel';

declare const google: any; // Reference the Google Sign-In library

@Component({
  selector: 'app-main-menu',
  templateUrl: './main-menu.component.html',
  styleUrls: ['./main-menu.component.css'],
  encapsulation: ViewEncapsulation.None,
})
export class MainMenuComponent {
  constructor(private router: Router, private loginViewModel: LoginViewModel) { }

  // Called when the Google Sign-In button is clicked and returns a credential response
  handleCredentialResponse(response: any) {
    const token = response.credential;

    // Call the ViewModel to process Google Login
    this.loginViewModel.googleLogin(token).subscribe(
      (res) => {
        const role = res.user.role;
        this.redirectUser(role);
      },
      (error) => {
        console.error('Google login failed:', error);
        alert('Google login failed. Please try again.');
      }
    );
  }

  redirectToLogin() {
    this.router.navigate(['/login']);
  }

  redirectUser(role: string) {
    switch (role) {
      case 'Admin':
        this.router.navigate(['/admin']);
        break;
      case 'Patient':
        this.router.navigate(['/patient']);
        break;
      case 'Doctor':
        this.router.navigate(['/doctor']);
        break;
      default:
        this.router.navigate(['/main']);
    }
  }

  ngOnInit() {
    this.initializeGoogleSignIn();
  }

  // Initialize the Google Sign-In button
  initializeGoogleSignIn() {
    google.accounts.id.initialize({
      client_id: '786619119473-8nkdsd6n1j9fuv3o6isbrlcdgfbg526q.apps.googleusercontent.com',
      callback: (response: any) => this.handleCredentialResponse(response),
    });

    google.accounts.id.renderButton(
      document.getElementById('google-signin-button'),
      {
        theme: 'outline',
        size: 'large',
        text: 'signin_with',
      }
    );
  }
}
