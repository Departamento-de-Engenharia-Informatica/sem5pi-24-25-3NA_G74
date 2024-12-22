import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { LoginViewModel } from '../../../application/viewmodels/login-viewmodel';

@Component({
  selector: 'app-admin-menu',
  templateUrl: './admin-menu.component.html',
  styleUrls: ['./admin-menu.component.css'],
  standalone:true,
  imports: [],
})
export class AdminMenuComponent {
  constructor(private router: Router, private loginViewModel: LoginViewModel) {}

  navigateTo(path: string): void {
    this.router.navigate([`/admin/${path}`]);
  }

  logout(): void {
    this.loginViewModel.logout(); // Calls the logout function in LoginViewModel
    this.router.navigate(['/main']); // Redirect to the main page
  }
}
