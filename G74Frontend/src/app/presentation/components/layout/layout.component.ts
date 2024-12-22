// layout.component.ts
import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../../../domain/services/auth.service';
import { LoginViewModel } from '../../../application/viewmodels/login-viewmodel'

// If you are using standalone components, import everything you need:
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

@Component({
  selector: 'app-layout',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './layout.component.html',
  styleUrls: ['./layout.component.css']
})
export class LayoutComponent implements OnInit {

  // Variables from your header logic
  isMenuVisible = true;       // Whether to show/hide the left navbar
  urlSegments: string[] = []; // For your path buttons ("/main", "/admin", etc.)
  user: any;                  // Current logged in user
  currentUrl: string = '';    // The current URL

  constructor(
    private router: Router,
    private authService: AuthService,
    private loginViewModel: LoginViewModel
  ) { }

  ngOnInit(): void {
    // Grab current user from your AuthService
    this.user = this.authService.currentUserSubject.value;

    // For your path buttons:
    this.currentUrl = this.router.url;
    this.urlSegments = this.currentUrl.split('/').filter(segment => segment);

    // Optionally log for debugging
    console.log('LayoutComponent -> user:', this.user);
    console.log('LayoutComponent -> currentUrl:', this.currentUrl);
  }

  // ===================
  // HEADER LOGIC
  // ===================
  toggleMenu(): void {
    this.isMenuVisible = !this.isMenuVisible;
  }

  navigateTo(segment: string): void {
    // This logic was in your header:
    // Finds the index of the segment in the current path array
    const index = this.urlSegments.indexOf(segment);

    // Builds a path up to that segment
    const path = this.urlSegments.slice(0, index + 1).join('/');
    this.router.navigate([`/${path}`]);
  }

  navigateToHome(): void {
    // Example: navigate to Home route
    this.router.navigate(['/home']);
  }

  logout(): void {
    this.loginViewModel.logout();  // Or call a method in authService
    this.router.navigate(['/main']); // Or wherever you want after logout
  }

  // ===================
  // NAVBAR LOGIC
  // ===================

}
