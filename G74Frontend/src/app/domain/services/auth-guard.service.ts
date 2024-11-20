import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { AuthService } from './auth.service';



@Injectable({
  providedIn: 'root',
})
export class AuthGuard implements CanActivate {
  constructor(private authService: AuthService, private router: Router) {}

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    const user = this.authService.currentUserSubject.value;
    const requiredRole = route.data['role'];

    //console.log('AuthGuard - User:', user); // Debug log
    //console.log('AuthGuard - Required Role:', requiredRole); // Debug log

    if (user) {
      // Allow access if the user's role matches the required role or no role is specified
      //console.log('user.role:', user.role);

      if (!requiredRole || user.role === requiredRole) {
        return true;
      } else {
        console.warn(`Unauthorized access - User role '${user.role}' is not allowed.`);
        this.router.navigate(['/unauthorized']); // Redirect to an unauthorized page
        return false;
      }
    }

    // If no user is logged in, redirect to login
    console.warn('Unauthorized access attempt - no user logged in.');
    this.router.navigate(['/login']);
    return false;
  }
}
