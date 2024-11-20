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

    if (user && (user.role === requiredRole || !requiredRole)) {
      return true;
    }

    if (!user) {
      console.warn('Unauthorized access attempt - no user logged in.');
      this.router.navigate(['/login']);
      return false;
    }

    this.router.navigate(['/login']);
    return false;
  }
}