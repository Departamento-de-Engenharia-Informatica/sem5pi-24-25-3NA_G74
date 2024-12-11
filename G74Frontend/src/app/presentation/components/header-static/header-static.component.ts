import { CommonModule } from '@angular/common';
import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { Router } from '@angular/router';
import { NavbarComponent } from '../navbar/navbar.component';
import { AuthService } from '../../../domain/services/auth.service';
import {LoginViewModel} from '../../../application/viewmodels/login-viewmodel'
@Component({
  selector: 'app-header-static',
  templateUrl: './header-static.component.html',
  styleUrls: ['./header-static.component.css'],
  standalone:true,
  encapsulation: ViewEncapsulation.None ,
  imports: [CommonModule, NavbarComponent]
})
export class HeaderStaticComponent implements OnInit{
  currentUrl: string | undefined;
  urlSegments: string[] = [];
  isMenuVisible = true;
  user: any;
  constructor(private router:Router,private authService: AuthService, private loginViewModel: LoginViewModel) {
  }

  ngOnInit(): void {
    this.currentUrl = this.router.url;
    this.urlSegments = this.currentUrl.split('/').filter(segment => segment);
    this.user = this.authService.currentUserSubject.value;
  
  }

  navigateTo(segment: string): void {
    const index = this.urlSegments.indexOf(segment);
    const path = this.urlSegments.slice(0, index + 1).join('/');
    this.router.navigate([`/${path}`]);
  }
  navigateToHome()
  {
    this.router.navigate(['/'])
  }

  toggleMenu() {
    this.isMenuVisible = !this.isMenuVisible;
  }

  logout(){
    console.log("Ola");
    this.loginViewModel.logout();
  }

  
}

