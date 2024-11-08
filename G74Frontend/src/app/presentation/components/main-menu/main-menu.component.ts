import { Component, ViewEncapsulation } from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Router} from '@angular/router';

@Component({
  selector: 'app-main-menu',
  templateUrl: './main-menu.component.html',
  styleUrls: ['./main-menu.component.css'],
  encapsulation: ViewEncapsulation.None 
})
export class MainMenuComponent {

  constructor(private router: Router, private http: HttpClient) {}

  redirectToRegister() {
    this.router.navigate(['/register-user']);
  }



  }
