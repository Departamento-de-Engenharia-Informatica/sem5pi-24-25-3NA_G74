import { Component, ViewEncapsulation } from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Router} from '@angular/router';
import { HeaderStaticComponent } from '../header-static/header-static.component';
@Component({
  selector: 'app-doctor-menu',
  standalone: true,
  imports: [HeaderStaticComponent],
  templateUrl: './doctor-menu.component.html',
  styleUrl: './doctor-menu.component.css',
  encapsulation: ViewEncapsulation.None 
})
export class DoctorMenuComponent {
  constructor(private router: Router, private http: HttpClient) {}

  redirectToRegister() {
    this.router.navigate(['/doctor/create-operation']);
  }

  redirectToUpdate(){
    this.router.navigate(['/doctor/update-operation']);
  }

  redirectToDelete(){
    this.router.navigate(['/doctor/delete-operation']);
  }

  redirectToListAll(){
    this.router.navigate(['/doctor/list-operation']);
  }
}
