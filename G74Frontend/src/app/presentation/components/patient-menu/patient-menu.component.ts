import { Component } from '@angular/core';
import {Router} from '@angular/router';
import { HeaderStaticComponent } from '../header-static/header-static.component';
@Component({
  selector: 'app-patient-menu',
  templateUrl: './patient-menu.component.html',
  styleUrl: './patient-menu.component.css',
  standalone:true,
  imports:[HeaderStaticComponent]
})
export class PatientMenuComponent {
  constructor(private router: Router) {}


  navigateToUpdate() {
    this.router.navigate(['/patient/update-user']);
  }

  navigateToDeleteUser() {
    this.router.navigate(['/patient/delete-user'])
  }
}
