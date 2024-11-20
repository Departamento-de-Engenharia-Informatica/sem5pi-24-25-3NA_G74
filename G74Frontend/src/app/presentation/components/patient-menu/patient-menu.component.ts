import { Component } from '@angular/core';
import {Router} from '@angular/router';

@Component({
  selector: 'app-patient-menu',
  templateUrl: './patient-menu.component.html',
  styleUrl: './patient-menu.component.css'
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
