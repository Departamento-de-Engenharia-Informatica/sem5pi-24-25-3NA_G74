import { Component, inject } from '@angular/core';
import { ReactiveFormsModule, FormControl, FormGroup } from '@angular/forms';
import { Validators } from '@angular/forms';
import { catchError } from 'rxjs/operators';
import { StaffService } from '../../../application/services/staff.service';
import { Staff } from '../../../domain/models/staff.model';
import { of } from 'rxjs';

@Component({
  selector: 'app-staff-create',
  standalone: true,
  imports: [ReactiveFormsModule],
  templateUrl: './staff-create.component.html',
  styleUrl: './staff-create.component.css'
})
export class StaffCreateComponent {
  staffService = inject(StaffService);

  message = '';

  staffForm = new FormGroup({
    licenceNumber: new FormControl('', Validators.required),
    name: new FormControl('', Validators.required),
    phoneNumber: new FormControl('', Validators.required),
    contactEmail: new FormControl('', [Validators.required, Validators.email]),
    staffSpecialization: new FormControl('', Validators.required),
    status: new FormControl('', Validators.required),
    availability: new FormControl('', Validators.required),
  });



  handleSubmit() {

    const staff: Staff = {
      licenceNumber: this.staffForm.value.licenceNumber || '',
      name: this.staffForm.value.name || '',
      phoneNumber: this.staffForm.value.phoneNumber || '',
      contactEmail: this.staffForm.value.contactEmail || '',
      staffSpecialization: this.staffForm.value.staffSpecialization || '',
      status: this.staffForm.value.status || '',
      availability: this.staffForm.value.availability || ''
    };

    // Call the service to create the staff profile
    this.staffService.createStaffProfile(staff).pipe(
      catchError(error => {
        console.error('Error creating staff profile:', error);  // Log the error details
        this.message = `Failed to create staff profile. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);  // Return a null observable to complete the stream
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Staff profile created successfully!';
      }
    });
  }
}
