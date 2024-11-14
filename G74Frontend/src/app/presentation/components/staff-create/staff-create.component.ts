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
  template: `
    <form [formGroup]="staffForm" (ngSubmit)="handleSubmit()" class="p-4">
      <div class="mb-3">
        <label for="licenceNumber" class="form-label">Licence Number:</label>
        <input 
          id="licenceNumber"
          type="text"
          formControlName="licenceNumber"
          class="form-control">
      </div>

      <div class="mb-3">
        <label for="name" class="form-label">Name:</label>
        <input 
          id="name"
          type="text"
          formControlName="name"
          class="form-control">
      </div>

      <div class="mb-3">
        <label for="phoneNumber" class="form-label">Phone Number:</label>
        <input 
          id="phoneNumber"
          type="tel"
          formControlName="phoneNumber"
          class="form-control">
      </div>

      <div class="mb-3">
        <label for="contactEmail" class="form-label">Email:</label>
        <input 
          id="contactEmail"
          type="email"
          formControlName="contactEmail"
          class="form-control">
      </div>

      <div class="mb-3">
        <label for="staffSpecialization" class="form-label">Specialization:</label>
        <input 
          id="staffSpecialization"
          type="text"
          formControlName="staffSpecialization"
          class="form-control">
      </div>

      <div class="mb-3">
        <label for="status" class="form-label">Status:</label>
        <select 
          id="status"
          formControlName="status"
          class="form-control"
        >
        <option value="">Select Status</option>
        <option value="active">Active</option>
        <option value="inactive">Inactive</option>
        </select>
      </div>

      <div class="mb-3">
        <label for="availability" class="form-label">Availability:</label>
        <input 
          id="availability"
          type="text"
          formControlName="availability"
          class="form-control">
      </div>

      <button type="submit" class="btn btn-primary" [disabled]="!staffForm.valid">Submit</button>
    </form>
    @if (message) {
      <p>{{ message }}</p>
    }
  `,
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
