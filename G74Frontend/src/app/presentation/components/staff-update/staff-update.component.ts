import { Component, inject } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Staff } from '../../../domain/models/staff.model';
import { StaffService } from '../../../application/services/staff.service';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { ReactiveFormsModule, FormControl, FormGroup } from '@angular/forms';
import { Validators } from '@angular/forms';

@Component({
  selector: 'app-staff-update',
  standalone: true,
  imports: [ReactiveFormsModule],
  templateUrl: './staff-update.component.html',
  styleUrl: './staff-update.component.css'
})
export class StaffUpdateComponent {
  staffService = inject(StaffService);

  licenceNumber: string = '';
  staff = {} as Staff;
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

  route = inject(ActivatedRoute);

  ngOnInit() {
    this.licenceNumber = this.route.snapshot.paramMap.get('licenceNumber') || '';

    this.staffService.getStaffByLicenceNumber(this.licenceNumber).pipe(
      catchError(error => {
        console.error('Error fetching staff profile:', error);  // Log the error details
        this.message = `Failed to fetch staff profile. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);  // Return a null as the observable value to complete the stream
      })
    ).subscribe(staff => {
      if (staff) {
        this.staff = staff;

        // Set form values after receiving the staff data
        this.staffForm.patchValue({
          licenceNumber: staff.licenceNumber,
          name: staff.name,
          phoneNumber: staff.phoneNumber,
          contactEmail: staff.contactEmail,
          staffSpecialization: staff.staffSpecialization,
          status: staff.status,
          availability: staff.availability
        });
      } else {
        this.message = 'No staff profile found';
      }
    });
  }

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
    this.staffService.updateStaffProfile(this.licenceNumber, staff).pipe(
      catchError(error => {
        console.error('Error updating staff profile:', error);  // Log the error details
        this.message = `Failed to update staff profile. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);  // Return a null observable to complete the stream
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Staff profile updates successfully!';
      }
    });
  }
}
