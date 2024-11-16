import { Component, inject } from '@angular/core';
import { StaffService } from '../../../application/services/staff.service';
import { Staff } from '../../../domain/models/staff.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { ConfirmationDialogComponent } from '../confirmation-dialog/confirmation-dialog.component';

@Component({
  selector: 'app-list-all-staff',
  standalone: true,
  imports: [CommonModule, ConfirmationDialogComponent],
  templateUrl: './list-all-staff.component.html',
  styleUrl: './list-all-staff.component.css'
})
export class ListAllStaffComponent {
  staffService = inject(StaffService);
  router = inject(Router);
  staffMembers: Staff[] = [];
  message = '';
  showConfirmDialog = false;
  selectedLicenceNumberToDeactivate: string = '';

  ngOnInit() {
    this.getAllStaff();
  }

  getAllStaff() {
    // Call the service to get all staff profiles
    this.staffService.getAllStaff().pipe(
      catchError(error => {
        console.error('Error fetching staff profiles:', error);  // Log the error details
        this.message = `Failed to fetch staff profiles. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of([]);  // Return an empty array as the observable value to complete the stream
      })
    ).subscribe(staffMembers => {
      if (staffMembers) {
        this.staffMembers = staffMembers;
      } else {
        this.message = 'No staff profiles found';
      }
    });
  }

  editStaff(licenceNumber: string) {
    this.router.navigate([`/admin/update-staff/${licenceNumber}`]);
  }

  deactivateStaff(licenceNumber: string) {
    this.router.navigate([`/admin/deactivate-staff/${licenceNumber}`]);
  }

  showDeactivateConfirmation(licenceNumber: string) {
    this.selectedLicenceNumberToDeactivate = licenceNumber;
    this.showConfirmDialog = true;
  }

  handleConfirmDeactivate() {
    // Call your deactivate service here
    this.staffService.deactivateStaff(this.selectedLicenceNumberToDeactivate).pipe(
      catchError(error => {
        console.error('Error deactivating staff:', error);
        this.message = `Failed to deactivate staff. ${error?.error?.message || 'Please try again.'}`;
        return of(null);
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Staff deactivated successfully!';
        this.getAllStaff(); // Refresh the list
      }
      this.showConfirmDialog = false;
    });
  }

  handleCancelDeactivate() {
    this.showConfirmDialog = false;
    this.selectedLicenceNumberToDeactivate = '';
  }
}
