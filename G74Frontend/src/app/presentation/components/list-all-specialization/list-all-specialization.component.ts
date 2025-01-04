import { Component, inject } from '@angular/core';
import { SpecializationViewModel } from '../../../application/viewmodels/specialization.viewmodel';
import { Specialization } from '../../../domain/models/specialization.model';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { ConfirmationDialogComponent } from '../confirmation-dialog/confirmation-dialog.component';

@Component({
  selector: 'app-list-all-specialization',
  standalone: true,
  imports: [CommonModule, ConfirmationDialogComponent],
  templateUrl: './list-all-specialization.component.html',
  styleUrl: './list-all-specialization.component.css'
})
export class ListAllSpecializationComponent {
  specializationService = inject(SpecializationViewModel);
  router = inject(Router);
  specializations: Specialization[] = [];
  message = '';
  showConfirmDialog = false;

  ngOnInit() {
    this.getAllSpecialization();
  }

  getAllSpecialization() {
    // Call the service to get all specializations
    this.specializationService.getAllSpecialization().pipe(
      catchError(error => {
        console.error('Error fetching specializations:', error);  // Log the error details
        this.message = `Failed to fetch specializations. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of([]);  // Return an empty array as the observable value to complete the stream
      })
    ).subscribe(specializations => {
      if (specializations) {
        this.specializations = specializations;
      } else {
        this.message = 'No specializations found';
      }
    });
  }
}
