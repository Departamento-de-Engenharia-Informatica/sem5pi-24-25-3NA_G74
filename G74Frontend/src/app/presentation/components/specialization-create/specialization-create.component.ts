import { Component, inject } from '@angular/core';
import { ReactiveFormsModule, FormControl, FormGroup } from '@angular/forms';
import { Validators } from '@angular/forms';
import { catchError } from 'rxjs/operators';
import { SpecializationViewModel } from '../../../application/viewmodels/specialization.viewmodel';
import { Specialization } from '../../../domain/models/specialization.model';
import { of } from 'rxjs';

@Component({
  selector: 'app-specialization-create',
  standalone: true,
  imports: [ReactiveFormsModule],
  templateUrl: './specialization-create.component.html',
  styleUrl: './specialization-create.component.css'
})
export class SpecializationCreateComponent {
  specializationService = inject(SpecializationViewModel);

  message = '';

  specializationForm = new FormGroup({
    code: new FormControl('', Validators.required),
    designation: new FormControl('', Validators.required),
  });



  handleSubmit() {

    const specialization: Specialization = {
      code: this.specializationForm.value.code || '',
      designation: this.specializationForm.value.designation || '',
    };

    // Call the service to create the specialization
    this.specializationService.createSpecialization(specialization).pipe(
      catchError(error => {
        console.error('Error creating specialization:', error);  // Log the error details
        this.message = `Failed to create specialization. ${error?.error?.message || 'Please try again.'}`;
        // Additional logging for debugging
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);  // Return a null observable to complete the stream
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'Specialization created successfully!';
      }
    });
  }
}
