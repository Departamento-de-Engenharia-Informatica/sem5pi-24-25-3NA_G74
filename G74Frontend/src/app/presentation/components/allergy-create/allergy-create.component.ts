import { Component } from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {AllergyDTO} from '../../../dto/allergy.dto';
import {AllergyViewModel} from '../../../application/viewmodels/allergy.viewmodel';

@Component({
  selector: 'app-allergy-create',
  templateUrl: './allergy-create.component.html',
  styleUrl: './allergy-create.component.css'
})
export class AllergyCreateComponent {
  allergy: AllergyDTO = {
    code: '',
    designation: '',
    description: ''
  };

  isLoading = false;
  message = '';

  constructor(private allergyVM: AllergyViewModel) {}

  onSubmit(): void {
    this.isLoading = true;
    this.message = '';

    // Call the ViewModel to create the medical condition
    this.allergyVM
      .createAllergy(this.allergy)
      .pipe(
        catchError(error => {
          console.error('Error creating allergy:', error);
          this.message = error?.error?.message || 'Failed to create allergy.';
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe(response => {
        this.isLoading = false;
        if (response) {
          this.message = 'Allergy created successfully!';
          this.resetForm();
        }
      });
  }

  resetForm(): void {
    this.allergy = {
      code: '',
      designation: '',
      description: ''
    };
  }
}
