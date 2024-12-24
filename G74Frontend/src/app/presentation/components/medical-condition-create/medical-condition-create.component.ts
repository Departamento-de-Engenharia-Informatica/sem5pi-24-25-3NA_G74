import { Component } from '@angular/core';
import { MedicalConditionDto } from '../../../dto/medicalCondition.dto';
import { MedicalConditionViewModel } from '../../../application/viewmodels/medicalCondition.viewmodel';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { NgModule } from '@angular/core';

import { BrowserModule } from '@angular/platform-browser';

@Component({
  selector: 'app-create-medical-condition',
  templateUrl: './medical-condition-create.component.html',
  styleUrls: ['./medical-condition-create.component.css']
})
export class CreateMedicalConditionComponent {
  medicalCondition: MedicalConditionDto = {
    medicalConditionCode: '',
    designation: '',
    description: '',
    commonSymptoms: ''
  };

  isLoading = false;
  message = '';

  constructor(private medicalConditionVM: MedicalConditionViewModel) {}

  onSubmit(): void {
    this.isLoading = true;
    this.message = '';

    // Call the ViewModel to create the medical condition
    this.medicalConditionVM
      .createMedicalCondition(this.medicalCondition)
      .pipe(
        catchError(error => {
          console.error('Error creating Medical Condition:', error);
          this.message = error?.error?.message || 'Failed to create Medical Condition.';
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe(response => {
        this.isLoading = false;
        if (response) {
          this.message = 'Medical Condition created successfully!';
          this.resetForm();
        }
      });
  }

  resetForm(): void {
    this.medicalCondition = {
      medicalConditionCode: '',
      designation: '',
      description: '',
      commonSymptoms: ''
    };
  }
}
