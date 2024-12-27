import { Component, OnInit } from '@angular/core';
import { MedicalConditionDto } from '../../../dto/medicalCondition.dto';
import { MedicalConditionViewModel } from '../../../application/viewmodels/medicalCondition.viewmodel';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { AuthService } from '../../../domain/services/auth.service';

@Component({
  selector: 'app-medical-condition-list',
  templateUrl: './medical-condition-list.component.html',
  styleUrls: ['./medical-condition-list.component.css']
})
export class MedicalConditionListComponent implements OnInit {
  medicalConditions: MedicalConditionDto[] = [];
  isLoading = false;
  message = '';

  // Filters
  codeFilter = '';
  designationFilter = '';

  // Update popup
  selectedForUpdate: MedicalConditionDto | null = null;

  get isAdmin(): boolean {
    // If the stored user role is 'Admin', return true
    return this.authService.currentUserSubject.value?.role === 'Admin';
  }


  constructor(
    private medicalConditionVM: MedicalConditionViewModel,
    private authService: AuthService) { }

  ngOnInit(): void {
    this.fetchMedicalConditions();
  }

  fetchMedicalConditions(): void {
    this.isLoading = true;
    this.message = '';

    this.medicalConditionVM.searchMedicalCondition(this.codeFilter, this.designationFilter)
      .pipe(
        catchError(error => {
          console.error('Error searching medical conditions:', error);
          this.message = 'Failed to fetch medical conditions.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((results) => {
        this.medicalConditions = results;
        this.isLoading = false;
        if (!results || !results.length) {
          this.message = 'No medical conditions found.';
        }
      });
  }

  search(): void {
    this.fetchMedicalConditions();
  }

  clearFilters(): void {
    this.codeFilter = '';
    this.designationFilter = '';
    this.fetchMedicalConditions();
  }

  openUpdatePopup(medCond: MedicalConditionDto): void {
    // Create a copy so we don't mutate the original until saved
    this.selectedForUpdate = { ...medCond };
  }

  closeUpdatePopup(refresh: boolean): void {
    this.selectedForUpdate = null;
    if (refresh) {
      this.fetchMedicalConditions();
    }
  }
}
