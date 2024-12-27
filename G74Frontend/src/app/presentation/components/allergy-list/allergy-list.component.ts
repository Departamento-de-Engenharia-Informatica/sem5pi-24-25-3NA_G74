import {Component, OnInit} from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {AllergyDTO} from '../../../dto/allergy.dto';
import {AllergyViewModel} from '../../../application/viewmodels/allergy.viewmodel';

@Component({
  selector: 'app-allergy-list',
  templateUrl: './allergy-list.component.html',
  styleUrl: './allergy-list.component.css'
})
export class AllergyListComponent implements OnInit {
  allergy: AllergyDTO[] = [];
  isLoading = false;
  message = '';

  codeFilter = '';

  selectedForUpdate: AllergyDTO | null = null;

  constructor(private allergyVM: AllergyViewModel) {}

  ngOnInit(): void {
    this.fetchAllergy();
  }

  fetchAllergy(): void {
    this.isLoading = true;
    this.message = '';

    this.allergyVM.searchAllergy(this.codeFilter)
      .pipe(
        catchError(error => {
          console.error('Error searching allergy:', error);
          this.message = 'Failed to fetch allergy.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((results) => {
        this.allergy = results;
        this.isLoading = false;
        if (!results || !results.length) {
          this.message = 'No allergy found.';
        }
      });
  }

  search(): void {
    this.fetchAllergy();
  }

  clearFilters(): void {
    this.codeFilter = '';
    this.fetchAllergy();
  }

  openUpdatePopup(allergy: AllergyDTO): void {
    // Create a copy so we don't mutate the original until saved
    this.selectedForUpdate = { ...allergy };
  }

  closeUpdatePopup(refresh: boolean): void {
    this.selectedForUpdate = null;
    if (refresh) {
      this.fetchAllergy();
    }
  }
}
