import {Component, OnInit} from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {OperationType} from '../../../domain/models/operationType.model';
import {OperationTypeViewModel} from '../../../application/viewmodels/operationtype-viewmodel';

@Component({
  selector: 'app-list-operationtype',
  templateUrl: './list-operationtype.component.html',
  styleUrl: './list-operationtype.component.css'
})
export class ListOperationtypeComponent implements OnInit{

  operationTypes: OperationType[] = [];
  message: string = '';

  filters: Partial<OperationType> = {
    operationTypeId: '',
    name: '',
    requiredStaffBySpecialization: '',
    duration: ''
  };
  isLoading: boolean=false;

  constructor(private operationTypeViewModel: OperationTypeViewModel) {}

  ngOnInit(): void {
    this.fetchOperationType();
  }

  fetchOperationType(): void {
    this.isLoading = true;

    const filters: Partial<OperationType> = { ...this.filters };

    this.operationTypeViewModel
      .listOperationType(Object.keys(filters).length ? filters : null)
      .pipe(
        catchError((error) => {
          console.error('Error fetching operation types:', error);
          this.message = 'Failed to fetch operation types. Please try again.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((operationtypes) => {
        this.operationTypes = operationtypes;
        console.log('Operation Types:', operationtypes);
        this.message = operationtypes.length ? '' : 'No operation types found.';
        this.isLoading = false;
      });
  }

  clearFilters(): void {
    this.filters = {
      operationTypeId: '',
      name: '',
      requiredStaffBySpecialization: '',
      duration: ''
    };
  }
}

