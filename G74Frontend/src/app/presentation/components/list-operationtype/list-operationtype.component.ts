import {Component, OnInit} from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {OperationType} from '../../../domain/models/operationType.model';
import {OperationTypeRepository} from '../../../infrastructure/repositories/operationType-repository';
import {OperationTypeService} from '../../../application/services/operationType.service';

@Component({
  selector: 'app-list-operationtype',
  templateUrl: './list-operationtype.component.html',
  styleUrl: './list-operationtype.component.css'
})
export class ListOperationtypeComponent{

  operationTypes: OperationType[] = [];
  errorMessage: string = '';

  filters: Partial<OperationType> = {
    operationTypeId: '',
    name: '',
    requiredStaffBySpecialization: '',
    duration: ''
  };

  constructor(private operationTypeService: OperationTypeService) {}

  search(): void {
    const criteria: Partial<OperationType> = {};
    for (const key in this.filters) {
      const typedKey = key as keyof OperationType; 
      if (this.filters[typedKey]) {
        criteria[typedKey] = this.filters[typedKey];
      }
    }
    this.operationTypeService.listOperationTypesByFilter(criteria).subscribe({
      next: (data) => {
        this.operationTypes = data;
        this.errorMessage = '';
      },
      error: (error) => {
        this.operationTypes = [];
        this.errorMessage = 'Failed to load operation types.';
        console.error(error);
      }
    });
  }
}

