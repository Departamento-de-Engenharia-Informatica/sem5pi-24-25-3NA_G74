import {Injectable} from '@angular/core';
import {OperationTypeService} from '../../domain/services/operation-type.service';
import {Patient} from '../../domain/models/patient.model';
import {Observable} from 'rxjs';
import {OperationType} from '../../domain/models/operationType.model';

@Injectable({
  providedIn: 'root'
})

export class OperationTypeViewModel{

  constructor(private operationtypeService: OperationTypeService) {
  }

  listOperationType(operationtype: Partial<OperationType> | null): Observable<OperationType[]> {
    return this.operationtypeService.listOperationType(operationtype);
  }

}
