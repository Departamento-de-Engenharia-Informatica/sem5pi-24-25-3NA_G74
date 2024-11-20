import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { OperationTypeRepository } from '../../infrastructure/repositories/operationType-repository';
import { OperationType } from '../../domain/models/operationType.model';


@Injectable({
    providedIn: 'root'
})

export class OperationTypeService {

    constructor(private operationTypeRepository: OperationTypeRepository ) { }

    listOperationTypesByFilter(criteria: Partial<OperationType>): Observable<OperationType[]> {
      return this.operationTypeRepository.listOperationTypesByFilter(criteria);
    }


}
