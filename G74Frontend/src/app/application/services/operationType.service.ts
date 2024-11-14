import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { OperationTypeRepository } from '../../infrastructure/repositories/operationType-repository';
import { OperationType } from '../../domain/models/operationType.model';


@Injectable({
    providedIn: 'root'
})

export class OperationRequestService {

    constructor(private operationRepository: OperationTypeRepository ) { }

    

    listAllOperations(): Observable<OperationType[]> {
        return this.operationRepository.listAllOperationType();
    }

}