import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { OperationRequestRepository  } from '../../infrastructure/repositories/operationRequest-repository';
import { OperationRequest } from '../../domain/models/operationRequest.model';


@Injectable({
    providedIn: 'root'
})

export class OperationRequestService {

    constructor(private patientRepository: OperationRequestRepository ) { }

    createOperation(operation: OperationRequest): Observable<OperationRequest> {
        return this.patientRepository.createOperationRequest(operation);
    }

    updateOperation(operation: Partial<OperationRequest>, id: BigInt): Observable<OperationRequest> {
        return this.updateOperation(operation, id);
    }

    deleteOperation(id: BigInt): Observable<any> {
        return this.deleteOperation(id);
    }

    listAllOperations(): Observable<OperationRequest[]> {
        return this.listAllOperations();
    }

}