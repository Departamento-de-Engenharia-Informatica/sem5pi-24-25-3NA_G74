import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { OperationRequestRepository  } from '../../infrastructure/repositories/operationRequest-repository';
import { OperationRequest, OperationRequestDTO } from '../../domain/models/operationRequest.model';


@Injectable({
    providedIn: 'root'
})

export class OperationRequestService {

    constructor(private operationRepository: OperationRequestRepository ) { }

    async createOperation(operation: OperationRequest): Promise<OperationRequest> {
        return this.operationRepository.createOperationRequest(operation);
    }

    updateOperation(operation: Partial<OperationRequest>, id: BigInt): Observable<OperationRequest> {
        return this.operationRepository.updateOperationRequest(operation, id);
    }

    deleteOperation(id: BigInt): Observable<any> {
        return this.operationRepository.deleteOperationRequest(id);
    }

    listAllOperations(): Observable<OperationRequestDTO[]> {
        return this.operationRepository.listAllOperationRequests();
    }

}