import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { OperationRequest } from '../../domain/models/operationRequest.model';
import { IOperationRepository } from '../../domain/interfaces/ioperationRequest-repository';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class OperationRequestRepository implements IOperationRepository {
  private apiUrl = `${environment.apiUrl}/patient/`;

  constructor(private http: HttpClient) {}
    createOperationRequest(operation: OperationRequest): Observable<OperationRequest> {
        console.log("Operation Request Criada com Sucesso!")
        throw new Error('Method not implemented.');
        
    }
    updateOperationRequest(operation: Partial<OperationRequest>, medicalRecordNumber: string): Observable<OperationRequest> {
        console.log("Operation Request Atualizada com Sucesso!")
        throw new Error('Method not implemented.');
    }
    deleteOperationRequest(medicalRecordNumber: string): Observable<any> {
        console.log("Operation Request Apagada com sucesso!")
        throw new Error('Method not implemented.');
    }

}
