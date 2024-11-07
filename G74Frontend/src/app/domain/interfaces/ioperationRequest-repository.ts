import { Observable } from 'rxjs';
import { OperationRequest } from '../models/operationRequest.model';

export interface IOperationRepository {
  createOperationRequest(operation: OperationRequest): Observable<OperationRequest>;

  updateOperationRequest(operation: Partial<OperationRequest>, medicalRecordNumber : string): Observable<OperationRequest>;
  
  deleteOperationRequest(medicalRecordNumber: string): Observable<any>;


  
}