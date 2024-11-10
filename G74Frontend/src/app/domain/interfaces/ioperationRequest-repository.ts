import { Observable } from 'rxjs';
import { OperationRequest } from '../models/operationRequest.model';

export interface IOperationRepository {

  createOperationRequest(operation: OperationRequest): Observable<OperationRequest>;

  updateOperationRequest(operation: Partial<OperationRequest>, id : BigInt): Observable<OperationRequest>;
  
  deleteOperationRequest(id: BigInt): Observable<any>;

  listAllOperationRequests(): Observable<OperationRequest[]>;
  
}