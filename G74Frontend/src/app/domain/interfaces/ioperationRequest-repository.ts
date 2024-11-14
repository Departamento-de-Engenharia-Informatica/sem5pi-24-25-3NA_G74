import { Observable } from 'rxjs';
import { OperationRequest, OperationRequestDTO } from '../models/operationRequest.model';

export interface IOperationRepository {

  createOperationRequest(operation: OperationRequest): Promise<OperationRequest>;

  updateOperationRequest(operation: Partial<OperationRequest>, id : number): Observable<OperationRequest>;
  
  deleteOperationRequest(id: number): Observable<any>;

  listAllOperationRequests(): Observable<OperationRequestDTO[]>;
  
}