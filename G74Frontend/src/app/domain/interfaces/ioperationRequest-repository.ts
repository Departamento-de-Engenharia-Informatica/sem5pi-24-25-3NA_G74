import { Observable } from 'rxjs';
import { OperationRequest, OperationRequestDTO } from '../models/operationRequest.model';

export interface IOperationRepository {

  createOperationRequest(operation: OperationRequest): Promise<OperationRequest>;

  updateOperationRequest(operation: Partial<OperationRequest>, id : BigInt): Observable<OperationRequest>;
  
  deleteOperationRequest(id: BigInt): Observable<any>;

  listAllOperationRequests(): Observable<OperationRequestDTO[]>;
  
}