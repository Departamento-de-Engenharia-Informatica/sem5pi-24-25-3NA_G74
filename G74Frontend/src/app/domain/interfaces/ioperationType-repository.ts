import { Observable } from 'rxjs';
import { OperationType } from '../models/operationType.model';

export interface IOperationTypeRepository {

  listAllOperationType(): Observable<OperationType[]>;
  
}