import { Observable } from 'rxjs';
import { OperationType } from '../models/operationType.model';

export interface IOperationTypeRepository {

  listOperationTypesByFilter(criteria: Partial<OperationType>): Observable<OperationType[]>;

}
