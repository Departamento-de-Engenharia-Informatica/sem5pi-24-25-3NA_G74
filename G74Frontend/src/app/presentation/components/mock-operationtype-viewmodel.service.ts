import { of } from 'rxjs';
import {OperationType} from '../../domain/models/operationType.model';

export class MockOperationTypeViewModel {
  private mockOperationTypes: OperationType[] = [
    {
      operationTypeId: '1',
      name: 'Surgery',
      requiredStaffBySpecialization: 'Anesthesiologist:1; Surgeon:1',
      duration: '120',
    },
    {
      operationTypeId: '2',
      name: 'Radiology',
      requiredStaffBySpecialization: 'Radiologist:1',
      duration: '60',
    },
  ];

  listOperationType(filters: Partial<OperationType> | null) {
    if (!filters) {
      return of(this.mockOperationTypes);
    }
    const filteredOperationTypes = this.mockOperationTypes.filter((op) => {
      return Object.keys(filters).every((key) => {
        const filterValue = (filters as any)[key];
        const operationValue = (op as any)[key];
        return !filterValue || operationValue?.toString().includes(filterValue);
      });
    });

    return of(filteredOperationTypes);
  }
}
