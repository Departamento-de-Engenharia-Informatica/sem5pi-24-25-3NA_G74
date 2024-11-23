import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListOperationtypeComponent } from './list-operationtype.component';
import { OperationTypeViewModel } from '../../../application/viewmodels/operationtype-viewmodel';
import { of, throwError } from 'rxjs';
import { OperationType } from '../../../domain/models/operationType.model';
import {FormsModule} from '@angular/forms';

describe('ListOperationtypeComponent', () => {
  let component: ListOperationtypeComponent;
  let fixture: ComponentFixture<ListOperationtypeComponent>;
  let operationTypeViewModelMock: jasmine.SpyObj<OperationTypeViewModel>;

  beforeEach(() => {
    operationTypeViewModelMock = jasmine.createSpyObj('OperationTypeViewModel', ['listOperationType']);

    TestBed.configureTestingModule({
      declarations: [ListOperationtypeComponent],
      providers: [
        { provide: OperationTypeViewModel, useValue: operationTypeViewModelMock }
      ],
      imports: [FormsModule]
    }).compileComponents();

    fixture = TestBed.createComponent(ListOperationtypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should fetch operation types successfully', () => {
    const mockOperationTypes: OperationType[] = [
      { operationTypeId: '1', name: 'Test Operation 1', requiredStaffBySpecialization: '5', duration: '30' },
      { operationTypeId: '2', name: 'Test Operation 2', requiredStaffBySpecialization: '10', duration: '45' }
    ];

    // Simular a resposta bem-sucedida da chamada listOperationType
    operationTypeViewModelMock.listOperationType.and.returnValue(of(mockOperationTypes));

    // Chamar o método
    component.fetchOperationType();

    // Verificar os resultados
    expect(component.isLoading).toBe(false);
    expect(component.operationTypes.length).toBe(2);
    expect(component.message).toBe('');
  });

  it('should handle error while fetching operation types', () => {
    // Simular erro na chamada listOperationType
    operationTypeViewModelMock.listOperationType.and.returnValue(throwError('Error fetching operation types'));

    // Chamar o método
    component.fetchOperationType();

    // Verificar os resultados
    expect(component.isLoading).toBe(false);
    expect(component.message).toBe('Failed to fetch operation types. Please try again.');
    expect(component.operationTypes.length).toBe(0);
  });

  it('should handle empty response', () => {
    // Simular uma resposta vazia
    operationTypeViewModelMock.listOperationType.and.returnValue(of([]));

    // Chamar o método
    component.fetchOperationType();

    // Verificar os resultados
    expect(component.isLoading).toBe(false);
    expect(component.message).toBe('No operation types found.');
    expect(component.operationTypes.length).toBe(0);
  });
});
