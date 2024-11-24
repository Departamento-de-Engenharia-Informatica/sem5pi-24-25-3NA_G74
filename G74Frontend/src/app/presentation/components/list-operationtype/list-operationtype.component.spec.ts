import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListOperationtypeComponent } from './list-operationtype.component';
import { OperationTypeViewModel } from '../../../application/viewmodels/operationtype-viewmodel';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import {MockOperationTypeViewModel} from '../mock-operationtype-viewmodel.service';

describe('ListOperationtypeComponent', () => {
  let component: ListOperationtypeComponent;
  let fixture: ComponentFixture<ListOperationtypeComponent>;
  let mockViewModel: MockOperationTypeViewModel;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ListOperationtypeComponent],
      imports: [FormsModule],
      providers: [{ provide: OperationTypeViewModel, useClass: MockOperationTypeViewModel }],
    }).compileComponents();

    fixture = TestBed.createComponent(ListOperationtypeComponent);
    component = fixture.componentInstance;
    mockViewModel = TestBed.inject(OperationTypeViewModel) as unknown as MockOperationTypeViewModel;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch operation types with filters applied', () => {
    spyOn(mockViewModel, 'listOperationType').and.callThrough();

    component.filters.operationTypeId = '1';
    component.filters.name = 'Surgery';
    component.filters.requiredStaffBySpecialization = 'Anesthesiologist';
    component.filters.duration = '120';

    component.fetchOperationType();

    expect(mockViewModel.listOperationType).toHaveBeenCalledWith(
      jasmine.objectContaining({
        operationTypeId: '1',
        name: 'Surgery',
        requiredStaffBySpecialization: 'Anesthesiologist',
        duration: '120',
      })
    );
  });

  it('should display an error message when fetching operation types fails', () => {
    spyOn(mockViewModel, 'listOperationType').and.returnValue(
      throwError({ error: { message: 'No operation types found.' } })
    );

    component.fetchOperationType();

    expect(component.message).toBe('No operation types found.');
    expect(component.isLoading).toBeFalse();
  });

  it('should show a message when no operation types are found', () => {
    spyOn(mockViewModel, 'listOperationType').and.returnValue(of([]));

    component.fetchOperationType();

    expect(component.message).toBe('No operation types found.');
    expect(component.operationTypes.length).toBe(0);
  });

  it('should clear filters and fetch operation types', () => {
    spyOn(component, 'fetchOperationType');

    component.filters = {
      operationTypeId: '1',
      name: 'Surgery',
      requiredStaffBySpecialization: 'Anesthesiologist',
      duration: '120',
    };

    component.clearFilters();

    expect(component.filters).toEqual({
      operationTypeId: '',
      name: '',
      requiredStaffBySpecialization: '',
      duration: '',
    });
    expect(component.fetchOperationType).not.toHaveBeenCalled();
  });

  it('should set isLoading to true while fetching operation types', () => {
    spyOn(mockViewModel, 'listOperationType').and.callFake(() => {
      expect(component.isLoading).toBeTrue();
      return of([]);
    });

    component.fetchOperationType();

    expect(component.isLoading).toBeFalse();
  });
});
