import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientListComponent } from './patient-list.component';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { MockPatientViewModel } from '../mock-patient-viewmodel.service';
import { FormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';

describe('PatientListComponent', () => {
  let component: PatientListComponent;
  let fixture: ComponentFixture<PatientListComponent>;
  let mockViewModel: MockPatientViewModel;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [PatientListComponent],
      imports: [FormsModule], // To handle forms in the component
      providers: [{ provide: PatientViewModel, useClass: MockPatientViewModel }],
    }).compileComponents();

    fixture = TestBed.createComponent(PatientListComponent);
    component = fixture.componentInstance;
    mockViewModel = TestBed.inject(PatientViewModel) as unknown as MockPatientViewModel;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch patients on initialization', () => {
    spyOn(mockViewModel, 'listPatients').and.callThrough();
  
    component.ngOnInit();
  
    expect(mockViewModel.listPatients).toHaveBeenCalledWith(
      jasmine.objectContaining({
        contactInformation: jasmine.objectContaining({
          phoneNumber: '',
          emailAddress: '',
        }),
      })
    );
    expect(component.patients.length).toBeGreaterThan(0); // Ensure patients are fetched
  });

  it('should apply filters when fetching patients', () => {
    spyOn(mockViewModel, 'listPatients').and.callThrough();

    component.phoneNumberFilter = '123';
    component.emailAddressFilter = 'john.doe@example.com';
    component.dateOfBirthInput = '1980-05-15';

    component.fetchPatients();

    expect(mockViewModel.listPatients).toHaveBeenCalledWith(
      jasmine.objectContaining({
        contactInformation: jasmine.objectContaining({
          phoneNumber: '123',
          emailAddress: 'john.doe@example.com',
        }),
        dateOfBirth: jasmine.objectContaining({
          yearOfBirth: 1980,
          monthOfBirth: 5,
          dayOfBirth: 15,
        }),
      })
    );
  });

  it('should open update popup with selected patient', () => {
    const mockPatient = mockViewModel.getMockPatients()[0];

    component.openUpdatePopup(mockPatient);

    expect(component.selectedPatientForUpdate).toEqual(mockPatient);
  });

  it('should close update popup and refresh patient list if refresh is true', () => {
    spyOn(component, 'fetchPatients');

    component.closeUpdatePopup(true);

    expect(component.selectedPatientForUpdate).toBeNull();
    expect(component.fetchPatients).toHaveBeenCalled();
  });

  it('should not refresh patient list if refresh is false when closing update popup', () => {
    spyOn(component, 'fetchPatients');

    component.closeUpdatePopup(false);

    expect(component.selectedPatientForUpdate).toBeNull();
    expect(component.fetchPatients).not.toHaveBeenCalled();
  });

  it('should open delete popup with selected patient', () => {
    const mockPatient = mockViewModel.getMockPatients()[0];

    component.openDeletePopup(mockPatient);

    expect(component.selectedPatientForDelete).toEqual(mockPatient);
  });

  it('should close delete popup and refresh patient list if refresh is true', () => {
    spyOn(component, 'fetchPatients');

    component.closeDeletePopup(true);

    expect(component.selectedPatientForDelete).toBeNull();
    expect(component.fetchPatients).toHaveBeenCalled();
  });

  it('should not refresh patient list if refresh is false when closing delete popup', () => {
    spyOn(component, 'fetchPatients');

    component.closeDeletePopup(false);

    expect(component.selectedPatientForDelete).toBeNull();
    expect(component.fetchPatients).not.toHaveBeenCalled();
  });

  it('should clear filters and fetch patients', () => {
    spyOn(component, 'fetchPatients');

    component.phoneNumberFilter = '123';
    component.emailAddressFilter = 'john.doe@example.com';
    component.dateOfBirthInput = '1980-05-15';

    component.clearFilters();

    expect(component.phoneNumberFilter).toBe('');
    expect(component.emailAddressFilter).toBe('');
    expect(component.dateOfBirthInput).toBe('');
    expect(component.fetchPatients).toHaveBeenCalled();
  });

  it('should show a message when no patients are found', () => {
    spyOn(mockViewModel, 'listPatients').and.returnValue(of([]));

    component.fetchPatients();

    expect(component.message).toBe('No patients found.');
    expect(component.patients.length).toBe(0);
  });
});
