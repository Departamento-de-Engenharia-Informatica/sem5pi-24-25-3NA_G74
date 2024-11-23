import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientUpdateComponent } from './patient-update.component';
import { FormsModule } from '@angular/forms';
import { MockPatientViewModel } from '../mock-patient-viewmodel.service';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { By } from '@angular/platform-browser';
import { of, throwError } from 'rxjs';
import { Patient } from '../../../domain/models/patient.model';

describe('PatientUpdateComponent', () => {
  let component: PatientUpdateComponent;
  let fixture: ComponentFixture<PatientUpdateComponent>;
  let mockViewModel: MockPatientViewModel;

  const mockPatient: Patient = {
    name: 'John Doe',
    gender: 'Male',
    dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
    contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
    emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [PatientUpdateComponent],
      imports: [FormsModule], // To support template-driven forms
      providers: [{ provide: PatientViewModel, useClass: MockPatientViewModel }],
    }).compileComponents();

    fixture = TestBed.createComponent(PatientUpdateComponent);
    component = fixture.componentInstance;
    mockViewModel = TestBed.inject(PatientViewModel) as unknown as MockPatientViewModel;

    component.patient = { ...mockPatient }; // Pass input data
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize the date input field on ngOnInit', () => {
    component.ngOnInit();
    expect(component.dateInput).toBe('1980-05-15');
  });

  it('should call getMedicalRecordNumber and updatePatientProfile on updatePatient', () => {
    const medicalRecordNumber = 'mock-medical-record-number';

    const getMedicalRecordNumberSpy = spyOn(mockViewModel, 'getMedicalRecordNumber').and.returnValue(of(medicalRecordNumber));
    const updatePatientProfileSpy = spyOn(mockViewModel, 'updatePatientProfile').and.returnValue(of({ ...mockPatient }));

    // Simulate user changing date input
    component.dateInput = '1985-12-25';

    component.updatePatient();

    // Expect getMedicalRecordNumber to be called with the patient's email
    expect(getMedicalRecordNumberSpy).toHaveBeenCalledWith('john.doe@example.com');

    // Expect updatePatientProfile to be called with the updated patient
    expect(updatePatientProfileSpy).toHaveBeenCalledWith(
      jasmine.objectContaining({
        name: 'John Doe',
        dateOfBirth: { yearOfBirth: 1985, monthOfBirth: 12, dayOfBirth: 25 },
      }),
      medicalRecordNumber
    );
    expect(component.message).toBe('Patient john.doe@example.com updated successfully!');
  });

  it('should display an error message when getMedicalRecordNumber fails', () => {
    spyOn(mockViewModel, 'getMedicalRecordNumber').and.returnValue(
      throwError({ error: { message: 'Error fetching medical record number' } })
    );

    component.updatePatient();

    expect(component.message).toBe('Failed to fetch medical record number for john.doe@example.com.');
    expect(component.isLoading).toBeFalse();
  });

  it('should display an error message when updatePatientProfile fails', () => {
    spyOn(mockViewModel, 'getMedicalRecordNumber').and.returnValue(of('mock-medical-record-number'));
    spyOn(mockViewModel, 'updatePatientProfile').and.returnValue(
      throwError({ error: { message: 'Error updating patient profile' } })
    );

    component.updatePatient();

    expect(component.message).toBe('Failed to update patient john.doe@example.com.');
    expect(component.isLoading).toBeFalse();
  });

  it('should close the popup on cancelUpdate', () => {
    const closeSpy = spyOn(component.close, 'emit');
    component.cancelUpdate();
    expect(closeSpy).toHaveBeenCalledWith(false);
  });

  it('should emit false when clicking outside the popup', () => {
    const closeSpy = spyOn(component.close, 'emit');
    const mockEvent = { target: { classList: { contains: () => true } } } as unknown as Event;

    component.onOutsideClick(mockEvent.target as HTMLElement);

    expect(closeSpy).toHaveBeenCalledWith(false);
  });

  it('should not emit close if the click is not outside the popup', () => {
    const closeSpy = spyOn(component.close, 'emit');
    const mockEvent = { target: { classList: { contains: () => false } } } as unknown as Event;

    component.onOutsideClick(mockEvent.target as HTMLElement);

    expect(closeSpy).not.toHaveBeenCalled();
  });
});
