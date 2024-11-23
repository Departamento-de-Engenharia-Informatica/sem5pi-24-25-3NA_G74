import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientCreateComponent } from './patient-create.component';
import { FormsModule } from '@angular/forms';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { MockPatientViewModel } from '../mock-patient-viewmodel.service';
import { By } from '@angular/platform-browser';
import { of, throwError } from 'rxjs';

describe('PatientCreateComponent', () => {
  let component: PatientCreateComponent;
  let fixture: ComponentFixture<PatientCreateComponent>;
  let mockViewModel: MockPatientViewModel;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [PatientCreateComponent],
      imports: [FormsModule], // Import FormsModule to enable ngForm functionality
      providers: [{ provide: PatientViewModel, useClass: MockPatientViewModel }],
    }).compileComponents();

    fixture = TestBed.createComponent(PatientCreateComponent);
    component = fixture.componentInstance;
    mockViewModel = TestBed.inject(PatientViewModel) as unknown as MockPatientViewModel;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call createPatientProfile on submit with correct data', () => {
    const spy = spyOn(mockViewModel, 'createPatientProfile').and.returnValue(of({
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
      contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
      emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' }
    }));

    // Simulate form data
    component.patient.name = 'John Doe';
    component.dateOfBirthInput = '1980-05-15';
    component.patient.gender = 'Male';
    component.patient.contactInformation.phoneNumber = '123456789';
    component.patient.contactInformation.emailAddress = 'john.doe@example.com';
    component.patient.emergencyContact.name = 'Jane Doe';
    component.patient.emergencyContact.phoneNumber = '987654321';

    // Trigger submit
    component.onSubmit();

    // Assert that the service was called
    expect(spy).toHaveBeenCalledWith(jasmine.objectContaining({
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
    }));
  });

  it('should reset the form after a successful submit', () => {
    spyOn(mockViewModel, 'createPatientProfile').and.returnValue(of({
      name: '',
      gender: '',
      dateOfBirth: { yearOfBirth: 0, monthOfBirth: 0, dayOfBirth: 0 },
      contactInformation: { phoneNumber: '', emailAddress: '' },
      emergencyContact: { name: '', phoneNumber: '' }
    }));

    // Fill in form data
    component.patient.name = 'John Doe';
    component.dateOfBirthInput = '1980-05-15';
    component.patient.gender = 'Male';
    component.patient.contactInformation.phoneNumber = '123456789';
    component.patient.contactInformation.emailAddress = 'john.doe@example.com';
    component.patient.emergencyContact.name = 'Jane Doe';
    component.patient.emergencyContact.phoneNumber = '987654321';

    // Submit the form
    component.onSubmit();

    // Verify form reset
    expect(component.patient.name).toBe('');
    expect(component.dateOfBirthInput).toBe('');
    expect(component.patient.gender).toBe('');
    expect(component.patient.contactInformation.phoneNumber).toBe('');
    expect(component.patient.contactInformation.emailAddress).toBe('');
    expect(component.patient.emergencyContact.name).toBe('');
    expect(component.patient.emergencyContact.phoneNumber).toBe('');
  });

  it('should display an error message when submit fails', () => {
    spyOn(mockViewModel, 'createPatientProfile').and.returnValue(
      throwError({ error: { message: 'Test Error' } })
    );

    // Trigger submit with valid data
    component.onSubmit();

    // Verify error message is displayed
    expect(component.message).toBe('Failed to create patient profile. Test Error');
  });

  
});
