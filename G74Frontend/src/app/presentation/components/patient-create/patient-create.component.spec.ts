import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientCreateComponent } from './patient-create.component';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';

describe('PatientCreateComponent', () => {
  let component: PatientCreateComponent;
  let fixture: ComponentFixture<PatientCreateComponent>;
  let mockPatientViewModel: jasmine.SpyObj<PatientViewModel>;

  beforeEach(async () => {
    // Create a spy for the PatientViewModel
    mockPatientViewModel = jasmine.createSpyObj('PatientViewModel', ['createPatientProfile']);

    await TestBed.configureTestingModule({
      declarations: [PatientCreateComponent],
      imports: [FormsModule], // Import FormsModule for two-way binding
      providers: [
        { provide: PatientViewModel, useValue: mockPatientViewModel }, // Mock PatientViewModel
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PatientCreateComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should reset the form when resetForm() is called', () => {
    // Modify the component's patient object
    component.patient.name = 'John Doe';
    component.dateOfBirthInput = '1980-05-15';

    component.resetForm();

    // Verify that the form is reset
    expect(component.patient.name).toBe('');
    expect(component.dateOfBirthInput).toBe('');
  });

  it('should call createPatientProfile on form submission and show success message', () => {
    // Arrange
    const mockPatient = {
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: {
        yearOfBirth: 1980,
        monthOfBirth: 5,
        dayOfBirth: 15,
      },
      contactInformation: {
        phoneNumber: '123456789',
        emailAddress: 'john.doe@example.com',
      },
      emergencyContact: {
        name: 'Jane Doe',
        phoneNumber: '987654321',
      },
    };

    // Simulate a successful response from the ViewModel
    mockPatientViewModel.createPatientProfile.and.returnValue(of(mockPatient));

    // Set the component's form data
    component.patient = { ...mockPatient };
    component.dateOfBirthInput = '1980-05-15';

    // Act
    component.onSubmit();

    // Assert
    expect(mockPatientViewModel.createPatientProfile).toHaveBeenCalledWith(mockPatient);
    expect(component.message).toBe('Patient profile created successfully!');
    expect(component.patient.name).toBe(''); // Ensure form reset
  });

  it('should display an error message if createPatientProfile fails', () => {
    // Arrange
    const mockError = {
      status: 500,
      message: 'Internal Server Error',
      error: { message: 'Database connection failed' },
    };

    mockPatientViewModel.createPatientProfile.and.returnValue(throwError(mockError));

    // Set the component's form data
    component.patient = {
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: {
        yearOfBirth: 1980,
        monthOfBirth: 5,
        dayOfBirth: 15,
      },
      contactInformation: {
        phoneNumber: '123456789',
        emailAddress: 'john.doe@example.com',
      },
      emergencyContact: {
        name: 'Jane Doe',
        phoneNumber: '987654321',
      },
    };
    component.dateOfBirthInput = '1980-05-15';

    // Act
    component.onSubmit();

    // Assert
    expect(mockPatientViewModel.createPatientProfile).toHaveBeenCalled();
    expect(component.message).toBe('Failed to create patient profile. Database connection failed');
  });
});
