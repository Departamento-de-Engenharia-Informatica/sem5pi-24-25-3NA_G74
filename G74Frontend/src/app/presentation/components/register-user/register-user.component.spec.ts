import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RegisterUserComponent } from './register-user.component';
import { FormsModule } from '@angular/forms'; // Import FormsModule
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { UserViewmodel } from '../../../application/viewmodels/user.viewmodel';
import { of, throwError } from 'rxjs';

describe('RegisterUserComponent', () => {
  let component: RegisterUserComponent;
  let fixture: ComponentFixture<RegisterUserComponent>;
  let userViewModelMock: jasmine.SpyObj<UserViewmodel>;

  beforeEach(async () => {
    userViewModelMock = jasmine.createSpyObj('UserViewmodel', ['registerUser']);

    await TestBed.configureTestingModule({
      declarations: [RegisterUserComponent],
      imports: [FormsModule, HttpClientTestingModule], // Add FormsModule
      providers: [{ provide: UserViewmodel, useValue: userViewModelMock }],
    }).compileComponents();

    fixture = TestBed.createComponent(RegisterUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should reset the form', () => {
    component.user = {
      username: 'test',
      email: 'test@gmail.com',
      role: 'patient',
    };

    component.resetForm();

    expect(component.user.username).toBe('');
    expect(component.user.email).toBe('');
    expect(component.user.role).toBe('');
  });

  it('should upgrade user successfully', () => {
    const mockUser = {
      username: 'test',
      email: 'test@gmail.com',
      role: 'patient',
    };

    userViewModelMock.registerUser.and.returnValue(of(mockUser));
    component.user = { ...mockUser };
    component.onSubmit();
    expect(userViewModelMock.registerUser).toHaveBeenCalledWith(mockUser);
    expect(component.message).toBe('User profile created successfully!');
    expect(component.user.username).toBe('');
    expect(component.user.email).toBe('');
    expect(component.user.role).toBe('');
  });

  it('should display an error message if user registration fails', () => {
    const mockError = { error: { message: 'Registration failed' } };
    userViewModelMock.registerUser.and.returnValue(throwError(mockError));
    component.user = {
      username: 'test',
      email: 'test@gmail.com',
      role: 'patient',
    };
    component.onSubmit();
    expect(component.message).toBe('Failed to create new user. Registration failed');
  });
});
