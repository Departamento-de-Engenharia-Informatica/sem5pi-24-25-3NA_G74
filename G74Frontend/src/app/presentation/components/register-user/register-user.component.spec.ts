import {ComponentFixture, TestBed} from '@angular/core/testing';
import {RegisterUserComponent} from './register-user.component';
import {HttpClientTestingModule} from '@angular/common/http/testing';
import {UserViewmodel} from '../../../application/viewmodels/user.viewmodel';
import {of} from 'rxjs';
import {response} from 'express';

describe('RegisterUserComponent', () => {
  let component: RegisterUserComponent;
  let fixture: ComponentFixture<RegisterUserComponent>;
  let userViewModelMock: jasmine.SpyObj<UserViewmodel>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [RegisterUserComponent],
      imports: [HttpClientTestingModule],
      providers: [UserViewmodel]
    });
    fixture = TestBed.createComponent(RegisterUserComponent);
    component = fixture.componentInstance;
    userViewModelMock = jasmine.createSpyObj('UserViewModel', ['registerUser']);
  })

  it('Should create', () => {
    expect(component).toBeDefined();
  });

  it('Should contain reset form', () => {
    component.user = {
      username: 'test',
      email: 'test@gmail.com',
      role: 'patient'
    };

    component.resetForm();

    expect(component.user.username).toBe('');
    expect(component.user.email).toBe('');
    expect(component.user.role).toBe('');
  })

  it('Should upgrade user successfully', () => {
    const mockUser = {
      username: 'test',
      email: 'test@gmail.com',
      role: 'patient'
    };

    userViewModelMock.registerUser.and.returnValue(of(mockUser));

    component.onSubmit();

    expect(userViewModelMock.registerUser).toHaveBeenCalledWith(mockUser);
    expect(component.message).toBe('User profile created successfully!');
    expect(component.user.email).toBe('');

  })
});
