import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UpdateUserComponent } from './update-user.component';
import { FormsModule } from '@angular/forms';
import { of, throwError } from 'rxjs';
import { UserViewmodel } from '../../../application/viewmodels/user.viewmodel';
import { LoginViewModel } from '../../../application/viewmodels/login-viewmodel';

describe('UpdateUserComponent', () => {
  let component: UpdateUserComponent;
  let fixture: ComponentFixture<UpdateUserComponent>;
  let mockUserViewmodel: jasmine.SpyObj<UserViewmodel>;

  beforeEach(async () => {
    mockUserViewmodel = jasmine.createSpyObj('UserViewmodel', ['updateUser']);

    await TestBed.configureTestingModule({
      declarations: [UpdateUserComponent],
      imports: [FormsModule],
      providers: [
        { provide: UserViewmodel, useValue: mockUserViewmodel },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(UpdateUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set isEditing to true and clear message when a valid email is submitted', () => {
    component.email = 'user@example.com';
    component.submitEmail();
    expect(component.isEditing).toBeTrue();
  });

  it('should display an error message if email is empty during submission', () => {
    component.email = '';
    component.submitEmail();
    expect(component.message).toBe('Please enter a valid email.');
  });

  it('should update the user and display success message when updateUser is successful', () => {
    const mockUser = { username: 'testuser', email: 'user@example.com', role: 'Admin' };
    mockUserViewmodel.updateUser.and.returnValue(of(mockUser));
    component.email = 'user@example.com';
    component.user = { ...mockUser };

    component.updateUser();

    expect(mockUserViewmodel.updateUser).toHaveBeenCalledWith('user@example.com', mockUser);
    expect(component.message).toBe('User profile updated successfully!');
    expect(component.user).toEqual({ username: '', email: '', role: 'Admin' });
    expect(component.email).toBe('');
  });

  it('should display an error message when updateUser fails', () => {
    const mockError = { error: { message: 'Update failed' } };
    mockUserViewmodel.updateUser.and.returnValue(throwError(mockError));
    component.email = 'user@example.com';
    component.user = { username: 'testuser', email: 'user@example.com', role: 'Admin' };

    component.updateUser();

    expect(component.message).toBe('Failed to update user profile. Update failed');
  });

  it('should display an error message if fields are incomplete when updating user', () => {
    component.user = { username: '', email: '', role: '' };
    component.updateUser();
    expect(component.message).toBe('Please fill in all fields.');
  });

  it('should reset form when resetForm is called', () => {
    component.email = 'user@example.com';
    component.user = { username: 'testuser', email: 'user@example.com', role: 'Admin' };

    component.resetForm();

    expect(component.email).toBe('');
    expect(component.user).toEqual({ username: '', email: '', role: 'Admin' });
  });

  it('should set isEditing to false and clear message when goBack is called', () => {
    component.isEditing = true;
    component.message = 'Some message';

    component.goBack();

    expect(component.isEditing).toBeFalse();
    expect(component.message).toBe('');
  });
});
