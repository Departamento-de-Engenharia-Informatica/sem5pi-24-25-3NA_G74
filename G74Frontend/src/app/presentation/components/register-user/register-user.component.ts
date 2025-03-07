import { Component } from '@angular/core';
import {User} from '../../../domain/models/user.model';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {UserViewmodel} from '../../../application/viewmodels/user.viewmodel';

@Component({
  selector: 'app-register-user',
  templateUrl: './register-user.component.html',
  styleUrl: './register-user.component.css'
})
export class RegisterUserComponent {
  user: User = {
    username: '',
    email: '',
    role: ''
  };
  message: string = "";

  constructor(private userViewModel: UserViewmodel) { }

  onSubmit(): void {

    this.userViewModel.registerUser(this.user).pipe(
      catchError(error => {
        console.error('Error creating new user:', error);
        this.message = `Failed to create new user. ${error?.error?.message || 'Please try again.'}`;
        console.error('Error status:', error.status);
        console.error('Error message:', error.message);
        console.error('Error details:', error.error);
        return of(null);
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'User profile created successfully!';
        this.resetForm();
      }
    });
  }

  resetForm(): void {
    this.user = {
      username: '',
      email: '',
      role: ''
    };
  }
}
