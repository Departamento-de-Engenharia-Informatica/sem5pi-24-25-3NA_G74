import { Component } from '@angular/core';
import {User} from '../../../domain/models/user.model';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {UserService} from '../../../application/services/user.service';

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

  constructor(private userService: UserService) { }

  onSubmit(): void {

    this.userService.registerUser(this.user).pipe(
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
        this.message = 'Patient profile created successfully!';
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
