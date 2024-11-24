import { Component } from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {User} from '../../../domain/models/user.model';
import {UserViewmodel} from '../../../application/viewmodels/user.viewmodel';

@Component({
  selector: 'app-update-user',
  templateUrl: './update-user.component.html',
  styleUrl: './update-user.component.css'
})
export class UpdateUserComponent {
  isEditing: boolean = false;
  email: string = '';
  message: string = '';
  username: string = '';
  newEmail: string = '';
  user: Partial<User> = {};

  constructor(private userViewmodel: UserViewmodel) {
  }

  submitEmail() {
    if (this.email) {
      this.isEditing = true;
      this.message = '';
    } else {
      this.message = 'Please enter a valid email.';
    }
  }

  updateUser() {
    if (this.user.username && this.user.email && this.user.role) {
      this.userViewmodel.updateUser(this.email,this.user).pipe(
        catchError(error => {
          console.error('Error updating user profile:', error);
          this.message = `Failed to update user profile. ${error?.error?.message || 'Please try again.'}`;
          return of(null);
        })
      ).subscribe(response => {
        if (response) {
          this.message = 'User profile updated successfully!';
          this.resetForm();
        }
      });
    } else {
      this.message = 'Please fill in all fields.';
    }
  }

  goBack() {
    this.isEditing = false;
    this.message = '';
  }

  resetForm() {
    this.user = {
      username: '',
      email: '',
      role: 'Admin'
    };
    this.email = '';
  }

}
