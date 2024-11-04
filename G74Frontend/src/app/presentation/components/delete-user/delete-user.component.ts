import { Component } from '@angular/core';
import {UserService} from '../../../application/services/user.service';
import {catchError, of} from 'rxjs';

@Component({
  selector: 'app-delete-user',
  templateUrl: './delete-user.component.html',
  styleUrl: './delete-user.component.css'
})
export class DeleteUserComponent {
  message: string = '';

  constructor(private userService: UserService) {
  }

  onSubmit(): void {

    this.userService.markUserAsDeleted().pipe(
      catchError(error => {
        console.error('Error deleting user profile: ', error);
        this.message = `Failed to delete user profile. ${error?.error?.message || 'Please try again.'}`
        return of(null)
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'User profile deleted successfully';
      }
    });
  }

}
