import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { AuthService } from '../../../domain/services/auth.service';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-welcome',
  templateUrl: './welcome.component.html',
  styleUrls: ['./welcome.component.css'], // corrected to 'styleUrls'
  encapsulation: ViewEncapsulation.None,
  standalone: true,
  imports: [CommonModule] // Required for *ngIf, *ngFor if this is truly standalone
})
export class WelcomeComponent implements OnInit {
  user: any;

  constructor(private authService: AuthService) {}

  ngOnInit(): void {
    this.user = this.authService.currentUserSubject.value;
    console.log('WelcomeComponent -> user role:', this.user?.role);
  }
}
